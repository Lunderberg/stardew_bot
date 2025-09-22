use std::borrow::Borrow;
use std::cell::OnceCell;
use std::cmp::Reverse;
use std::collections::HashSet;
use std::ops::{Deref, Range};

use elsa::FrozenMap;

use dll_unpacker::{
    Field, MetadataCodedIndex, MetadataRow, MetadataTableIndex,
    MetadataTypeDefOrRef, SignaturePrimType, SignatureType, TypeDef,
    TypeDefOrRef, TypeRef,
};
use iterator_extensions::ResultIteratorExt as _;
use itertools::{Either, Itertools};
use memory_reader::{MemoryMapRegion, MemoryReader, Pointer, TypedPointer};

use crate::{
    extensions::*,
    runtime_type::{DotNetType, RuntimePrimType},
    CorElementType, Error, FieldContainer, FieldDescription, FieldDescriptions,
    MethodTable, RuntimeModule, RuntimeModuleLayout, RuntimeObject,
    RuntimeType, RuntimeValue, SignatureTypeExt as _, SymbolicType, TypeHandle,
    TypeHandlePtrExt as _,
};

type CachedTypeDef = (TypedPointer<RuntimeModule>, MetadataTableIndex<TypeDef>);

type FieldLookupKey = (
    TypedPointer<MethodTable>, // Parent's method table
    TypedPointer<MethodTable>, // Field's method table
    MetadataTableIndex<Field>,
);

/// Cache containing known state of the remote process.  Only values
/// that are stable across the lifetime of the remote process should
/// be stored here.  For example, the location of a static field may
/// be stored, but the value contained within that static field may
/// not.
#[derive(Default)]
pub struct StaticValueCache {
    type_handles: FrozenMap<TypedPointer<TypeHandle>, Box<TypeHandle>>,

    runtime_modules: FrozenMap<TypedPointer<RuntimeModule>, Box<RuntimeModule>>,
    runtime_module_vtable: OnceCell<Pointer>,
    runtime_module_layout: OnceCell<RuntimeModuleLayout>,

    runtime_type: FrozenMap<TypedPointer<MethodTable>, Box<RuntimeType>>,

    names_from_metadata: OnceCell<HashSet<String>>,

    field_descriptions:
        FrozenMap<TypedPointer<MethodTable>, Box<Option<FieldDescriptions>>>,
    runtime_module_by_name: FrozenMap<String, Box<TypedPointer<RuntimeModule>>>,
    method_table_by_metadata:
        FrozenMap<TypeInModule, Box<TypedPointer<MethodTable>>>,
    method_table_by_name: FrozenMap<String, Box<TypedPointer<MethodTable>>>,

    module_defining_type: FrozenMap<TypeInModule, Box<CachedTypeDef>>,

    field_to_runtime_type: FrozenMap<FieldLookupKey, Box<RuntimeType>>,
    field_to_type_name: FrozenMap<
        (TypedPointer<MethodTable>, MetadataTableIndex<Field>),
        Box<String>,
    >,
    field_to_name: FrozenMap<
        (TypedPointer<MethodTable>, MetadataTableIndex<Field>),
        Box<String>,
    >,

    method_table_to_name: FrozenMap<TypedPointer<MethodTable>, Box<String>>,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
struct TypeInModule {
    module: TypedPointer<RuntimeModule>,
    coded_index: MetadataCodedIndex<TypeDefOrRef>,
}

#[derive(Clone, Copy)]
pub struct CachedReader<'a> {
    state: &'a StaticValueCache,
    reader: &'a MemoryReader,
}

impl StaticValueCache {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn cached_reader<'a>(
        &'a self,
        reader: &'a MemoryReader,
    ) -> CachedReader<'a> {
        CachedReader {
            state: self,
            reader,
        }
    }
}

impl<'a> CachedReader<'a> {
    pub fn underlying_reader(&self) -> &'a MemoryReader {
        self.reader
    }

    pub fn object(
        &self,
        location: TypedPointer<RuntimeObject>,
    ) -> Result<RuntimeObject, Error> {
        location.read(self.reader)
    }

    pub fn value(
        &self,
        runtime_type: RuntimeType,
        location: Range<Pointer>,
    ) -> Result<RuntimeValue, Error> {
        let bytes = self.reader.read_bytes(location)?;
        let value = runtime_type.parse(&bytes)?;
        Ok(value)
    }

    pub fn method_table(
        &self,
        ptr: TypedPointer<MethodTable>,
    ) -> Result<&MethodTable, Error> {
        let ptr: Pointer = ptr.into();

        // All method tables exist at locations with pointer
        // alignment.  However, many locations in the CLR hold either
        // method tables or type descriptions, using the low bits of
        // the pointer to determine which it is.
        //
        // To avoid having method tables be duplicated between a
        // lookup table for MethodTable and a lookup table for
        // TypeHandle, this function unpacks a TypeHandle, then
        // unwraps to a MethodTable.
        if ptr.as_usize() % Pointer::SIZE != 0 {
            return Err(Error::MisalignedMethodTable(ptr));
        }

        let type_handle = self.type_handle(ptr.into())?;
        match type_handle {
            TypeHandle::MethodTable(method_table) => Ok(method_table),
            TypeHandle::TypeDescription(_) => panic!(
                "Unreachable, would be caught by earlier alignment check."
            ),
        }
    }

    pub fn type_handle(
        &self,
        ptr: TypedPointer<TypeHandle>,
    ) -> Result<&TypeHandle, Error> {
        self.state
            .type_handles
            .try_insert(ptr, || ptr.read(self.reader))
    }

    pub fn runtime_type(
        &self,
        ptr: TypedPointer<MethodTable>,
    ) -> Result<RuntimeType, Error> {
        self.state
            .runtime_type
            .try_insert(ptr, || {
                let method_table = self.method_table(ptr)?;
                let runtime_type = method_table.runtime_type(self)?;
                //runtime_type.validate()?;
                runtime_type.validate().unwrap();
                Ok(runtime_type)
            })
            .cloned()
    }

    pub fn runtime_module(
        &self,
        ptr: TypedPointer<RuntimeModule>,
    ) -> Result<&'a RuntimeModule, Error> {
        self.state.runtime_modules.try_insert(ptr, || {
            let runtime_module = RuntimeModule::new(
                ptr.into(),
                self.state.runtime_module_layout.get().cloned(),
            );

            // If not already known, save the vtable pointer for later
            // use.
            self.state
                .runtime_module_vtable
                .or_try_init(|| runtime_module.vtable_location(self))?;

            // Save the module into the by-name lookup.
            let name = runtime_module.name(self)?;
            self.state
                .runtime_module_by_name
                .insert(name.to_string(), Box::new(ptr));

            Ok(runtime_module)
        })
    }

    fn iter_clr_dll_regions(&self) -> impl Iterator<Item = &MemoryMapRegion> {
        self.reader
            .iter_regions()
            .filter(|region| region.file_offset() == 0)
            .filter(|region| region.size_bytes() > 4096)
            .filter(|region| {
                region
                    .name()
                    .map(|name| name.ends_with(".dll"))
                    .unwrap_or(false)
            })
    }

    pub fn init_dlls(&self) -> Result<(), Error> {
        let dll_data = self
            .iter_clr_dll_regions()
            .map(|region| region.read(self))
            .collect::<Result<Vec<_>, _>>()?;

        let layouts = dll_data
            .iter()
            .map(dll_unpacker::unpack_metadata_layout)
            .collect::<Result<Vec<_>, _>>()?;

        let metadata: Vec<_> = layouts
            .iter()
            .zip(dll_data.iter())
            .map(|(layout, data)| layout.metadata(data))
            .collect();

        let module_vtable = self
            .state
            .runtime_module_vtable
            .or_try_init(|| -> Result<_, Error> {
                // This initial pass based on the MethodDef pointers
                // is slower and less reliable than the later pass.
                // However, the later pass requires knowing the
                // location of the C++ vtable used by the module
                // object, and that requires first locating at least
                // one RuntimeModule.
                let module_pointers =
                    RuntimeModule::locate_by_metadata(&metadata, self);

                let module_vtable = module_pointers
                    .iter()
                    .filter_map(|opt| opt.as_ref())
                    .map(|&ptr| RuntimeModule::new(ptr.into(), None))
                    .find_map(|runtime_module| {
                        runtime_module.vtable_location(self).ok()
                    })
                    .ok_or(Error::NoModulePointerFoundFromMethodDef)?;

                Ok(module_vtable)
            })
            .copied()?;

        // This is the faster and more reliable way to locate each
        // RuntimeModule.  But it can only be used once the location
        // of the vtable has been determined.
        let module_pointers =
            RuntimeModule::locate_by_vtable(&metadata, module_vtable, self)?;

        // If not already known, infer the layout for every
        // runtime module, to be re-used later.
        self.state.runtime_module_layout.or_try_init(|| {
            let modules = module_pointers
                .iter()
                .filter_map(|opt| opt.as_ref())
                .map(|&ptr| RuntimeModule::new(ptr.into(), None))
                .collect::<Vec<_>>();
            RuntimeModule::infer_layout(&modules, self.reader)
        })?;

        // Read each module, populating the cache for later use.
        module_pointers.into_iter().flatten().try_for_each(
            |ptr| -> Result<_, Error> {
                self.runtime_module(ptr)?;
                Ok(())
            },
        )?;

        Ok(())
    }

    fn init_method_table_by_name(&self) -> Result<(), Error> {
        self.iter_known_modules()?
            .map(Ok)
            .and_map_ok(|module_ptr| self.runtime_module(module_ptr))
            .flat_map_ok(|module| {
                module.iter_method_table_pointers(self.reader)
            })
            .and_map_ok(|method_table_ptr| self.method_table(method_table_ptr))
            .filter_ok(|method_table| method_table.token().is_some())
            .try_for_each(|res_method_table| -> Result<_, Error> {
                let method_table = res_method_table?;
                let module = self.runtime_module(method_table.module())?;
                let metadata = module.metadata(self)?;
                let row = metadata.get(method_table.token().unwrap())?;
                let namespace = row.namespace()?;
                let name = row.name()?;
                let lookup_key = if namespace.is_empty() {
                    name.to_string()
                } else {
                    format!("{namespace}.{name}")
                };
                self.state
                    .method_table_by_name
                    .insert(lookup_key, Box::new(method_table.ptr()));

                Ok(())
            })?;
        Ok(())
    }

    pub fn runtime_module_by_name(
        &self,
        name: &str,
    ) -> Result<TypedPointer<RuntimeModule>, Error> {
        let name = name.strip_suffix(".dll").unwrap_or(name);

        if let Some(value) = self.state.runtime_module_by_name.get(name) {
            Ok(*value)
        } else {
            self.init_dlls()?;
            self.state
                .runtime_module_by_name
                .get(name)
                .ok_or_else(|| {
                    // panic!(
                    //     "Could not find {name}.\n\
                    //      Known DLLs:\n\t{}",
                    //     self.state
                    //         .runtime_module_by_name
                    //         .clone()
                    //         .into_tuple_vec()
                    //         .iter()
                    //         .map(|(name, _)| name)
                    //         .format("\n\t")
                    // );
                    Error::RegionForDLLNotFoundFromName(name.to_string())
                })
                .copied()
        }
    }

    pub fn find_field_by_name(
        &self,
        method_table_ptr: TypedPointer<MethodTable>,
        field_name: &str,
    ) -> Result<(TypedPointer<MethodTable>, FieldDescription<'a>), Error> {
        self.iter_instance_fields(method_table_ptr)?
            .find(|(_, field)| {
                self.field_to_name(field)
                    .map(|name| name == field_name)
                    .unwrap_or(false)
            })
            .ok_or_else(|| match self.method_table_to_name(method_table_ptr) {
                Ok(class_name) => Error::NoSuchInstanceField {
                    class_name: class_name.to_string(),
                    field_name: field_name.to_string(),
                },
                Err(err) => err,
            })
    }

    pub fn unwrap_type_ref(
        &self,
        row: MetadataTypeDefOrRef<'a>,
    ) -> Result<MetadataTypeDefOrRef<'a>, Error> {
        let type_ref = match &row {
            MetadataTypeDefOrRef::TypeRef(tr) => tr,
            _ => {
                return Ok(row);
            }
        };
        let mut module_name = type_ref.target_dll_name()?;
        let namespace = type_ref.namespace()?;
        let name = type_ref.name()?;

        'main_loop: loop {
            let module_ptr = self.runtime_module_by_name(module_name)?;
            let module = self.runtime_module(module_ptr)?;
            let metadata = module.metadata(self)?;

            for type_def in metadata.type_def_table().iter_rows() {
                if type_def.namespace()? == namespace
                    && type_def.name()? == name
                {
                    return Ok(MetadataTypeDefOrRef::TypeDef(type_def));
                }
            }

            for type_ref in metadata.type_ref_table().iter_rows() {
                if type_ref.namespace()? == namespace
                    && type_ref.name()? == name
                {
                    module_name = type_ref.target_dll_name()?;
                    continue 'main_loop;
                }
            }

            for exported_type in metadata.exported_type_table().iter_rows() {
                if exported_type.namespace()? == namespace
                    && exported_type.name()? == name
                {
                    module_name = exported_type.target_dll_name()?;
                    continue 'main_loop;
                }
            }

            return Err(Error::MissingTypeDefInModule {
                type_def: type_ref.full_name()?,
                module: module_name.to_string(),
            });
        }
    }

    pub fn find_type_def_in_module(
        &self,
        full_name: &str,
        module_name: &str,
    ) -> Result<MetadataRow<'a, TypeDef>, Error> {
        // println!("Looking for {full_name} within {module_name}");

        let module_ptr = self.runtime_module_by_name(module_name)?;
        let module = self.runtime_module(module_ptr)?;
        let metadata = module.metadata(self)?;

        // println!(
        //     "Searching through {} TypeDef entries",
        //     metadata.type_def_table().num_rows()
        // );
        for type_def in metadata.type_def_table().iter_rows() {
            // println!("\tTypeDef '{}'", type_def.full_name()?);
            if type_def.full_name()? == full_name {
                // println!("\tFound '{full_name}', returning");
                return Ok(type_def);
            }
        }

        // println!(
        //     "Searching through {} TypeRef entries",
        //     metadata.type_ref_table().num_rows()
        // );
        for type_ref in metadata.type_ref_table().iter_rows() {
            // println!("\tTypeRef '{}'", type_ref.full_name()?);
            if type_ref.full_name()? == full_name {
                let target_dll_name = type_ref.target_dll_name()?;
                // println!("\tFound '{full_name}', delegating to search in {target_dll_name}");
                assert!(target_dll_name != module_name);
                return self
                    .find_type_def_in_module(full_name, target_dll_name);
            }
        }

        // println!(
        //     "Searching through {} ExportedType entries",
        //     metadata.exported_type_table().num_rows()
        // );
        for exported_type in metadata.exported_type_table().iter_rows() {
            // println!("\tExportedType '{}'", exported_type.full_name()?);
            if exported_type.full_name()? == full_name {
                let target_dll_name = exported_type.target_dll_name()?;
                // println!("\tFound '{full_name}', delegating to search in {target_dll_name}");
                assert!(target_dll_name != module_name);
                return self
                    .find_type_def_in_module(full_name, target_dll_name);
            }
        }

        Err(Error::MissingTypeDefInModule {
            type_def: full_name.to_string(),
            module: module_name.to_string(),
        })
    }

    pub fn find_type_def_anywhere(
        &self,
        full_name: &str,
    ) -> Result<MetadataRow<'a, TypeDef>, Error> {
        assert!(
            !full_name.contains('<'),
            "Treating '{full_name}' as a type def won't find anything, \
             since the generic types will cause a mismatch.",
        );
        let type_def = self
            .iter_known_modules()?
            .map(Ok)
            .and_map_ok(|module_ptr| self.runtime_module(module_ptr))
            .and_map_ok(|module| module.metadata(self))
            .flat_map_ok(|metadata| -> Result<_, Error> {
                Ok(metadata.type_def_table().iter_rows())
            })
            .and_find_ok(|type_def| -> Result<bool, Error> {
                Ok(type_def.full_name()? == full_name)
            })?
            .ok_or_else(|| Error::MissingTypeDef(full_name.to_string()))?;

        Ok(type_def)
    }

    pub fn find_type_def(
        &self,
        full_name: &str,
        module_name: Option<&str>,
    ) -> Result<MetadataRow<'a, TypeDef>, Error> {
        if let Some(module_name) = module_name {
            self.find_type_def_in_module(full_name, module_name)
        } else {
            self.find_type_def_anywhere(full_name)
        }
    }

    pub fn field_type_by_parent_and_name(
        &self,
        parent_mt: Option<TypedPointer<MethodTable>>,
        _symbolic_parent: Option<&SymbolicType>,
        field_name: &str,
    ) -> Result<RuntimeType, Error> {
        // let opt_type_def = symbolic_parent
        //     .map(|ty| self.find_type_def(ty))
        //     .transpose()?;

        if let Some(parent_mt) = parent_mt {
            // The owning object may be a subclass, accessing a field
            // defined within its parent class.  In that case, we need to
            // find the method table of the parent class.
            let (parent_of_field, field_desc) =
                self.find_field_by_name(parent_mt, field_name)?;

            // The runtime type of that field can then be inspected,
            // relative to the class in which it is defined.
            let runtime_type =
                self.field_to_runtime_type(parent_of_field, &field_desc)?;
            Ok(runtime_type)
        } else {
            Ok(RuntimeType::Unknown)
        }
    }

    pub fn field_to_runtime_type(
        &self,
        ptr_mtable_of_parent: TypedPointer<MethodTable>,
        desc: &FieldDescription,
    ) -> Result<RuntimeType, Error> {
        let field_name = self.field_to_name(desc)?;
        assert!(
            ptr_mtable_of_parent.as_usize() % Pointer::SIZE == 0,
            "Attempted to find field '{field_name}' \
             with misaligned parent mtable {ptr_mtable_of_parent}"
        );

        let lookup_key =
            (ptr_mtable_of_parent, desc.method_table(), desc.token());

        if let Some(runtime_type) =
            self.state.field_to_runtime_type.get(&lookup_key)
        {
            return Ok(runtime_type.clone());
        }

        let runtime_type: RuntimeType = 'runtime_type: {
            if let CorElementType::Prim(prim) = desc.cor_element_type()? {
                break 'runtime_type prim.into();
            }

            let parent = self.method_table(ptr_mtable_of_parent)?;
            let module_ptr = parent.module();
            let module = self.runtime_module(module_ptr)?;
            let metadata = module.metadata(self)?;
            let field_metadata = metadata.get(desc.token())?;
            let signature = field_metadata.signature()?;

            let sig_type = signature.field_type()?;

            // if let CorElementType::Prim(prim) = desc.cor_element_type()? {
            //     println!(
            //         "Within {}, field {} has signature {sig_type}, \
            //          and primitive type {prim}",
            //         parent.printable(*self),
            //         field_metadata.name()?,
            //     );
            //     break 'runtime_type prim.into();
            // }

            let parent_generic_types = self
                .method_table(ptr_mtable_of_parent)?
                .generic_types(self)?;

            self.signature_type_to_runtime_type(
                &sig_type,
                &parent_generic_types,
            )?
        };

        if runtime_type.is_complete() {
            self.state
                .field_to_runtime_type
                .insert(lookup_key, Box::new(runtime_type.clone()));
        }

        Ok(runtime_type)
    }

    fn signature_type_matches_type_handle(
        &self,
        sig_arg: &SignatureType<'_>,
        type_handle_ptr: TypedPointer<TypeHandle>,
        parent_generic_types: &[TypedPointer<TypeHandle>],
    ) -> Result<bool, Error> {
        let type_handle = self.type_handle(type_handle_ptr)?;

        let arg_matches = match sig_arg {
            SignatureType::Class {
                index, metadata, ..
            }
            | SignatureType::ValueType {
                index, metadata, ..
            } => match type_handle {
                TypeHandle::MethodTable(arg_method_table) => {
                    let module_ptr =
                        self.runtime_module_by_name(metadata.name()?)?;
                    let (expected_module, expected_token) =
                        self.module_defining_type(module_ptr, *index)?;

                    arg_method_table.module() == expected_module
                        && arg_method_table.token() == Some(expected_token)
                }
                TypeHandle::TypeDescription(_) => false,
            },
            SignatureType::Prim(sig_prim_type) => match type_handle {
                TypeHandle::TypeDescription(type_description) => {
                    match type_description.element_type() {
                        CorElementType::Prim(actual_prim_type) => {
                            Some(actual_prim_type)
                        }
                        _ => None,
                    }
                }
                TypeHandle::MethodTable(method_table) => {
                    if method_table.is_prim_type() {
                        match method_table.runtime_type(self.reader)? {
                            RuntimeType::Prim(actual_prim_type) => {
                                Some(actual_prim_type)
                            }
                            _ => None,
                        }
                    } else {
                        None
                    }
                }
            }
            .map(|prim_type| sig_prim_type == &prim_type)
            .unwrap_or(false),
            SignatureType::String => match type_handle {
                TypeHandle::MethodTable(method_table) => {
                    method_table.is_string()
                }
                _ => false,
            },
            SignatureType::Object => {
                let object_method_table_ptr = self
                    .method_table_by_name("System.Object")?
                    .ok_or(Error::MethodTableOfSystemObjectShouldBeLoaded)?;
                type_handle_ptr.as_method_table()
                    == Some(object_method_table_ptr)
            }
            SignatureType::GenericInst {
                index,
                metadata,
                type_args,
                ..
            } => match type_handle {
                TypeHandle::MethodTable(method_table) => {
                    let module_ptr =
                        self.runtime_module_by_name(metadata.name()?)?;
                    let (_ptr_to_defining_module, type_def_index) =
                        self.module_defining_type(module_ptr, *index)?;

                    if method_table.token() == Some(type_def_index)
                        && method_table.has_generics()
                    {
                        let generics: Vec<_> = method_table
                            .generic_types_excluding_base_class(self)?
                            .collect();
                        generics.len() == type_args.len()
                            && generics
                                .into_iter()
                                .zip(type_args.iter().cloned())
                                .and_all(|(type_handle_ptr, sig_arg)| {
                                    self.signature_type_matches_type_handle(
                                        &sig_arg,
                                        type_handle_ptr,
                                        parent_generic_types,
                                    )
                                })?
                    } else {
                        false
                    }
                }
                TypeHandle::TypeDescription(_) => false,
            },
            SignatureType::GenericVarFromType(sig_index) => {
                let sig_index = *sig_index as usize;
                let expected_type_handle = *parent_generic_types
                    .get(sig_index)
                    .ok_or_else(|| Error::InvalidGenericTypeVar {
                        index: sig_index,
                        num_vars: parent_generic_types.len(),
                    })?;

                type_handle_ptr == expected_type_handle
            }
            SignatureType::SizeArray(sig_element_type) => match type_handle {
                TypeHandle::MethodTable(method_table) => method_table
                    .array_element_type()
                    .map(|element_ptr| -> Result<_, Error> {
                        let element_ptr: Pointer = element_ptr.into();
                        let element_ptr: TypedPointer<TypeHandle> =
                            element_ptr.into();
                        self.signature_type_matches_type_handle(
                            sig_element_type,
                            element_ptr,
                            parent_generic_types,
                        )
                    })
                    .transpose()?
                    .unwrap_or(false),
                TypeHandle::TypeDescription(_) => false,
            },

            other => {
                todo!("Unhandled type: {other}, with signature type {other:?}")
            }
        };

        Ok(arg_matches)
    }

    fn ptr_to_loader_module(
        &self,
        sig_type: &SignatureType<'_>,
    ) -> Result<Option<TypedPointer<RuntimeModule>>, Error> {
        Ok(match sig_type {
            SignatureType::Prim(_)
            | SignatureType::Object
            | SignatureType::String => {
                Some(self.runtime_module_by_name("System.Private.CoreLib")?)
            }

            SignatureType::ValueType {
                index, metadata, ..
            }
            | SignatureType::Class { index, metadata }
            | SignatureType::GenericInst {
                index, metadata, ..
            } => {
                let module_ptr =
                    self.runtime_module_by_name(metadata.name()?)?;
                Some(
                    self.module_defining_type(module_ptr, *index)
                        .map(|(ptr, _)| ptr)?,
                )
            }

            SignatureType::SizeArray(element_type)
            | SignatureType::MultiDimArray { element_type, .. } => {
                self.ptr_to_loader_module(element_type.as_ref())?
            }

            SignatureType::GenericVarFromType(_) => None,
            SignatureType::GenericVarFromMethod(_) => None,

            SignatureType::Void => None,
            SignatureType::Ptr(pointee) => {
                self.ptr_to_loader_module(pointee.as_ref())?
            }
        })
    }

    pub fn symbolic_type_to_method_table(
        &self,
        symbolic: &SymbolicType,
    ) -> Result<Option<TypedPointer<MethodTable>>, Error> {
        let opt_method_table_ptr = match symbolic {
            SymbolicType::Named { name, .. } => {
                self.method_table_by_name(name)?
            }
            SymbolicType::Metadata { module, index } => {
                let module_ptr = self.runtime_module_by_name(module)?;
                self.method_table_by_metadata(module_ptr, (*index).into())?
            }
            SymbolicType::GenericInst { base, args } => {
                let Some(base_ptr) =
                    self.symbolic_type_to_method_table(base)?
                else {
                    return Ok(None);
                };
                let base = self.method_table(base_ptr)?;
                let expected_type_def = base.token();

                let mut generics = Vec::new();
                for arg in args.iter() {
                    if let Some(arg_mt) =
                        self.symbolic_type_to_method_table(arg)?
                    {
                        generics.push(arg_mt);
                    } else {
                        return Ok(None);
                    }
                }

                let first_generic = 'first_generic: {
                    for arg in args {
                        if let Some(ptr) =
                            self.symbolic_type_to_method_table(arg)?
                        {
                            break 'first_generic Some(ptr);
                        }
                    }
                    None
                };

                [Some(base_ptr), first_generic]
                    .into_iter()
                    .flatten()
                    .map(|method_table_ptr| self.method_table(method_table_ptr))
                    .map_ok(|method_table| method_table.module())
                    .and_map_ok(|module_ptr| self.runtime_module(module_ptr))
                    .and_map_ok(|module| module.loaded_types(self))
                    .flatten_ok()
                    .and_flat_map_ok(|instantiated_generics| {
                        instantiated_generics.iter_method_tables(self)
                    })
                    .filter_map_ok(|type_handle_ptr| {
                        type_handle_ptr.as_method_table()
                    })
                    .and_map_ok(|method_table_ptr| {
                        self.method_table(method_table_ptr)
                    })
                    .and_find_ok(|method_table| -> Result<_, Error> {
                        if method_table.token() != expected_type_def {
                            return Ok(false);
                        }
                        if !method_table.has_generics() {
                            return Ok(false);
                        }

                        let mut iter_candidate_generics = method_table
                            .generic_types_excluding_base_class(self)?;
                        for generic in generics.iter().cloned() {
                            let Some(candidate_generic) =
                                iter_candidate_generics.next()
                            else {
                                return Ok(false);
                            };

                            let Some(candidate_generic) =
                                candidate_generic.as_method_table()
                            else {
                                return Ok(false);
                            };

                            if candidate_generic != generic {
                                return Ok(false);
                            }
                        }

                        if iter_candidate_generics.next().is_some() {
                            return Ok(false);
                        }

                        Ok(true)
                    })?
                    .map(|method_table| method_table.ptr())
            }
            SymbolicType::Array(element) => {
                if let Some(prim_type) = element.try_prim_type() {
                    self.find_array_method_table(&prim_type.into(), None)?
                } else {
                    todo!("Find method table for array of type {element}")
                }
            }
            SymbolicType::MultiDimArray { element_type, rank } => {
                if let Some(prim_type) = element_type.try_prim_type() {
                    self.find_array_method_table(
                        &prim_type.into(),
                        Some(*rank),
                    )?
                } else {
                    todo!(
                        "Find method table for multi-dim array \
                           of type {element_type}"
                    )
                }
            }
        };

        Ok(opt_method_table_ptr)
    }

    fn backing_type_of_enum(
        &self,
        type_metadata: MetadataTypeDefOrRef<'_>,
    ) -> Result<Option<RuntimePrimType>, Error> {
        let type_metadata = self.unwrap_type_ref(type_metadata)?;
        let MetadataTypeDefOrRef::TypeDef(type_def) = type_metadata else {
            return Ok(None);
        };

        let Some(parent) = type_def.extends()? else {
            return Ok(None);
        };
        let is_enum =
            parent.namespace()? == "System" && parent.name()? == "Enum";
        if !is_enum {
            return Ok(None);
        }

        // Enums are required to have exactly one
        // field, with the name "value__" (ECMA-335,
        // Section I.8.5.2, CLS Rule 7).
        let field = type_def
            .iter_fields()?
            .filter(|field| !field.is_static().unwrap_or(true))
            .exactly_one()
            .map_err(|_| {
                Error::EnumMustHaveExactlyOneField(
                    type_def.full_name().unwrap(),
                )
            })?;
        let prim: RuntimePrimType = match field.signature()?.field_type()? {
            SignatureType::Prim(prim) => Ok(prim.into()),
            other => Err(Error::EnumMustBeBackedByPrimitive {
                enum_name: type_def.full_name()?,
                field_type: format!("{other}"),
            }),
        }?;

        return Ok(prim.into());
    }

    pub fn signature_type_to_runtime_type(
        &self,
        sig_type: &SignatureType<'_>,
        parent_generic_types: &[TypedPointer<TypeHandle>],
    ) -> Result<RuntimeType, Error> {
        let type_formatter = |index: usize| -> SymbolicType {
            let ptr = parent_generic_types[index];
            let type_handle = self.type_handle(ptr).unwrap();
            type_handle.to_symbolic(*self).unwrap()
        };

        // TODO: Match by reference to avoid the clone.
        let runtime_type: RuntimeType = match sig_type.clone() {
            SignatureType::Prim(prim) => {
                let prim: RuntimePrimType = prim.into();
                prim.into()
            }

            SignatureType::ValueType {
                metadata, index, ..
            } => 'ty: {
                // Enums require special handling.  While they are
                // derived from `System.Enum`, and are ValueTypes,
                // they also should be treated as their underlying
                // type.
                if let Some(backing_enum_type) =
                    self.backing_type_of_enum(metadata.get(index)?)?
                {
                    break 'ty backing_enum_type.into();
                }

                let module_ptr =
                    self.runtime_module_by_name(metadata.name()?)?;
                let method_table =
                    self.method_table_by_metadata(module_ptr, index)?;

                // TODO: Either provide a better default here, or
                // refactor such that the size is determined later.
                // If no method table is found, then the size is given
                // as zero.  In any context where an object of the
                // specified type exists within the remote process,
                // the method table should exist, but a method table
                // may not yet have been generated if an object of
                // that type has not been constructed.  This typically
                // occurs for empty collections of generic types.
                //
                // For example, even if an instance of
                // `Array<MyGeneric<i32>>` exists, it may be empty and
                // so `MyGeneric<i32>` may not yet have a method
                // table.
                let size = method_table
                    .map(|ptr| -> Result<_, Error> {
                        Ok(self.method_table(ptr)?.base_size())
                    })
                    .transpose()?
                    .unwrap_or(0);
                DotNetType::ValueType {
                    method_table,
                    size,
                    symbolic: Some(sig_type.to_symbolic(&type_formatter)),
                }
                .into()
            }
            SignatureType::Class {
                metadata, index, ..
            } => {
                let module_ptr =
                    self.runtime_module_by_name(metadata.name()?)?;
                let method_table =
                    self.method_table_by_metadata(module_ptr, index)?;
                DotNetType::Class {
                    method_table,
                    symbolic: Some(sig_type.to_symbolic(&type_formatter)),
                }
                .into()
            }

            SignatureType::GenericInst {
                metadata,
                index,
                type_args,
                is_value_type,
            } => {
                let module_ptr =
                    self.runtime_module_by_name(metadata.name()?)?;
                let generic_method_table_ptr =
                    self.method_table_by_metadata(module_ptr, index)?;

                let ptr_to_loader_module = self.ptr_to_loader_module(
                    type_args
                        .first()
                        .expect("Expect at least one arg for GenericInst"),
                )?;

                let (ptr_to_defining_module, type_def_index) =
                    self.module_defining_type(module_ptr, index)?;

                let iter_loaded_types =
                    [ptr_to_loader_module, Some(ptr_to_defining_module)]
                        .into_iter()
                        .flatten()
                        .unique()
                        .map(|module_ptr| self.runtime_module(module_ptr))
                        .filter_map_ok(|module| {
                            module.loaded_types(self.reader).transpose()
                        })
                        .map(|res_loaded_types| -> Result<_, Error> {
                            let loaded_types = res_loaded_types??;
                            let iter =
                                loaded_types.iter_method_tables(self.reader)?;
                            Ok(iter)
                        })
                        .flat_map(|res_iter| match res_iter {
                            Ok(iter) => Either::Left(iter),
                            Err(err) => {
                                Either::Right(std::iter::once(Err(err)))
                            }
                        });

                let opt_method_table_ptr = generic_method_table_ptr
                    .into_iter()
                    .map(|ptr| -> Result<TypedPointer<TypeHandle>, Error> {
                        let ptr: Pointer = ptr.into();
                        Ok(ptr.into())
                    })
                    .chain(iter_loaded_types)
                    .and_find(|res_method_table_ptr| -> Result<bool, Error> {
                        let type_handle_ptr =
                            match res_method_table_ptr.as_ref() {
                                Ok(ptr) => ptr,
                                Err(_) => {
                                    return Ok(true);
                                }
                            };

                        let type_handle = self.type_handle(*type_handle_ptr)?;
                        let Some(method_table) = type_handle.as_method_table()
                        else {
                            return Ok(false);
                        };

                        if method_table.token() != Some(type_def_index) {
                            return Ok(false);
                        }

                        let generic_types: Vec<_> = method_table
                            .generic_types_excluding_base_class(self)?
                            .collect();

                        if generic_types.len() != type_args.len() {
                            return Ok(false);
                        }

                        for (sig_arg, type_handle_ptr) in
                            type_args.iter().zip(generic_types.iter())
                        {
                            let arg_matches = self
                                .signature_type_matches_type_handle(
                                    sig_arg,
                                    *type_handle_ptr,
                                    parent_generic_types,
                                )?;
                            if !arg_matches {
                                return Ok(false);
                            }
                        }

                        Ok(true)
                    })?
                    .transpose()?
                    .map(|ptr| {
                        ptr.as_method_table()
                            .ok_or(Error::GenericInstShouldNotBeTypeDescription)
                    })
                    .transpose()?;
                let symbolic = Some(sig_type.to_symbolic(&type_formatter));
                if is_value_type {
                    let size =
                        if let Some(method_table_ptr) = opt_method_table_ptr {
                            self.method_table(method_table_ptr)?.base_size()
                        } else {
                            0
                        };
                    DotNetType::ValueType {
                        method_table: opt_method_table_ptr,
                        size,
                        symbolic,
                    }
                    .into()
                } else {
                    DotNetType::Class {
                        method_table: opt_method_table_ptr,
                        symbolic,
                    }
                    .into()
                }
            }
            SignatureType::String => {
                // The String type appears as `Runtime::Class`
                // when inspecting the `desc.runtime_type()`.
                // It's technically correct, but identifying
                // the `System.String` type here, where the
                // signature must have the shorter type is
                // more convenient than identifying strings by
                // the use of the `System.String` method
                // table.
                DotNetType::String.into()
            }
            SignatureType::GenericVarFromType(var_index) => {
                let var_index = var_index as usize;
                let ptr_type_handle = parent_generic_types
                    .get(var_index)
                    .cloned()
                    .ok_or_else(|| Error::InvalidGenericTypeVar {
                        index: var_index,
                        num_vars: parent_generic_types.len(),
                    })?;

                let type_handle = self.type_handle(ptr_type_handle)?;

                let runtime_type = match type_handle {
                    TypeHandle::MethodTable(method_table) => 'ty: {
                        let module_ptr = method_table.module();
                        let module = self.runtime_module(module_ptr)?;
                        let metadata = module.metadata(self)?;
                        if let Some(token) = method_table.token() {
                            if let Some(backing_enum_type) = self
                                .backing_type_of_enum(
                                    MetadataTypeDefOrRef::TypeDef(
                                        metadata.get(token)?,
                                    ),
                                )?
                            {
                                break 'ty backing_enum_type.into();
                            }
                        }

                        method_table.runtime_type(self.reader)?
                    }
                    TypeHandle::TypeDescription(type_desc) => {
                        match type_desc.element_type() {
                            CorElementType::Prim(prim) => prim.into(),
                            CorElementType::Var => {
                                todo!(
                                    "Signature type {sig_type} \
                                     is a generic type index {var_index} \
                                     points to generic type index {}",
                                    type_desc.index().unwrap()
                                )
                            }
                            other => todo!("Type description with {other}"),
                        }
                    }
                };

                runtime_type
            }

            SignatureType::SizeArray(element_type) => {
                let symbolic_element =
                    element_type.to_symbolic(&type_formatter);
                let element_type = self.signature_type_to_runtime_type(
                    element_type.as_ref(),
                    parent_generic_types,
                )?;

                let array_method_table =
                    self.find_array_method_table(&element_type, None)?;

                DotNetType::Array {
                    method_table: array_method_table,
                    symbolic_element: Some(symbolic_element),
                }
                .into()
            }

            SignatureType::MultiDimArray {
                element_type, rank, ..
            } => {
                let symbolic_element =
                    element_type.to_symbolic(&type_formatter);
                let element_type = self.signature_type_to_runtime_type(
                    element_type.as_ref(),
                    parent_generic_types,
                )?;

                let array_method_table =
                    self.find_array_method_table(&element_type, Some(rank))?;

                DotNetType::MultiDimArray {
                    method_table: array_method_table,
                    rank,
                    symbolic_element: Some(symbolic_element),
                }
                .into()
            }
            SignatureType::Object => {
                let method_table =
                    self.method_table_by_name("System.Object")?;
                DotNetType::Class {
                    method_table,
                    symbolic: Some("System.Object".into()),
                }
                .into()
            }

            other => {
                todo!("Not currently handling {other}")
            }
        };

        runtime_type.validate().unwrap();

        Ok(runtime_type)
    }

    fn array_element_signature_to_method_table(
        &self,
        search_element: &SignatureType<'_>,
        rank: Option<usize>,
    ) -> Result<Option<TypedPointer<MethodTable>>, Error> {
        let opt_module_ptr = self.ptr_to_loader_module(search_element)?;
        let Some(module_ptr) = opt_module_ptr else {
            return Ok(None);
        };
        let module = self.runtime_module(module_ptr)?;
        let Some(loaded_types) = module.loaded_types(self)? else {
            return Ok(None);
        };

        for res_type_handle_ptr in loaded_types.iter_method_tables(self)? {
            let type_handle_ptr = res_type_handle_ptr?;
            let type_handle = self.type_handle(type_handle_ptr)?;

            let TypeHandle::MethodTable(loaded_type) = type_handle else {
                continue;
            };

            let Some(loaded_element_type_ptr) =
                loaded_type.array_element_type()
            else {
                continue;
            };

            let is_correct_element = self.signature_type_matches_type_handle(
                search_element,
                loaded_element_type_ptr,
                &[],
            )?;

            let is_correct_rank = rank == loaded_type.multi_dim_rank(self)?;

            if is_correct_element && is_correct_rank {
                return Ok(Some(loaded_type.ptr()));
            }
        }

        Ok(None)
    }

    fn find_array_method_table(
        &self,
        element_type: &RuntimeType,
        rank: Option<usize>,
    ) -> Result<Option<TypedPointer<MethodTable>>, Error> {
        let (opt_prim_element, opt_element_method_table_ptr): (
            Option<RuntimePrimType>,
            Option<TypedPointer<MethodTable>>,
        ) = match element_type {
            RuntimeType::Prim(prim) => (Some(*prim), None),
            RuntimeType::DotNet(
                DotNetType::ValueType { method_table, .. }
                | DotNetType::Class { method_table, .. }
                | DotNetType::Array { method_table, .. }
                | DotNetType::MultiDimArray { method_table, .. },
            ) => (None, *method_table),
            RuntimeType::DotNet(DotNetType::String) => {
                let name = element_type
                    .builtin_class_name()
                    .expect("String has a known name");
                let opt_method_table_ptr = self.method_table_by_name(name)?;
                (None, opt_method_table_ptr)
            }
            RuntimeType::Unknown => {
                return Ok(None);
            }
        };
        let is_string =
            matches!(element_type, RuntimeType::DotNet(DotNetType::String));

        let module_ptr = if let Some(element_method_table_ptr) =
            opt_element_method_table_ptr
        {
            let element_method_table =
                self.method_table(element_method_table_ptr)?;
            element_method_table.module()
        } else if opt_prim_element.is_some() || is_string {
            self.runtime_module_by_name("System.Private.CoreLib")?
        } else {
            return Ok(None);
        };
        let module = self.runtime_module(module_ptr)?;

        let opt_loaded_types = module.loaded_types(self)?;
        let Some(loaded_types) = opt_loaded_types else {
            return Ok(None);
        };

        for res_type_handle_ptr in loaded_types.iter_method_tables(self)? {
            let type_handle_ptr = res_type_handle_ptr?;
            let type_handle = self.type_handle(type_handle_ptr)?;

            if let TypeHandle::MethodTable(loaded_type) = type_handle {
                let is_correct_element = loaded_type
                    .array_element_type()
                    .map(|loaded_element_type_ptr| -> Result<bool, Error> {
                        let is_match = if is_string {
                            let loaded_element_type =
                                self.type_handle(loaded_element_type_ptr)?;
                            loaded_element_type.is_string()
                        } else if let Some(element_method_table_ptr) =
                            opt_element_method_table_ptr
                        {
                            loaded_element_type_ptr.as_untyped_ptr()
                                == element_method_table_ptr.as_untyped_ptr()
                        } else if let Some(prim_element) = opt_prim_element {
                            let loaded_element_type =
                                self.type_handle(loaded_element_type_ptr)?;
                            let runtime_element_type =
                                loaded_element_type.runtime_prim_type(self)?;
                            runtime_element_type == Some(prim_element)
                        } else {
                            false
                        };
                        Ok(is_match)
                    })
                    .transpose()?
                    .unwrap_or(false);

                let is_correct_rank =
                    rank == loaded_type.multi_dim_rank(self.reader)?;
                if is_correct_element && is_correct_rank {
                    return Ok(Some(loaded_type.ptr()));
                }
            }
        }

        Ok(None)
    }

    pub fn field_to_type_name(
        &self,
        desc: &FieldDescription,
    ) -> Result<&str, Error> {
        let lookup_key = (desc.method_table(), desc.token());
        self.state
            .field_to_type_name
            .try_insert(lookup_key, || {
                let module_ptr =
                    self.method_table(desc.method_table())?.module();
                let field_metadata = self
                    .runtime_module(module_ptr)?
                    .metadata(self)?
                    .get(desc.token())?;

                let signature = field_metadata.signature()?;
                let type_name = format!("{signature}");

                Ok(type_name)
            })
            .map(|s| s.as_str())
    }

    pub fn field_to_name(
        &self,
        desc: &FieldDescription,
    ) -> Result<&str, Error> {
        let lookup_key = (desc.method_table(), desc.token());
        self.state
            .field_to_name
            .try_insert(lookup_key, || {
                let module_ptr =
                    self.method_table(desc.method_table())?.module();
                let field_metadata = self
                    .runtime_module(module_ptr)?
                    .metadata(self)?
                    .get(desc.token())?;
                let name = field_metadata.name()?.to_string();

                Ok(name)
            })
            .map(|s| s.as_str())
    }

    pub fn field_descriptions(
        &self,
        method_table_ptr: TypedPointer<MethodTable>,
    ) -> Result<Option<&'a FieldDescriptions>, Error> {
        self.state
            .field_descriptions
            .try_insert(method_table_ptr, || {
                let method_table = self.method_table(method_table_ptr)?;
                method_table.get_field_descriptions(self)
            })
            .map(|opt| opt.as_ref())
    }

    pub fn module_defining_type(
        &self,
        module_ptr: TypedPointer<RuntimeModule>,
        coded_index: MetadataCodedIndex<TypeDefOrRef>,
    ) -> Result<(TypedPointer<RuntimeModule>, MetadataTableIndex<TypeDef>), Error>
    {
        let lookup_key = TypeInModule {
            module: module_ptr,
            coded_index,
        };
        self.state
            .module_defining_type
            .try_insert(lookup_key, || -> Result<_, Error> {
                if let Some(index) = coded_index.as_type::<TypeDef>() {
                    return Ok((module_ptr, index));
                }

                let module = self.runtime_module(module_ptr)?;
                let type_metadata = module.metadata(self)?.get(coded_index)?;
                let res = match type_metadata {
                    MetadataTypeDefOrRef::TypeDef(_) => {
                        panic!(
                            "Unreachable, should be \
                             handled by earlier check for TypeDef"
                        )
                    }
                    MetadataTypeDefOrRef::TypeRef(row) => self
                        .find_containing_module(
                            row.target_dll_name()?,
                            row.name()?,
                            row.namespace()?,
                        )?,
                    MetadataTypeDefOrRef::TypeSpec(_) => {
                        todo!()
                    }
                };

                Ok(res)
            })
            .cloned()
    }

    /// Look up the method table for a class/ValueType.
    ///
    /// * `full_name`: The full name of the type, including the namespace.
    pub fn method_table_by_name(
        &self,
        full_name: &str,
    ) -> Result<Option<TypedPointer<MethodTable>>, Error> {
        if let Some(value) = self.state.method_table_by_name.get(full_name) {
            Ok(Some(*value))
        } else {
            self.init_method_table_by_name()?;
            Ok(self.state.method_table_by_name.get(full_name).copied())
        }
    }

    pub fn class_exists(&self, full_name: &str) -> Result<bool, Error> {
        let all_names = self.state.names_from_metadata.or_try_init(|| {
            self.iter_known_modules()?
                .map(Ok)
                .and_map_ok(|module_ptr| self.runtime_module(module_ptr))
                .and_map_ok(|module| module.metadata(self.reader))
                .and_flat_map_ok(|metadata| {
                    Ok(metadata.type_def_table().iter_rows().map(
                        |row| -> Result<_, Error> {
                            let namespace = row.namespace()?;
                            let name = row.name()?;
                            let full_name = format!("{namespace}.{name}");
                            Ok(full_name)
                        },
                    ))
                })
                .collect()
        })?;

        let class_exists = all_names.contains(full_name);
        Ok(class_exists)
    }

    pub fn method_table_by_metadata(
        &self,
        module_ptr: TypedPointer<RuntimeModule>,
        coded_index: MetadataCodedIndex<TypeDefOrRef>,
    ) -> Result<Option<TypedPointer<MethodTable>>, Error> {
        let lookup_key = TypeInModule {
            module: module_ptr,
            coded_index,
        };
        self.state
            .method_table_by_metadata
            .try_maybe_insert(
                lookup_key,
                || -> Result<Option<TypedPointer<MethodTable>>, Error> {
                    let module = self.runtime_module(module_ptr)?;
                    let type_metadata =
                        module.metadata(self)?.get(coded_index)?;
                    let method_table_ptr = match type_metadata {
                        MetadataTypeDefOrRef::TypeDef(row) => {
                            module.get_type_def(row.index(), self)?
                        }
                        MetadataTypeDefOrRef::TypeRef(row) => {
                            self.type_ref_lookup(row)?
                        }
                        MetadataTypeDefOrRef::TypeSpec(_) => {
                            todo!()
                        }
                    };

                    Ok(method_table_ptr)
                },
            )
            .map(|opt| opt.copied())
    }

    pub fn symbolic_to_signature(
        &self,
        symbolic: &SymbolicType,
    ) -> Result<SignatureType<'a>, Error> {
        let sig = match symbolic {
            SymbolicType::Named { module, name } => match name.as_str() {
                "Object" => SignatureType::Object,
                "String" => SignatureType::String,
                "Void" => SignatureType::Void,
                "Pointer" | "ptr" | "Ptr" => {
                    SignatureType::Ptr(Box::new(SignatureType::Void))
                }
                "bool" => SignaturePrimType::Bool.into(),
                "char" => SignaturePrimType::Char.into(),
                "u8" => SignaturePrimType::U8.into(),
                "u16" => SignaturePrimType::U16.into(),
                "u32" => SignaturePrimType::U32.into(),
                "u64" => SignaturePrimType::U64.into(),
                "usize" => SignaturePrimType::NativeUInt.into(),
                "i8" => SignaturePrimType::I8.into(),
                "i16" => SignaturePrimType::I16.into(),
                "i32" => SignaturePrimType::I32.into(),
                "i64" => SignaturePrimType::I64.into(),
                "isize" => SignaturePrimType::NativeInt.into(),
                "f32" => SignaturePrimType::F32.into(),
                "f64" => SignaturePrimType::F64.into(),
                _ => {
                    let type_def = self.find_type_def(
                        &name,
                        module.as_ref().map(|s| s.as_str()),
                    )?;
                    type_def.signature()?
                }
            },

            SymbolicType::Metadata {
                module: module_name,
                index,
            } => {
                let module_ptr = self.runtime_module_by_name(module_name)?;
                let module = self.runtime_module(module_ptr)?;
                let metadata = module.metadata(self)?;
                let type_def = metadata.get(*index)?;
                match type_def {
                    MetadataTypeDefOrRef::TypeDef(row) => row.signature()?,
                    MetadataTypeDefOrRef::TypeRef(row) => {
                        let type_def = self.find_type_def(
                            row.full_name()?.as_str(),
                            Some(row.target_dll_name()?),
                        )?;
                        type_def.signature()?
                    }
                    MetadataTypeDefOrRef::TypeSpec(_) => todo!(),
                }
            }

            SymbolicType::GenericInst { base, args } => {
                let base = self.symbolic_to_signature(base)?;
                let type_args: Vec<_> = args
                    .iter()
                    .map(|arg| self.symbolic_to_signature(arg))
                    .collect::<Result<_, _>>()?;
                base.with_type_args(type_args)?
            }

            SymbolicType::Array(element) => {
                let element = self.symbolic_to_signature(element)?;
                SignatureType::SizeArray(Box::new(element))
            }
            SymbolicType::MultiDimArray { element_type, rank } => {
                let element_type = self.symbolic_to_signature(element_type)?;
                SignatureType::MultiDimArray {
                    element_type: Box::new(element_type),
                    rank: *rank,
                    fixed_sizes: Vec::new(),
                    lower_bounds: Vec::new(),
                }
            }
        };

        Ok(sig)
    }

    pub fn signature_to_method_table(
        &self,
        sig: &SignatureType<'_>,
    ) -> Result<Option<TypedPointer<MethodTable>>, Error> {
        let opt_method_table_ptr: Option<TypedPointer<MethodTable>> = match sig
        {
            SignatureType::ValueType { index, metadata }
            | SignatureType::Class { index, metadata } => {
                let module = self.runtime_module_by_name(metadata.name()?)?;
                self.method_table_by_metadata(module, *index)?
            }
            SignatureType::GenericInst {
                index,
                metadata,
                type_args,
                ..
            } => {
                let base_type_def = match self.unwrap_type_ref(metadata.get(*index)?)? {
                    MetadataTypeDefOrRef::TypeDef(row) => row,
                    MetadataTypeDefOrRef::TypeRef(_) => unreachable!("Handled by unwrap_type_ref"),
                    MetadataTypeDefOrRef::TypeSpec(_) => panic!("Uninstantiated generic should be TypeDef or TypeRef, not TypeSpec"),
                };

                let defining_module = self.ptr_to_loader_module(sig)?;
                let loader_module =
                    self.ptr_to_loader_module(type_args.first().unwrap())?;
                let iter_loaded_types = [defining_module, loader_module]
                    .into_iter()
                    .flatten()
                    .unique()
                    .map(Ok::<_, Error>)
                    .and_map_ok(|module_ptr| self.runtime_module(module_ptr))
                    .filter_map_ok(|module| {
                        module.loaded_types(self).transpose()
                    })
                    .flatten()
                    .flat_map_ok(|loaded_types| {
                        loaded_types.iter_method_tables(self)
                    })
                    .flatten();

                let opt_method_table = iter_loaded_types
                    .and_map_ok(|type_handle_ptr| {
                        self.type_handle(type_handle_ptr)
                    })
                    .filter_map_ok(|type_handle| match type_handle {
                        TypeHandle::MethodTable(method_table) => {
                            Some(method_table)
                        }
                        TypeHandle::TypeDescription(_) => None,
                    })
                    .filter_ok(|method_table| {
                        method_table.token() == Some(base_type_def.index())
                    })
                    .and_find_ok(|method_table| -> Result<bool, Error> {
                        let generic_types: Vec<_> = method_table
                            .generic_types_excluding_base_class(self)?
                            .collect();
                        if generic_types.len() != type_args.len() {
                            return Ok(false);
                        }
                        for (sig_arg, type_handle_ptr) in
                            type_args.iter().zip(generic_types)
                        {
                            let arg_matches = self
                                .signature_type_matches_type_handle(
                                    sig_arg,
                                    type_handle_ptr,
                                    &[],
                                )?;
                            if !arg_matches {
                                return Ok(false);
                            }
                        }
                        Ok(true)
                    })?;

                opt_method_table.map(|method_table| method_table.ptr())
            }

            SignatureType::SizeArray(element_type) => self
                .array_element_signature_to_method_table(
                    element_type.as_ref(),
                    None,
                )?,

            SignatureType::MultiDimArray {
                element_type, rank, ..
            } => self.array_element_signature_to_method_table(
                element_type.as_ref(),
                Some(*rank),
            )?,

            other => todo!("Find method table for {other}"),
        };

        Ok(opt_method_table_ptr)
    }

    pub fn method_table_to_name(
        &self,
        ptr: TypedPointer<MethodTable>,
    ) -> Result<&str, Error> {
        self.state
            .method_table_to_name
            .try_insert(ptr, || {
                let method_table = self.method_table(ptr)?;

                let fullname = format!(
                    "{}",
                    TypeHandle::MethodTable(method_table.clone())
                        .printable(*self)
                );

                // let Some(type_def_token) = method_table.token() else {
                //     return Ok("(null metadata token)".to_string());
                // };

                // let module = self.runtime_module(method_table.module())?;
                // let metadata = module.metadata(self)?;

                // let type_def = metadata.get(type_def_token)?;
                // let name = type_def.name()?;

                // let namespace = type_def.namespace()?;
                // let namespace = if namespace.is_empty() {
                //     None
                // } else {
                //     Some(namespace)
                // };

                // let extends = type_def
                //     .extends()?
                //     .map(|type_def_or_ref| type_def_or_ref.name())
                //     .transpose()?;

                // let fullname = match (namespace, extends) {
                //     (Some(namespace), Some(extends)) => {
                //         format!("{namespace}.{name} extends {extends}")
                //     }
                //     (None, Some(extends)) => {
                //         format!("{name} extends {extends}")
                //     }
                //     (Some(namespace), None) => format!("{namespace}.{name}"),
                //     (None, None) => format!("{name}"),
                // };

                Ok(fullname)
            })
            .map(|s| s.as_str())
    }

    pub fn iter_known_modules(
        &self,
    ) -> Result<impl Iterator<Item = TypedPointer<RuntimeModule>> + '_, Error>
    {
        if self.state.runtime_module_by_name.is_empty() {
            self.init_dlls()?;
        }
        let iter = self
            .state
            .runtime_module_by_name
            .clone()
            .into_tuple_vec()
            .into_iter()
            .map(|(_name, ptr)| *ptr.deref());
        Ok(iter)
    }

    pub fn iter_static_fields(
        &self,
        method_table_ptr: TypedPointer<MethodTable>,
    ) -> Result<impl Iterator<Item = FieldDescription<'a>> + 'a, Error> {
        // Static fields are stored by class, so there's no need to
        // check the parent classes.
        let iter = self
            .field_descriptions(method_table_ptr)?
            .into_iter()
            .flatten()
            .filter(|field| field.is_static());

        Ok(iter)
    }

    pub fn iter_instance_fields(
        &self,
        method_table_ptr: TypedPointer<MethodTable>,
    ) -> Result<
        impl Iterator<Item = (TypedPointer<MethodTable>, FieldDescription<'a>)> + 'a,
        Error,
    > {
        // Each class only records its own instance fields.  To get a
        // full record of all instance fields, the parent classes must
        // also be inspected.
        let iter = std::iter::successors(
            Some(Ok(method_table_ptr)),
            |res_ptr| -> Option<Result<_, Error>> {
                res_ptr
                    .as_ref()
                    .ok()
                    .map(|method_table_ptr| -> Result<_, Error> {
                        let method_table =
                            self.method_table(*method_table_ptr)?;
                        let parent = method_table.parent_method_table();
                        Ok(parent)
                    })
                    .and_then(|res| res.transpose())
            },
        )
        .flat_map_ok(|method_table_ptr| {
            let iter = self
                .field_descriptions(method_table_ptr)?
                .into_iter()
                .flatten()
                .map(move |field| Ok((method_table_ptr, field)));
            Ok(iter)
        })
        .map(|res| res?)
        .filter_ok(|(_, field)| !field.is_static())
        .collect::<Result<Vec<_>, Error>>()?
        .into_iter();

        Ok(iter)
    }

    fn find_containing_module(
        &self,
        assembly_name: &str,
        symbol_name: &str,
        symbol_namespace: &str,
    ) -> Result<(TypedPointer<RuntimeModule>, MetadataTableIndex<TypeDef>), Error>
    {
        let field_module_ptr = self.runtime_module_by_name(assembly_name)?;
        let field_module = self.runtime_module(field_module_ptr)?;
        let metadata = field_module.metadata(self)?;

        let ref_type_def = metadata.type_def_table().iter_rows().and_find(
            |row| -> Result<_, Error> {
                Ok(row.name()? == symbol_name
                    && row.namespace()? == symbol_namespace)
            },
        )?;

        if let Some(ref_type_def) = ref_type_def {
            // The TypeRef pointed to an assembly, and that assembly
            // contains the TypeDef we're looking for.
            return Ok((field_module.location.into(), ref_type_def.index()));
        }

        let ref_exported_type = metadata
            .exported_type_table()
            .iter_rows()
            .and_find(|row| -> Result<_, Error> {
                Ok(row.name()? == symbol_name
                    && row.namespace()? == symbol_namespace)
            })?;

        if let Some(ref_exported_type) = ref_exported_type {
            // > I'm sorry, but your TypeDef is in another castle.
            //
            // The TypeRef/ExportedType pointed to an assembly, but
            // that assembly just re-exports a symbol, and points to
            // another
            return self.find_containing_module(
                ref_exported_type.target_dll_name()?,
                ref_exported_type.name()?,
                ref_exported_type.namespace()?,
            );
        }

        use itertools::Itertools;
        let names = std::iter::empty()
            .chain(metadata.type_def_table().iter_rows().filter_map(|row| {
                let name = row.name().ok()?;
                let namespace = row.namespace().ok()?;
                Some(format!("  TypeDef {namespace} . {name}"))
            }))
            .chain(metadata.exported_type_table().iter_rows().filter_map(
                |row| {
                    let name = row.name().ok()?;
                    let namespace = row.namespace().ok()?;
                    Some(format!("  ExportedType {namespace} . {name}"))
                },
            ))
            .sorted()
            .join("\n");

        panic!(
            "Could not find {symbol_namespace} . {symbol_name} \
             in the assembly {assembly_name}.  \
             The assembly contains symbols:\n{names}"
        )
    }

    fn type_ref_lookup(
        &self,
        type_ref: MetadataRow<TypeRef>,
    ) -> Result<Option<TypedPointer<MethodTable>>, Error> {
        self.pseudo_link_method_table(
            type_ref.target_dll_name()?,
            type_ref.name()?,
            type_ref.namespace()?,
        )
    }

    fn pseudo_link_method_table(
        &self,
        assembly_name: &str,
        symbol_name: &str,
        symbol_namespace: &str,
    ) -> Result<Option<TypedPointer<MethodTable>>, Error> {
        let field_module_ptr = self.runtime_module_by_name(assembly_name)?;
        let field_module = self.runtime_module(field_module_ptr)?;
        let metadata = field_module.metadata(self)?;

        let ref_type_def = metadata.type_def_table().iter_rows().and_find(
            |row| -> Result<_, Error> {
                Ok(row.name()? == symbol_name
                    && row.namespace()? == symbol_namespace)
            },
        )?;

        if let Some(ref_type_def) = ref_type_def {
            // The TypeRef pointed to an assembly, and that assembly
            // contains the TypeDef we're looking for.
            let ptr = field_module.get_type_def(ref_type_def.index(), self)?;

            return Ok(ptr);
        }

        let ref_exported_type = metadata
            .exported_type_table()
            .iter_rows()
            .and_find(|row| -> Result<_, Error> {
                Ok(row.name()? == symbol_name
                    && row.namespace()? == symbol_namespace)
            })?;

        if let Some(ref_exported_type) = ref_exported_type {
            // > I'm sorry, but your TypeDef is in another castle.
            //
            // The TypeRef/ExportedType pointed to an assembly, but
            // that assembly just re-exports a symbol, and points to
            // another
            return self.pseudo_link_method_table(
                ref_exported_type.target_dll_name()?,
                ref_exported_type.name()?,
                ref_exported_type.namespace()?,
            );
        }

        use itertools::Itertools;
        let names = std::iter::empty()
            .chain(metadata.type_def_table().iter_rows().filter_map(|row| {
                let name = row.name().ok()?;
                let namespace = row.namespace().ok()?;
                Some(format!("  TypeDef {namespace} . {name}"))
            }))
            .chain(metadata.exported_type_table().iter_rows().filter_map(
                |row| {
                    let name = row.name().ok()?;
                    let namespace = row.namespace().ok()?;
                    Some(format!("  ExportedType {namespace} . {name}"))
                },
            ))
            .sorted()
            .join("\n");

        panic!(
            "Could not find {symbol_namespace} . {symbol_name} \
                in the assembly {assembly_name}.  \
                The assembly contains symbols:\n{names}"
        )
    }

    /// Returns ranges in which static values may exist.  Used for
    /// loading all static values with a minimal number of reads from
    /// the remote process.
    pub fn static_value_ranges(
        &self,
        ptr: TypedPointer<RuntimeModule>,
    ) -> impl Iterator<Item = Range<Pointer>> + '_ {
        let res_iter = || -> Result<_, Error> {
            let module = self.runtime_module(ptr)?;

            let iter = self
                .runtime_module(ptr)?
                .iter_method_table_pointers(self)?
                .filter(|&method_table_ptr| {
                    // TODO: Handle unpacking/display for static
                    // fields of generic types.
                    self.method_table(method_table_ptr)
                        .map(|method_table| !method_table.has_generics())
                        .unwrap_or(true)
                })
                .flat_map(|method_table_ptr| {
                    self.iter_static_fields(method_table_ptr)
                        .into_iter()
                        .flatten()
                        .map(move |field| -> Result<_, Error> {
                            let runtime_type = self.field_to_runtime_type(
                                method_table_ptr,
                                &field,
                            )?;
                            let start = field.location(
                                module,
                                FieldContainer::Static,
                                self,
                            )?;

                            let size = runtime_type.size_bytes()?;

                            Ok(start..start + size)
                        })
                })
                .filter_map(|res| res.ok())
                .sorted_by_key(|range| (range.start, Reverse(range.end)))
                .peekable()
                .batching(|iter| {
                    let mut range = iter.next()?;
                    while let Some(next) =
                        iter.next_if(|peek| peek.start <= range.end + 128)
                    {
                        range = range.start..range.end.max(next.end);
                    }
                    Some(range)
                });
            Ok(iter)
        }();

        res_iter.into_iter().flatten()
    }

    pub fn is_base_of(
        &self,
        parent_class_ptr: TypedPointer<MethodTable>,
        mut child_class_ptr: TypedPointer<MethodTable>,
    ) -> Result<bool, Error> {
        loop {
            if child_class_ptr == parent_class_ptr {
                return Ok(true);
            }

            let child_class = self.method_table(child_class_ptr)?;
            if let Some(next_parent) = child_class.parent_method_table() {
                child_class_ptr = next_parent;
            } else {
                return Ok(false);
            }
        }
    }
}

impl<'a> Deref for CachedReader<'a> {
    type Target = MemoryReader;

    fn deref(&self) -> &Self::Target {
        self.reader
    }
}

impl<'a> AsRef<MemoryReader> for CachedReader<'a> {
    fn as_ref(&self) -> &MemoryReader {
        self.reader
    }
}

impl<'a> Borrow<MemoryReader> for CachedReader<'a> {
    fn borrow(&self) -> &MemoryReader {
        self.reader
    }
}

impl<'a, 'b> Borrow<MemoryReader> for &'b CachedReader<'a> {
    fn borrow(&self) -> &MemoryReader {
        self.reader
    }
}

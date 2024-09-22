use std::borrow::Borrow;
use std::cell::OnceCell;
use std::cmp::Reverse;
use std::ops::{Deref, Range};

use elsa::FrozenMap;

use dll_unpacker::{
    Field, MetadataCodedIndex, MetadataRow, MetadataTableIndex,
    MetadataTypeDefOrRef, TypeDef, TypeDefOrRef, TypeRef,
};
use itertools::{Either, Itertools};
use memory_reader::{MemoryMapRegion, MemoryReader, Pointer};

use crate::{extensions::*, CorElementType, FieldContainer, TypeHandle};
use crate::{
    Error, FieldDescription, FieldDescriptions, MethodTable, RuntimeModule,
    RuntimeObject, RuntimeType, RuntimeValue, TypedPointer,
};

/// Cache containing known state of the remote process.  Only values
/// that are stable across the lifetime of the remote process should
/// be stored here.  For example, the location of a static field may
/// be stored, but the value contained within that static field may
/// not.
#[derive(Default)]
pub struct StaticValueCache {
    method_tables: FrozenMap<TypedPointer<MethodTable>, Box<MethodTable>>,
    runtime_module_vtable: OnceCell<Pointer>,
    runtime_modules: FrozenMap<TypedPointer<RuntimeModule>, Box<RuntimeModule>>,
    field_descriptions:
        FrozenMap<TypedPointer<MethodTable>, Box<Option<FieldDescriptions>>>,
    runtime_module_by_name: FrozenMap<String, Box<TypedPointer<RuntimeModule>>>,
    method_table_by_metadata:
        FrozenMap<TypeInModule, Box<TypedPointer<MethodTable>>>,

    module_defining_type: FrozenMap<
        TypeInModule,
        Box<(TypedPointer<RuntimeModule>, MetadataTableIndex<TypeDef>)>,
    >,

    field_to_runtime_type: FrozenMap<
        (
            TypedPointer<MethodTable>, // Parent's method table
            TypedPointer<MethodTable>, // Field's method table
            MetadataTableIndex<Field>,
        ),
        Box<RuntimeType>,
    >,
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
        self.state
            .method_tables
            .try_insert(ptr, || ptr.read(self.reader))
    }

    pub fn runtime_module(
        &self,
        ptr: TypedPointer<RuntimeModule>,
    ) -> Result<&RuntimeModule, Error> {
        self.state.runtime_modules.try_insert(ptr, || {
            let runtime_module = ptr.read(self)?;

            self.state
                .runtime_module_vtable
                .or_try_init(|| runtime_module.vtable_location(self))?;

            // Save the name of the module, to avoid needing to do a
            // memory search for it later.
            //
            // TODO: See if this could cause a weird order-dependent
            // error, if a module can be successfully located through
            // a known pointer, but cannot be located by the
            // heuristics used to search in memory.  If that is the
            // case, then this caching could paper over a need to
            // improve the heuristics.
            let name = runtime_module.name(self)?;
            self.state
                .runtime_module_by_name
                .insert(name.to_string(), Box::new(ptr));

            Ok(runtime_module)
        })
    }

    fn iter_clr_dll_regions(&self) -> impl Iterator<Item = &MemoryMapRegion> {
        self.reader
            .regions
            .iter()
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
            .filter_map(|region| region.read().ok())
            .collect::<Vec<_>>();

        let layouts = dll_data
            .iter()
            .map(|data| dll_unpacker::unpack_metadata_layout(data))
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
                    .filter_map(|&ptr| RuntimeModule::read(ptr.into()).ok())
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
        let modules_pointers =
            RuntimeModule::locate_by_vtable(&metadata, module_vtable, self)?;

        modules_pointers
            .into_iter()
            .filter_map(|opt_ptr| opt_ptr)
            .try_for_each(|ptr| -> Result<_, Error> {
                self.runtime_module(ptr)?;
                Ok(())
            })?;

        Ok(())
    }

    pub fn runtime_module_by_name(
        &self,
        name: &str,
    ) -> Result<TypedPointer<RuntimeModule>, Error> {
        // TODO: Have `try_insert` accept `impl ToOwned<Owned=T>` to
        // avoid needing to copy the string.
        self.state
            .runtime_module_by_name
            .try_insert(name.to_string(), || {
                let dll_region = self
                    .iter_clr_dll_regions()
                    .filter(|region| {
                        region.short_name().trim_end_matches(".dll") == name
                    })
                    .max_by_key(|region| region.size_bytes())
                    .ok_or_else(|| {
                        Error::RegionForDLLNotFoundFromName(name.to_string())
                    })?
                    .read()?;

                let metadata_layout =
                    dll_unpacker::unpack_metadata_layout(&dll_region)?;
                let metadata = metadata_layout.metadata(&dll_region);
                let module_vtable =
                    self.state.runtime_module_vtable.get().copied();

                let module_ptr =
                    RuntimeModule::locate(metadata, module_vtable, self)?;

                Ok(module_ptr)
            })
            .copied()
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

        self.state
            .field_to_runtime_type
            .try_insert(lookup_key, || {
                match desc.cor_element_type()? {
                    CorElementType::Prim(prim) => {
                        return Ok(RuntimeType::Prim(prim));
                    }
                    _ => {}
                }

                let parent = self.method_table(ptr_mtable_of_parent)?;
                let module_ptr = parent.module();
                let module = self.runtime_module(module_ptr)?;
                let metadata = module.metadata(self)?;
                let field_metadata = metadata.get(desc.token())?;
                let signature = field_metadata.signature()?;

                let sig_type = signature.first_type()?;

                let ty: RuntimeType = match sig_type.clone() {
                    dll_unpacker::SignatureType::Prim(prim) => {
                        // This case should already have been handled
                        // by checking the field description.
                        RuntimeType::Prim(prim.into())
                    }
                    dll_unpacker::SignatureType::ValueType {
                        index, ..
                    } => {
                        let method_table =
                            self.method_table_by_metadata(module_ptr, index)?;
                        let size = self.method_table(method_table)?.base_size();
                        RuntimeType::ValueType { method_table, size }
                    }
                    dll_unpacker::SignatureType::GenericInst {
                        is_value_type: true,
                        index,
                        type_args,
                        ..
                    } => {
                        let ptr_to_loader_module = type_args
                            .iter()
                            .next()
                            .map(|arg| -> Result<_,Error> {
                                match arg.as_ref() {
                                    dll_unpacker::SignatureType::Prim(_) => {
                                        self.runtime_module_by_name("System.Private.CoreLib")
                                    }
                                    dll_unpacker::SignatureType::Class { index, .. } |
                                    dll_unpacker::SignatureType::ValueType { index, .. } => {
                                        self.module_defining_type(module_ptr,*index)
                                            .map(|(ptr,_)| ptr)
                                    }
                                    _ => todo!(
                                        "Handle case, \
                                         first argument of {sig_type} is {arg}"
                                    ),
                                }
                            })
                            .expect("GenericInst should have at least one argument")?
                            .clone();

                        let (
                            ptr_to_defining_module,
                            type_def_index,
                        ) = self.module_defining_type(module_ptr,index)?;

                        let method_table_ptr = [
                            ptr_to_loader_module,
                            ptr_to_defining_module,
                        ]
                            .into_iter()
                            .unique()
                            .map(|module_ptr| self.runtime_module(module_ptr))
                            .filter_map_ok(|module| {
                                module
                                    .loaded_types(self.reader)
                                    .transpose()
                            })
                            .map(|res_loaded_types| -> Result<_,Error> {
                                let loaded_types = res_loaded_types??;
                                let iter = loaded_types.iter_method_tables(self.reader)?;
                                Ok(iter)
                            })
                            .flat_map(|res_iter| {
                                match res_iter {
                                    Ok(iter) => Either::Left(iter),
                                    Err(err) => Either::Right(std::iter::once(Err(err))),
                                }
                            })
                            .find_ok(|res_method_table_ptr| -> Result<bool,Error> {
                                let type_handle_ptr = match res_method_table_ptr.as_ref() {
                                    Ok(ptr) => ptr,
                                    Err(_) => {
                                        return Ok(true);
                                    }
                                };

                                let type_handle = type_handle_ptr.read(self.reader)?;
                                let method_table = match &type_handle {
                                    TypeHandle::MethodTable(mt) => mt,
                                    TypeHandle::TypeDescription(_) => {
                                        return Ok(false);
                                    },
                                };

                                if method_table.token() != Some(type_def_index) {
                                    return Ok(false);
                                }

                                let generic_types = method_table.generic_types(self)?;

                                if generic_types.len() != type_args.len() {
                                    return Ok(false);
                                }

                                for (sig_arg, type_handle_ptr) in type_args.iter().zip(generic_types.iter())
                                    {
                                        let type_handle = type_handle_ptr.read(self).unwrap();

                                        let arg_matches = match sig_arg.as_ref() {
                                            dll_unpacker::SignatureType::Class { index, .. } |
                                            dll_unpacker::SignatureType::ValueType { index, .. } => {
                                                match type_handle {
                                                    TypeHandle::MethodTable(arg_method_table) => {
                                                        let (expected_module,expected_token) = self
                                                            .module_defining_type(module_ptr,*index)?;

                                                        arg_method_table.module()==expected_module &&
                                                            arg_method_table.token() == Some(expected_token)
                                                    }
                                                    TypeHandle::TypeDescription(_) => false,
                                                }

                                            }
                                            dll_unpacker::SignatureType::Prim(sig_prim_type) => {
                                                match type_handle{
                                                    TypeHandle::TypeDescription(type_description) => {
                                                        match type_description.element_type() {
                                                            CorElementType::Prim(actual_prim_type) => Some(actual_prim_type),
                                                            _ => None,
                                                        }
                                                    }
                                                    TypeHandle::MethodTable(method_table) => {
                                                        match method_table.runtime_type(self.reader).unwrap() {
                                                            RuntimeType::Prim(actual_prim_type) => Some(actual_prim_type),
                                                            _ => None,
                                                        }
                                                    }
                                                }
                                                  .map(|prim_type| sig_prim_type == &prim_type)
                                                  .unwrap_or(false)
                                            }
                                            dll_unpacker::SignatureType::GenericInst { .. } => {
                                                todo!("Extract out a 'matches_signature' function, \
                                                       handle this case through recursion.")
                                            }
                                            _ => todo!(),
                                        };
                                        if !arg_matches {
                                            return Ok(false);
                                        }
                                    }

                                Ok(true)
                            })?
                            .ok_or_else(|| {
                                Error::GenericMethodTableNotFound(format!("{sig_type}"))
                            })??
                            .as_method_table()
                            .ok_or(Error::GenericInstShouldNotBeTypeDescription)?;

                        let size = self.method_table(method_table_ptr)?.base_size();
                        RuntimeType::ValueType { method_table: method_table_ptr, size }
                    }
                    dll_unpacker::SignatureType::GenericInst { .. } => {
                        RuntimeType::Class
                    }
                    dll_unpacker::SignatureType::Class { .. } => {
                        RuntimeType::Class
                    }
                    dll_unpacker::SignatureType::String => {
                        // The String type appears as `Runtime::Class`
                        // when inspecting the `desc.runtime_type()`.
                        // It's technically correct, but identifying
                        // the `System.String` type here, where the
                        // signature must have the shorter type is
                        // more convenient than identifying strings by
                        // the use of the `System.String` method
                        // table.
                        RuntimeType::String
                    }
                    dll_unpacker::SignatureType::SizeArray(_) => {
                        RuntimeType::Array
                    }
                    dll_unpacker::SignatureType::GenericVarFromType(
                        var_index,
                    ) => {
                        let mtable_of_parent =
                            self.method_table(ptr_mtable_of_parent)?;

                        let generic_types =
                            mtable_of_parent.generic_types(self)?;

                        let var_index = var_index as usize;
                        let ptr_type_handle = generic_types
                            .get(var_index)
                            .ok_or_else(|| Error::InvalidGenericTypeVar {
                                index: var_index,
                                num_vars: generic_types.len(),
                            })?
                            .clone();

                        let type_handle = ptr_type_handle.read(self.reader)?;
                        let runtime_type = match type_handle {
                            TypeHandle::MethodTable(method_table) => method_table.runtime_type(self.reader)?,
                            TypeHandle::TypeDescription(type_desc) => match type_desc.element_type() {
                                CorElementType::Prim(prim) => RuntimeType::Prim(prim),
                                _ => todo!(),
                            },
                        };

                        runtime_type
                    }
                    dll_unpacker::SignatureType::Array { .. } => {
                        RuntimeType::Class
                    }
                    dll_unpacker::SignatureType::Object => RuntimeType::Class,

                    other => {
                        todo!("Not currently handling {other}")
                    }
                };

                Ok(ty)
            })
            .cloned()
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
        ptr: TypedPointer<MethodTable>,
    ) -> Result<Option<&'a FieldDescriptions>, Error> {
        self.state
            .field_descriptions
            .try_insert(ptr, || {
                let method_table = self.method_table(ptr)?;
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

    pub fn method_table_by_metadata(
        &self,
        module_ptr: TypedPointer<RuntimeModule>,
        coded_index: MetadataCodedIndex<TypeDefOrRef>,
    ) -> Result<TypedPointer<MethodTable>, Error> {
        let lookup_key = TypeInModule {
            module: module_ptr,
            coded_index,
        };
        self.state
            .method_table_by_metadata
            .try_insert(
                lookup_key,
                || -> Result<TypedPointer<MethodTable>, Error> {
                    let module = self.runtime_module(module_ptr)?;
                    let type_metadata =
                        module.metadata(self)?.get(coded_index)?;
                    let method_table_ptr = match type_metadata {
                        MetadataTypeDefOrRef::TypeDef(row) => {
                            module.get_method_table(row.index(), self)?
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
            .and_then(|ptr| {
                if ptr.is_null() {
                    let module = self.runtime_module(module_ptr)?;
                    let metadata = module.metadata(self)?.get(coded_index)?;
                    let name = metadata.name()?;
                    let namespace = metadata.namespace()?;
                    Err(Error::UnexpectedNullMethodTable(format!(
                        "{namespace}.{name}"
                    )))
                } else {
                    Ok(ptr)
                }
            })
            .copied()
    }

    pub fn method_table_to_name(
        &self,
        ptr: TypedPointer<MethodTable>,
    ) -> Result<&str, Error> {
        self.state
            .method_table_to_name
            .try_insert(ptr, || {
                let method_table = self.method_table(ptr)?;
                let Some(type_def_token) = method_table.token() else {
                    return Ok("(null metadata token)".to_string());
                };

                let module = self.runtime_module(method_table.module())?;
                let metadata = module.metadata(self)?;

                let type_def = metadata.get(type_def_token)?;
                let name = type_def.name()?;

                let namespace = type_def.namespace()?;
                let namespace = if namespace.is_empty() {
                    None
                } else {
                    Some(namespace)
                };

                let extends = type_def
                    .extends()?
                    .map(|type_def_or_ref| type_def_or_ref.name())
                    .transpose()?;

                let fullname = match (namespace, extends) {
                    (Some(namespace), Some(extends)) => {
                        format!("{namespace}.{name} extends {extends}")
                    }
                    (None, Some(extends)) => {
                        format!("{name} extends {extends}")
                    }
                    (Some(namespace), None) => format!("{namespace}.{name}"),
                    (None, None) => format!("{name}"),
                };

                Ok(fullname)
            })
            .map(|s| s.as_str())
    }

    pub fn iter_known_modules(
        &self,
    ) -> impl Iterator<Item = Result<TypedPointer<RuntimeModule>, Error>> + '_
    {
        self.iter_clr_dll_regions()
            .map(|region| region.short_name().trim_end_matches(".dll"))
            .map(|name| self.runtime_module_by_name(name))
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
    ) -> Result<impl Iterator<Item = FieldDescription<'a>> + 'a, Error> {
        // Each class only records its own instance fields.  To get a
        // full record of all instance fields, the parent classes must
        // also be inspected.
        let iter = std::iter::successors(
            Some(Ok(method_table_ptr)),
            |res_ptr| -> Option<Result<_, Error>> {
                res_ptr
                    .as_ref()
                    .ok()
                    .map(|ptr| -> Result<_, Error> {
                        let method_table = self.method_table(*ptr)?;
                        let parent = method_table.parent_method_table();
                        Ok(parent)
                    })
                    .map(|res| res.transpose())
                    .flatten()
            },
        )
        .map(|res_ptr| res_ptr.and_then(|ptr| self.field_descriptions(ptr)))
        .collect::<Result<Vec<_>, Error>>()?
        .into_iter()
        .flatten()
        .flatten()
        .filter(|field| !field.is_static());

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

        let ref_type_def = metadata.type_def_table().iter_rows().find_ok(
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
            .find_ok(|row| -> Result<_, Error> {
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
    ) -> Result<TypedPointer<MethodTable>, Error> {
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
    ) -> Result<TypedPointer<MethodTable>, Error> {
        let field_module_ptr = self.runtime_module_by_name(assembly_name)?;
        let field_module = self.runtime_module(field_module_ptr)?;
        let metadata = field_module.metadata(self)?;

        let ref_type_def = metadata.type_def_table().iter_rows().find_ok(
            |row| -> Result<_, Error> {
                Ok(row.name()? == symbol_name
                    && row.namespace()? == symbol_namespace)
            },
        )?;

        if let Some(ref_type_def) = ref_type_def {
            // The TypeRef pointed to an assembly, and that assembly
            // contains the TypeDef we're looking for.
            let ptr =
                field_module.get_method_table(ref_type_def.index(), self)?;

            return Ok(ptr);
        }

        let ref_exported_type = metadata
            .exported_type_table()
            .iter_rows()
            .find_ok(|row| -> Result<_, Error> {
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
                                &module,
                                FieldContainer::Static,
                                self,
                            )?;

                            let size = runtime_type.size_bytes();
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
}

impl<'a> Deref for CachedReader<'a> {
    type Target = MemoryReader;

    fn deref(&self) -> &Self::Target {
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

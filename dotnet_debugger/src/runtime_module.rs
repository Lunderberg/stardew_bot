use std::borrow::Borrow;
use std::cell::OnceCell;
use std::collections::HashMap;
use std::ops::Range;

use dll_unpacker::{
    Annotation, Annotator, Metadata, MetadataLayout, MetadataTableIndex,
    TypeDef,
};
use iterator_extensions::ResultIteratorExt as _;
use itertools::Itertools as _;
use memory_reader::{
    extensions::*, MemoryMapRegion, MemoryRegion, OwnedBytes, UnpackedValue,
};
use memory_reader::{MemoryReader, Pointer};

use crate::{extensions::*, unpack_fields, TypeHandle};
use crate::{Error, MethodTable, ReadTypedPointer, TypedPointer};

/// Contains layout information that can be applied to any RuntimeModule.
///
/// For small structs in the .NET runtime, the layout doesn't usually have
/// significant changes between versions.  However, the layout of a runtime
/// module (the `Module` class in `ceeload.h` of the .NET runtime) can change
/// significantly between .NET versions.  As a result, the location of fields
/// contained within the Module are located by pattern-matching, not by static
/// offsets.
///
/// However, this pattern-matching is much slower than applying an offset, and
/// can fail in some edge cases.  For example, the non-generic type definitions
/// of `System.Private.CoreLib.dll` are stored in a separate lookup, causing the
/// pattern-match used to locate the hashmap of instantiated generics to fail.
///
/// Since the offsets are constant for any specific instance of the .NET
/// runtime, the pattern matching only needs to be performed applied for one
/// instance of RuntimeModule, and all subsequent instances can re-use the same
/// results.  The `RuntimeModuleLayout` struct contains this re-usable layout
/// information.
#[derive(Debug, Clone)]
pub struct RuntimeModuleLayout {
    /// The offset to the lookup table of TypeDef to MethodTable.  If verifying
    /// against the debug symbols for API compatibility, this field is called
    /// `m_TypeDefToMethodTableMap`.
    offset_to_type_def_table: usize,

    /// The offset to the hashmap of instantiated generic types.  If verifying
    /// against the debug symbols for API compatibility, this field is called
    /// `m_pAvailableParamTypes`.
    offset_to_instantiated_generics: usize,

    /// The offset to the address used as the base address for all
    /// non-garbage-collected static fields.  If verifying against the debug
    /// symbols for API compatibility, this field is called `m_ModuleID`.
    offset_to_base_address_of_non_gc_static_values: usize,
}

/// Contains locations of key structures within the runtime
/// representation of a single CLR DLL.
pub struct RuntimeModule {
    /// Location of the Module object
    pub location: Pointer,

    /// The layout of the module.  May be inferred by pattern-matching
    /// if not provided in `RuntimeModule::new`.
    layout: OnceCell<RuntimeModuleLayout>,

    /// Location of the vtable
    vtable_location: OnceCell<Pointer>,

    /// Location of the DLL in memory
    image_ptr: OnceCell<Pointer>,

    /// The DLL corresponding to this Module.
    dll_region_info: OnceCell<MemoryMapRegion>,

    /// The DLL corresponding to this Module.
    dll_region: OnceCell<MemoryRegion>,

    /// The metadata associated with the DLL
    metadata_layout: OnceCell<MetadataLayout>,

    /// The location of the pointer to the MethodTableLookup, and a
    /// bitmask defining flags that may be set on each method table
    /// pointer.
    type_def_table_info: OnceCell<(UnpackedValue<Pointer>, usize)>,

    /// The lookup for MethodTables defined in the module.
    type_def_table: OnceCell<MethodTableLookup>,

    ptr_to_loaded_types: OnceCell<UnpackedValue<Option<Pointer>>>,

    /// The base pointer of static pointer to non-garbage-collected
    /// static values.  These values are relative to the
    /// DomainLocalModule struct, a pointer to which is in the Module.
    ///
    /// For .NET 9.0 onward, this will need to be updated to find
    /// statics relative to the MethodTable.
    /// https://github.com/dotnet/runtime/commit/eb8f54d9 (2024-06-12)
    base_ptr_of_non_gc_statics: OnceCell<UnpackedValue<Pointer>>,

    /// The base pointer of static pointers to garbage-collected
    /// static values.  These values are relative to an allocation
    /// owned by the DomainLocalModule.
    base_ptr_of_gc_statics: OnceCell<UnpackedValue<Pointer>>,
}

pub struct MethodTableLookup {
    pub location: Range<Pointer>,
    pub method_tables: Vec<Range<Pointer>>,
}

pub struct LoadedParamTypes {
    bytes: OwnedBytes,
}

struct LoadedParamTypeEntry {
    bytes: OwnedBytes,
}

impl RuntimeModule {
    // TODO: Better locate/lookup.
    //
    // For the entry-point DLL, can located it as
    //
    // static AppDomain::m_pTheAppDomain
    //    -> m_pRootAssembly
    //    -> m_pManifest
    //
    // Similar steps could be done from `static
    // SystemDomain::m_pSystemDomain` to locate the system module.
    // The only problem is that the these static variables are
    // internal symbols, and only have ELF symbols in the
    // `libcoreclr.so.dbg` file.
    //
    // The `g_dacTable` does have its symbol externally exposed, even
    // in release builds.  However, interpreting it would be very
    // version-dependent and rather painful with the debug symbols.
    // And if I do have the debug symbols, why not just use them to
    // jump straight to the `m_pTheAppDomain` and `m_pSystemDomain`
    // statics directly?

    /// Given the unpacked Metadata from a DLL, attempt to locate its
    /// runtime representation.
    ///
    /// * metadata: The unpacked metadata for the DLL to be located.
    ///
    /// * reader: The memory reader to inspect the running subprocess.
    ///
    /// Returns a HashMap containing the location of each
    /// RuntimeModule that could be identified.  This may not include
    /// all modfules in the input `metadata`, as modules that don't
    /// define any methods tend not to be identifiable.  For those,
    /// see `locate_by_vtable`.
    pub fn locate_by_metadata(
        metadata: &[Metadata],
        reader: &MemoryReader,
    ) -> Vec<Option<TypedPointer<RuntimeModule>>> {
        // Pointers to IL method definitions in the loaded DLL.  These
        // are used to identify pointers to the module, which I
        // believe to be held by the `ILStubLinker`.  For each method,
        // there's an `ILStubLinker` that holds both a pointer to the
        // IL method definition, adjacent a pointer to the `Module`
        // that owns it.
        let dll_method_defs: HashMap<Pointer, usize> = metadata
            .iter()
            .enumerate()
            .flat_map(|(i, metadata)| {
                metadata
                    .method_def_table()
                    .iter_rows()
                    .filter_map(|method_def| {
                        method_def.cil_method().ok().flatten()
                    })
                    .filter_map(|cil_method| cil_method.body_range().ok())
                    .map(move |method_body| (method_body.start, i))
            })
            .collect();

        // Hashing the Pointer values to check if they're located in
        // `dll_method_defs` ends up being the slowest part of this
        // function.  Filtering out values that are outside of the
        // range of `dll_method_defs` gives a ~3x performance
        // improvement for this function, by skipping the hash
        // altogether.
        let method_def_range: Range<Pointer> = dll_method_defs.keys().copied()
            .minmax()
            .into_option()
            .map(|(a, b)| a..b)
            .unwrap_or_else(|| Pointer::null()..Pointer::null());

        let mut counter_lookups: Vec<HashMap<Pointer, usize>> =
            vec![HashMap::new(); metadata.len()];

        reader
            .iter_regions()
            .filter(|region| {
                region.is_readable
                    && region.is_writable
                    && !region.is_shared_memory
            })
            .filter_map(|region| region.read(reader).ok())
            .flat_map(|region| {
                region
                    .into_iter_as_pointers()
                    .map(|mem_value| mem_value.value)
                    .tuple_windows()
            })
            .filter(|(_, il_ptr)| method_def_range.contains(il_ptr))
            .for_each(|(module_ptr, il_ptr)| {
                // The pattern-matching above is pretty reliable at finding
                // the most common location.  With about 17k methods in
                // StardewValley.dll, there were 243 unique pairs of
                // `(module_ptr, il_ptr)`, one of which occurred
                // over 500 times.  No other pair occurred more than 3 times.
                //
                // I suppose I could further filter the `module_ptr` by values
                // that point to a pointer, which itself points to
                // `libcoreclr.so`, but that doesn't seem necessary at the
                // moment.

                if let Some(index) = dll_method_defs.get(&il_ptr) {
                    if reader.is_valid_ptr(module_ptr) {
                        *counter_lookups[*index]
                            .entry(module_ptr)
                            .or_insert(0) += 1;
                    }
                }
            });

        counter_lookups
            .into_iter()
            .map(|counter_lookup| {
                counter_lookup
                    .into_iter()
                    .max_by_key(|(_, counts)| *counts)
                    .map(|(ptr, _)| ptr.into())
            })
            .collect()
    }

    pub fn locate_by_vtable(
        metadata: &[Metadata],
        module_vtable: Pointer,
        reader: &MemoryReader,
    ) -> Result<Vec<Option<TypedPointer<RuntimeModule>>>, Error> {
        // However, that isn't sufficient for all modules.  Modules
        // aren't required to have any method definitions, and
        // `System.Runtime` in particular can't be located by this
        // method.  Instead, we can search for a pointer to the vtable
        // for Module, followed by a pointer to the name of the module.
        //
        // This is only possible if we already know the location of
        // the vtable.  With the debug symbols in `libcoreclr.so.dbg`,
        // this could be identified as the `_ZTV6Module` symbol.
        // However, as long as I'm being masochistic by assuming I
        // won't always have access to the debug symbols, the vtable
        // is only available after first identifying a module through
        // the first method.
        let module_name_location: Vec<Pointer> = metadata
            .iter()
            .map(|metadata| {
                metadata
                    .assembly_table()
                    .iter_rows()
                    .exactly_one()
                    .unwrap_or_else(|_| {
                        panic!("Each .DLL should define exactly one Assembly")
                    })
                    .name_location()
            })
            .collect::<Result<Vec<_>, _>>()?;

        #[derive(Clone)]
        enum NumFound {
            Zero,
            One(Pointer),
            Many,
        }
        let mut located = vec![NumFound::Zero; module_name_location.len()];

        reader
            .iter_regions()
            .filter(|region| {
                region.is_readable
                    && region.is_writable
                    && !region.is_shared_memory
            })
            .filter_map(|region| region.read(reader).ok())
            .flat_map(|region| region.into_iter_as_pointers().tuple_windows())
            .filter(|(a, _)| a.value == module_vtable)
            .for_each(|(a, b)| {
                for (name_ptr, out) in
                    module_name_location.iter().zip(located.iter_mut())
                {
                    if b.value == *name_ptr {
                        *out = match out {
                            NumFound::Zero => NumFound::One(a.location),
                            _ => NumFound::Many,
                        };
                    }
                }
            });

        let output = located
            .into_iter()
            .map(|out| match out {
                NumFound::One(ptr) => Some(ptr.into()),
                _ => None,
            })
            .collect();

        Ok(output)
    }

    /// Given the unpacked Metadata from a DLL, attempt to locate its
    /// runtime representation.
    ///
    /// * metadata: The unpacked metadata for the DLL to be located.
    ///
    /// * module_vtable_loc: The location of the module's vtable, if
    /// present.
    ///
    /// * reader: The memory reader to inspect the running subprocess.
    pub fn locate(
        metadata: Metadata,
        module_vtable_loc: Option<Pointer>,
        reader: impl Borrow<MemoryReader>,
    ) -> Result<TypedPointer<Self>, Error> {
        let reader = reader.borrow();

        let opt_ptr_vec: Vec<Option<TypedPointer<Self>>> =
            if let Some(module_vtable_loc) = module_vtable_loc {
                Self::locate_by_vtable(&[metadata], module_vtable_loc, reader)?
            } else {
                Self::locate_by_metadata(&[metadata], reader)
            };
        opt_ptr_vec[0].ok_or_else(|| {
            let metadata_start = metadata.ptr_range().start;
            let name = reader
                .iter_regions()
                .find(|region| region.contains(metadata_start))
                .map(|region| region.short_name())
                .unwrap_or("(unknown)")
                .to_string();
            Error::ModulePointerNotFound(name)
        })
    }

    pub fn new(location: Pointer, layout: Option<RuntimeModuleLayout>) -> Self {
        let layout_cell = OnceCell::new();
        if let Some(layout) = layout {
            layout_cell
                .set(layout)
                .expect("First set of OnceCell is guaranteed success.");
        }

        Self {
            location,
            layout: layout_cell,
            vtable_location: Default::default(),
            image_ptr: Default::default(),
            dll_region_info: Default::default(),
            dll_region: Default::default(),
            metadata_layout: Default::default(),
            type_def_table_info: Default::default(),
            type_def_table: Default::default(),
            ptr_to_loaded_types: Default::default(),
            base_ptr_of_non_gc_statics: Default::default(),
            base_ptr_of_gc_statics: Default::default(),
        }
    }

    pub fn infer_layout(
        modules: &[Self],
        reader: &MemoryReader,
    ) -> Result<RuntimeModuleLayout, Error> {
        // The extent of valid non-null pointers.  Used to quickly
        // exclude most invalid pointers.
        let valid_region: Range<Pointer> = reader
            .iter_regions()
            .map(|region| region.address_range())
            .reduce(|a, b| {
                let start = a.start.min(b.start);
                let end = a.end.max(b.end);
                start..end
            })
            .unwrap();

        let is_valid_ptr = |ptr: Pointer| -> bool {
            ptr.is_null() || valid_region.contains(&ptr)
        };

        const NBYTES: usize = 4096;

        let bytes = modules
            .iter()
            .map(|module| {
                reader.read_bytes(module.location..module.location + NBYTES)
            })
            .collect::<Result<Vec<_>, _>>()?;
        let num_type_defs = modules
            .iter()
            .map(|module| {
                module
                    .metadata(reader)
                    .map(|metadata| metadata.type_def_table().num_rows())
            })
            .collect::<Result<Vec<_>, _>>()?;

        let is_valid_type_def_offset = |offset: usize| -> bool {
            if offset + Pointer::SIZE * 4 > NBYTES {
                return false;
            }
            bytes
                .iter()
                .zip(num_type_defs.iter())
                .and_all(|(bytes, num_type_defs)| -> Result<_, Error> {
                    let p_next: Pointer = bytes
                        .subrange(
                            offset + Pointer::SIZE * 0
                                ..offset + Pointer::SIZE,
                        )
                        .unpack()?;
                    let p_table: Pointer = bytes
                        .subrange(
                            offset + Pointer::SIZE
                                ..offset + Pointer::SIZE * 2,
                        )
                        .unpack()?;
                    let dw_count: u32 = bytes
                        .subrange(
                            offset + Pointer::SIZE * 2
                                ..offset + Pointer::SIZE * 2 + 4,
                        )
                        .unpack()?;
                    let dw_count = dw_count as usize;
                    let supported_flags: Pointer = bytes
                        .subrange(
                            offset + Pointer::SIZE * 3
                                ..offset + Pointer::SIZE * 4,
                        )
                        .unpack()?;
                    let supported_flags = supported_flags.as_usize();

                    Ok(p_next == Pointer::null()
                        && is_valid_ptr(p_table)
                        && dw_count == num_type_defs + 1
                        && supported_flags < 8)
                })
                .unwrap_or(false)
        };

        let offset_to_type_def_table = (0..NBYTES)
            .step_by(Pointer::SIZE)
            .find(|offset| is_valid_type_def_offset(*offset))
            .ok_or(Error::PointerToMethodTableTableNotFound)?;

        let is_valid_instantiated_generics_offset = |offset: usize| -> bool {
            if offset < Pointer::SIZE || offset + Pointer::SIZE > NBYTES {
                return false;
            }

            let mut at_least_one_pair_of_pointers = false;
            for bytes in bytes.iter() {
                let a: Pointer = bytes
                    .subrange(offset - Pointer::SIZE..offset)
                    .unpack()
                    .unwrap();
                let b: Pointer = bytes
                    .subrange(offset..offset + Pointer::SIZE)
                    .unpack()
                    .unwrap();

                if !is_valid_ptr(a) {
                    return false;
                }
                if !is_valid_ptr(b) {
                    return false;
                }

                if valid_region.contains(&a) && valid_region.contains(&b) {
                    at_least_one_pair_of_pointers = true;
                }
            }

            at_least_one_pair_of_pointers
        };

        let offset_to_instantiated_generics = (offset_to_type_def_table
            ..offset_to_type_def_table + 512.min(NBYTES))
            .step_by(Pointer::SIZE)
            .find(|offset| is_valid_instantiated_generics_offset(*offset))
            .ok_or(Error::PointerToInstantiatedGenericsNotFound)?;

        let is_valid_base_address_of_non_gc_static_values =
            |offset: usize| -> bool {
                if offset + Pointer::SIZE * 5 > NBYTES {
                    return false;
                }

                bytes
                    .iter()
                    .zip(num_type_defs.iter().cloned())
                    .and_all(|(bytes, num_type_defs)| -> Result<_, Error> {
                        let domain_local_module: Pointer =
                            bytes.subrange(offset..offset + 8).unpack()?;
                        let module_index: u64 =
                            bytes.subrange(offset + 8..offset + 16).unpack()?;
                        let regular_statics_offsets: Pointer = bytes
                            .subrange(offset + 16..offset + 24)
                            .unpack()?;
                        let thread_statics_offsets: Pointer = bytes
                            .subrange(offset + 24..offset + 32)
                            .unpack()?;
                        let max_rid_statics_allocated: u32 = bytes
                            .subrange(offset + 32..offset + 36)
                            .unpack()?;
                        let max_rid_statics_allocated =
                            max_rid_statics_allocated as usize;

                        Ok(valid_region.contains(&domain_local_module)
                            && is_valid_ptr(regular_statics_offsets)
                            && is_valid_ptr(thread_statics_offsets)
                            && max_rid_statics_allocated == num_type_defs
                           // The condition on module_index isn't
                           // technically a requirement, but this is a
                           // 64-bit value that gets incremented for
                           // each module that is loaded.  It's
                           // unlikely to have several thousand DLLs
                           // loaded at once, so I might as well
                           // include this as part of the condition.
                        && module_index < 16384)
                    })
                    .unwrap_or(false)
            };

        let offset_to_base_address_of_non_gc_static_values =
            (offset_to_instantiated_generics..NBYTES)
                .step_by(Pointer::SIZE)
                .find(|offset| {
                    is_valid_base_address_of_non_gc_static_values(*offset)
                })
                .ok_or(Error::PointerToDomainLocalModuleNotFound)?;

        Ok(RuntimeModuleLayout {
            offset_to_type_def_table,
            offset_to_instantiated_generics,
            offset_to_base_address_of_non_gc_static_values,
        })
    }

    pub fn get_layout(
        &self,
        reader: impl Borrow<MemoryReader>,
    ) -> Result<&RuntimeModuleLayout, Error> {
        self.layout.or_try_init(|| {
            // The layout of the Module varies by .NET version, but should
            // be less than 4kB for each.  If I don't find each pointer by
            // then, then something else is probably wrong.
            let reader = reader.borrow();
            Self::infer_layout(std::slice::from_ref(self), reader)
        })
    }

    pub fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
        reader: impl Borrow<MemoryReader>,
    ) -> Result<(), Error> {
        let reader = reader.borrow();
        let name = self.name(reader)?;

        annotator
            .group(self.location..self.location + 1560)
            .name(format!("Module {name}"));

        annotator
            .value(self.base_ptr_of_gc_statics_unpacked(reader)?)
            .name(format!("Base ptr of GC statics, Module {name}"));
        annotator
            .value(self.base_ptr_of_non_gc_statics_unpacked(reader)?)
            .name(format!("Base ptr of non-GC statics, Module {name}"));

        annotator
            .value(self.type_def_table_info_unpacked(reader)?.0)
            .name(format!("TypeDefToMethodTable, Module {name}"));

        annotator
            .range(self.type_def_table(reader)?.location.clone())
            .name("TypeDefToMethodDef table".to_string());

        annotator
            .opt_value(self.ptr_to_loaded_types_unpacked(reader)?)
            .name(format!("Available param types, Module {name}"));

        if let Some(loaded_types) = self.loaded_types(reader)? {
            loaded_types.collect_annotations(annotator, reader)?;
        }

        Ok(())
    }

    pub fn iter_method_table_pointers<'a>(
        &'a self,
        reader: impl Borrow<MemoryReader>,
    ) -> Result<impl Iterator<Item = TypedPointer<MethodTable>> + 'a, Error>
    {
        let reader = reader.borrow();
        Ok(self.type_def_table(reader)?.iter_table_pointers())
    }

    pub fn iter_method_tables<'a>(
        &'a self,
        reader: &'a MemoryReader,
    ) -> Result<impl Iterator<Item = Result<MethodTable, Error>> + 'a, Error>
    {
        // TODO: Remove this method, since it re-reads every method
        // table each time.  Instead, should use
        // `iter_method_table_pointers`, followed by
        // `CachedReader.method_table`.
        Ok(self.type_def_table(reader)?.iter_tables(reader))
    }

    pub fn vtable_location(
        &self,
        reader: &MemoryReader,
    ) -> Result<Pointer, Error> {
        self.vtable_location
            .or_try_init(|| Ok(reader.read_byte_array(self.location)?.into()))
            .copied()
    }

    pub fn image_ptr(&self, reader: &MemoryReader) -> Result<Pointer, Error> {
        self.image_ptr
            .or_try_init(|| {
                let pe_file_ptr: Pointer =
                    reader.read_byte_array(self.location + 16)?.into();
                let pe_image_ptr: Pointer =
                    reader.read_byte_array(pe_file_ptr + 8)?.into();
                let pe_image_layout_ptr: Pointer = (0..3)
                    .map(|i| pe_image_ptr + 96 + Pointer::SIZE * i)
                    .map(|ptr| -> Result<Pointer, _> {
                        reader.read_byte_array(ptr).map(Into::into)
                    })
                    .find(|res| match res {
                        Ok(ptr) => !ptr.is_null(),
                        Err(_) => true,
                    })
                    .ok_or(Error::DLLPointerNotFoundFromModule)??;

                // In most cases, the pointers at offsets 8/24/32 will all
                // let the DLL region be found.  However, the PE pointers
                // at offset 8 and 24 may point to a memmap that only
                // contains the start of the file.
                let image_ptr: Pointer =
                    reader.read_byte_array(pe_image_layout_ptr + 32)?.into();

                Ok(image_ptr)
            })
            .copied()
    }

    pub fn dll_region_info(
        &self,
        reader: &MemoryReader,
    ) -> Result<&MemoryMapRegion, Error> {
        self.dll_region_info.or_try_init(|| {
            let image_ptr = self.image_ptr(reader)?;

            reader
                .iter_regions()
                .filter(|region| {
                    region.contains(image_ptr) && region.file_offset() == 0
                })
                .max_by_key(|region| region.size_bytes())
                .cloned()
                .ok_or(Error::RegionForDLLNotFoundFromPointer(image_ptr))
        })
    }

    pub fn name(
        &self,
        reader: impl Borrow<MemoryReader>,
    ) -> Result<&str, Error> {
        let reader = reader.borrow();
        let info = self.dll_region_info(reader)?;
        let filename = info.short_name();
        let name = filename.trim_end_matches(".dll");
        Ok(name)
    }

    pub fn dll_region(
        &self,
        reader: &MemoryReader,
    ) -> Result<&MemoryRegion, Error> {
        self.dll_region.or_try_init(|| {
            let info = self.dll_region_info(reader)?;
            let region = info.read(reader)?;
            Ok(region)
        })
    }

    pub fn metadata_layout(
        &self,
        reader: &MemoryReader,
    ) -> Result<&MetadataLayout, Error> {
        self.metadata_layout.or_try_init(|| {
            let dll_region = self.dll_region(reader)?;
            let layout = dll_unpacker::unpack_metadata_layout(dll_region)?;
            Ok(layout)
        })
    }

    pub fn type_def_table_info_unpacked(
        &self,
        reader: impl Borrow<MemoryReader>,
    ) -> Result<(UnpackedValue<Pointer>, usize), Error> {
        let reader = reader.borrow();

        self.type_def_table_info
            .or_try_init(|| {
                let layout = self.get_layout(reader)?;
                let start = self.location + layout.offset_to_type_def_table;

                let bytes = reader.read_bytes(start..start + 32)?;

                let p_table: UnpackedValue<Pointer> =
                    bytes.subrange(8..16).unpack()?;
                let supported_flags: Pointer =
                    bytes.subrange(24..32).unpack()?;
                let supported_flags = supported_flags.as_usize();

                Ok((p_table, supported_flags))
            })
            .cloned()
    }

    fn type_def_table_info(
        &self,
        reader: &MemoryReader,
    ) -> Result<(Pointer, usize), Error> {
        let (ptr, bitmask) = self.type_def_table_info_unpacked(reader)?;
        let ptr = ptr.value();
        Ok((ptr, bitmask))
    }

    pub fn ptr_to_type_def_table(
        &self,
        reader: &MemoryReader,
    ) -> Result<Pointer, Error> {
        self.type_def_table_info(reader).map(|(ptr, _)| ptr)
    }

    pub fn get_type_def(
        &self,
        index: MetadataTableIndex<TypeDef>,
        reader: impl Borrow<MemoryReader>,
    ) -> Result<Option<TypedPointer<MethodTable>>, Error> {
        let lookup = self.type_def_table(reader)?;
        let method_table = lookup.get_ptr(index);
        Ok(method_table)
    }

    pub fn type_def_table(
        &self,
        reader: impl Borrow<MemoryReader>,
    ) -> Result<&MethodTableLookup, Error> {
        let reader = reader.borrow();

        self.type_def_table.or_try_init(|| {
            // TODO: Does this cache too much and too early?  If a
            // type is not yet loaded, it would find a NULL pointer
            // and save it in the MethodTableLookup.  If a type is
            // loaded later, this caching would still return a NULL
            // pointer.
            //
            // Maybe the MethodTableLookup should be removed
            // altogether, with the remote pointer re-read when this
            // function is called.  Caching would still be handled at
            // the StaticValueCache/CachedReader level.
            let num_type_defs =
                self.metadata(reader)?.type_def_table().num_rows();

            let (ptr_to_type_def_table, supported_flags) =
                self.type_def_table_info(reader)?;

            let ptr_to_method_tables = {
                let nbytes = (num_type_defs + 1) * Pointer::SIZE;
                ptr_to_type_def_table..ptr_to_type_def_table + nbytes
            };

            let method_tables = reader
                .read_bytes(ptr_to_method_tables.clone())?
                .into_iter()
                .iter_as::<[u8; Pointer::SIZE]>()
                .map(|arr| -> Pointer {
                    let value = usize::from_ne_bytes(arr);
                    let value = value & !supported_flags;
                    value.into()
                })
                .map(|ptr| ptr..ptr + MethodTable::SIZE)
                .collect();

            let method_table_lookup = MethodTableLookup {
                location: ptr_to_method_tables,
                method_tables,
            };

            Ok(method_table_lookup)
        })
    }

    pub fn ptr_to_loaded_types_unpacked(
        &self,
        reader: impl Borrow<MemoryReader>,
    ) -> Result<UnpackedValue<Option<Pointer>>, Error> {
        self.ptr_to_loaded_types
            .or_try_init(|| -> Result<UnpackedValue<Option<Pointer>>, Error> {
                let reader = reader.borrow();
                let layout = self.get_layout(reader)?;
                let start =
                    self.location + layout.offset_to_instantiated_generics;
                let bytes = reader.read_bytes(start..start + Pointer::SIZE)?;
                let unpacked_ptr = bytes.subrange(..).unpack()?;
                Ok(unpacked_ptr)
            })
            .cloned()
    }

    pub fn ptr_to_loaded_types(
        &self,
        reader: impl Borrow<MemoryReader>,
    ) -> Result<Option<Pointer>, Error> {
        let unpacked_opt = self.ptr_to_loaded_types_unpacked(reader)?;
        let opt_ptr = unpacked_opt.value();
        Ok(opt_ptr)
    }

    pub fn loaded_types(
        &self,
        reader: impl Borrow<MemoryReader>,
    ) -> Result<Option<LoadedParamTypes>, Error> {
        let reader = reader.borrow();

        let Some(location) = self.ptr_to_loaded_types(reader)? else {
            return Ok(None);
        };

        let bytes =
            reader.read_bytes(location..location + LoadedParamTypes::SIZE)?;
        let loaded_types = LoadedParamTypes::new(bytes);
        Ok(Some(loaded_types))
    }

    pub fn base_ptr_of_non_gc_statics_unpacked(
        &self,
        reader: impl Borrow<MemoryReader>,
    ) -> Result<UnpackedValue<Pointer>, Error> {
        self.base_ptr_of_non_gc_statics
            .or_try_init(|| {
                let reader = reader.borrow();
                let layout = self.get_layout(reader)?;
                let start = self.location
                    + layout.offset_to_base_address_of_non_gc_static_values;
                let ptr: UnpackedValue<Pointer> = reader
                    .read_bytes(start..start + Pointer::SIZE)?
                    .subrange(..)
                    .unpack()?;
                Ok(ptr)
            })
            .copied()
    }

    pub fn base_ptr_of_non_gc_statics(
        &self,
        reader: impl Borrow<MemoryReader>,
    ) -> Result<Pointer, Error> {
        Ok(self.base_ptr_of_non_gc_statics_unpacked(reader)?.value())
    }

    pub fn base_ptr_of_gc_statics_unpacked(
        &self,
        reader: impl Borrow<MemoryReader>,
    ) -> Result<UnpackedValue<Pointer>, Error> {
        let reader = reader.borrow();
        self.base_ptr_of_gc_statics
            .or_try_init(|| {
                let base_ptr_of_non_gc_statics =
                    self.base_ptr_of_non_gc_statics(reader)?;
                let start = base_ptr_of_non_gc_statics + 32;
                let base_ptr_of_gc_statics: Pointer =
                    reader.read_byte_array(start)?.into();

                Ok(UnpackedValue::new(
                    start..start + Pointer::SIZE,
                    base_ptr_of_gc_statics,
                ))
            })
            .copied()
    }

    pub fn base_ptr_of_gc_statics(
        &self,
        reader: impl Borrow<MemoryReader>,
    ) -> Result<Pointer, Error> {
        Ok(self.base_ptr_of_gc_statics_unpacked(reader)?.value())
    }

    pub fn metadata<'a>(
        &'a self,
        reader: impl Borrow<MemoryReader>,
    ) -> Result<Metadata<'a>, Error> {
        let reader = reader.borrow();
        let layout = self.metadata_layout(reader)?;
        let dll_region = self.dll_region(reader)?;
        let metadata = layout.metadata(dll_region);
        Ok(metadata)
    }
}

impl MethodTableLookup {
    pub fn location_of_method_table_pointer(
        &self,
        index: MetadataTableIndex<TypeDef>,
    ) -> Range<Pointer> {
        let index: usize = index.into();
        let index = index + 1;
        let ptr_start = self.location.start + index * Pointer::SIZE;
        ptr_start..ptr_start + Pointer::SIZE
    }

    pub fn iter_table_pointers(
        &self,
    ) -> impl Iterator<Item = TypedPointer<MethodTable>> + '_ {
        self.method_tables
            .iter()
            .map(|range| -> TypedPointer<MethodTable> { range.start.into() })
            .filter(|ptr| !ptr.is_null())
    }

    fn iter_tables<'a>(
        &'a self,
        reader: &'a MemoryReader,
    ) -> impl Iterator<Item = Result<MethodTable, Error>> + 'a {
        self.method_tables
            .iter().filter(|&ptr| !ptr.start.is_null()).cloned()
            .map(move |location| {
                let bytes = reader.read_bytes(location)?;
                Ok(MethodTable { bytes })
            })
    }

    pub fn get(
        &self,
        index: MetadataTableIndex<TypeDef>,
        reader: impl Borrow<MemoryReader>,
    ) -> Result<MethodTable, Error> {
        let reader = reader.borrow();

        let byte_range = self[index].clone();
        let bytes = reader.read_bytes(byte_range)?;
        Ok(MethodTable { bytes })
    }

    pub fn get_ptr(
        &self,
        index: MetadataTableIndex<TypeDef>,
    ) -> Option<TypedPointer<MethodTable>> {
        let ptr: TypedPointer<MethodTable> = self[index].start.into();
        ptr.as_non_null()
    }
}

impl std::ops::Index<MetadataTableIndex<TypeDef>> for MethodTableLookup {
    type Output = Range<Pointer>;

    fn index(&self, index: MetadataTableIndex<TypeDef>) -> &Self::Output {
        let index: usize = index.into();
        &self.method_tables[index + 1]
    }
}

impl LoadedParamTypes {
    const SIZE: usize = 40;

    fn new(bytes: OwnedBytes) -> Self {
        Self { bytes }
    }

    unpack_fields! {
        module_ptr: {TypedPointer<RuntimeModule>, 0..8},
        heap_ptr: {Pointer, 8..16},
        buckets_ptr_ptr: {Option<Pointer>, 16..24},
        raw_num_buckets: {u32, 24..28},
        raw_num_entries: {u32, 28..32},
    }

    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
        reader: &MemoryReader,
    ) -> Result<(), Error> {
        annotator
            .value(self.raw_num_buckets_unpacked())
            .name("Num buckets");
        annotator
            .value(self.raw_num_entries_unpacked())
            .name("Num entries");
        annotator
            .opt_value(self.buckets_ptr_ptr_unpacked())
            .name("Ptr to ptr to buckets");

        self.iter_entries(reader)?.try_for_each(
            |res_entry| -> Result<_, Error> {
                let entry = res_entry?;
                entry.collect_annotations(annotator)
            },
        )?;

        Ok(())
    }

    fn num_buckets(&self) -> usize {
        self.raw_num_buckets() as usize
    }

    pub fn num_entries(&self) -> usize {
        self.raw_num_entries() as usize
    }

    fn iter_bucket_ptrs(
        &self,
        reader: &MemoryReader,
    ) -> Result<
        impl Iterator<Item = Option<TypedPointer<LoadedParamTypeEntry>>>,
        Error,
    > {
        let opt_bytes = self
            .buckets_ptr_ptr()
            .map(|ptr| {
                assert!(ptr.as_usize() % Pointer::SIZE == 0);
                let size = self.num_buckets() * Pointer::SIZE;
                reader.read_bytes(ptr..ptr + size)
            })
            .transpose()?;

        let iter = opt_bytes.into_iter().flat_map(|bytes| {
            bytes.into_iter().iter_as().map(
                    |chunk: [u8; Pointer::SIZE]| -> Option<
                        TypedPointer<LoadedParamTypeEntry>,
                    > {
                        let ptr: Pointer = chunk.into();
                        ptr.as_non_null().map(|ptr| {
                            assert!(ptr.as_usize() % Pointer::SIZE == 0);
                            ptr.into()
                        })
                    },
                )
        });

        Ok(iter)
    }

    fn iter_entries<'a>(
        &self,
        reader: &'a MemoryReader,
    ) -> Result<
        impl Iterator<Item = Result<LoadedParamTypeEntry, Error>> + 'a,
        Error,
    > {
        let iter = self
            .iter_bucket_ptrs(reader)?
            .flatten()
            .flat_map(move |ptr| {
                std::iter::successors(Some(ptr.read(reader)), |res_entry| {
                    match res_entry {
                        Ok(entry) => {
                            entry.next_entry().map(|next| next.read(reader))
                        }
                        Err(_) => None,
                    }
                })
            });

        Ok(iter)
    }

    pub fn iter_method_tables<'a>(
        &self,
        reader: &'a MemoryReader,
    ) -> Result<
        impl Iterator<Item = Result<TypedPointer<TypeHandle>, Error>> + 'a,
        Error,
    > {
        self.iter_entries(reader)
            .map(|iter| iter.map_ok(|entry| entry.type_handle()))
    }
}

impl ReadTypedPointer for LoadedParamTypeEntry {
    fn read_typed_ptr(
        ptr: Pointer,
        reader: &MemoryReader,
    ) -> Result<Self, Error> {
        let bytes = reader.read_bytes(ptr..ptr + Self::SIZE)?;
        Ok(Self { bytes })
    }
}

impl LoadedParamTypeEntry {
    // SIZE = 24, at least for 64-bit platforms
    const SIZE: usize = (Pointer::SIZE * 2 + 4).next_multiple_of(Pointer::SIZE);

    unpack_fields! {
        type_handle: {TypedPointer<TypeHandle>, 0..Pointer::SIZE},
        next_entry: {Option<TypedPointer<Self>>, Pointer::SIZE..Pointer::SIZE*2},
        // hash: {u32, Pointer::SIZE*2..Pointer::SIZE*2+4},
    }

    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.type_handle_unpacked())
            .name("Method table");

        annotator
            .opt_value(self.next_entry_unpacked())
            .name("Next entry");

        Ok(())
    }
}

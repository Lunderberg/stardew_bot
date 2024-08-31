use std::collections::HashSet;

use dll_unpacker::Metadata;
use itertools::Itertools as _;
use memory_reader::{extensions::*, MemoryRegion};
use memory_reader::{MemoryReader, Pointer};

use crate::{Error, MethodTable, MethodTableLookup};

/// Contains locations of key structures within the runtime
/// representation of a single CLR DLL.
pub struct RuntimeModule {
    /// Location of the Module object
    pub location: Pointer,

    /// The DLL corresponding to this Module.
    pub dll_region: MemoryRegion,

    /// The location of the pointer to the MethodTableLookup.
    pub ptr_to_table_of_method_tables: Pointer,

    /// The lookup for the MethodTables
    pub method_table_lookup: MethodTableLookup,

    /// The base pointer of static pointer to non-garbage-collected
    /// static values.  These values are relative to the
    /// DomainLocalModule struct, a pointer to which is in the Module.
    ///
    /// For .NET 9.0 onward, this will need to be updated to find
    /// statics relative to the MethodTable.
    /// https://github.com/dotnet/runtime/commit/eb8f54d9 (2024-06-12)
    pub base_ptr_of_non_gc_statics: Pointer,

    /// The base pointer of static pointers to garbage-collected
    /// static values.  These values are relative to an allocation
    /// owned by the DomainLocalModule.
    pub base_ptr_of_gc_statics: Pointer,
}

impl RuntimeModule {
    /// Given the unpacked Metadata from a DLL, attempt to locate its
    /// runtime representation.
    pub fn locate(
        metadata: &Metadata,
        reader: &MemoryReader,
    ) -> Result<Pointer, Error> {
        // Pointers to IL method definitions in the loaded DLL.
        let dll_method_def: HashSet<Pointer> = metadata
            .method_def_table()?
            .iter_rows()
            .filter_map(|method_def| method_def.cil_method().ok().flatten())
            .filter_map(|cil_method| cil_method.body_range().ok())
            .map(|method_body| method_body.start)
            .collect();

        reader
            .regions
            .iter()
            .filter(|region| {
                region.is_readable
                    && region.is_writable
                    && !region.is_shared_memory
            })
            .flat_map(|region| {
                region
                    .read()
                    .unwrap()
                    .into_iter_as_pointers()
                    .map(|mem_value| mem_value.value)
                    .tuple_windows()
            })
            .filter(|(module_ptr, il_ptr)| {
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
                dll_method_def.contains(il_ptr)
                    && reader.is_valid_ptr(*module_ptr)
            })
            .map(|(module_ptr, _)| module_ptr)
            .counts()
            .into_iter()
            .max_by_key(|(_, counts)| *counts)
            .map(|(value, _)| value)
            .ok_or(Error::ModulePointerNotFound)
    }

    pub fn read(
        location: Pointer,
        reader: &MemoryReader,
    ) -> Result<Self, Error> {
        let image_ptr = {
            let pe_file_ptr: Pointer =
                reader.read_byte_array(location + 16)?.into();
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
            let image_ptr: Pointer =
                reader.read_byte_array(pe_image_layout_ptr + 8)?.into();

            image_ptr
        };

        // TODO: Allow a caller to pass ownership of an existing DLL
        // region.
        let dll_region = reader
            .find_region(|region| {
                region.is_readable
                    && !region.is_writable
                    && !region.is_executable
                    && region.is_shared_memory
                    && region.contains(image_ptr)
            })
            .ok_or(Error::DLLPointerNotFoundFromModule)?
            .read()?;
        let dll_info = dll_unpacker::DLLUnpacker::new(&dll_region);
        let metadata = dll_info.metadata()?;

        // The layout of the Module varies by .NET version, but should
        // be less than 4kB for each.  If I don't find each pointer by
        // then, then something else is probably wrong.
        let bytes = reader.read_bytes(location, 4096)?;

        // The exact layout of the Module class varies.  The goal is
        // to find the `m_TypeDefToMethodTableMap` member,
        //
        // struct LookupMap {
        //     pNext: Pointer,
        //     pTable: Pointer,
        //     dwCount: i32,
        //     supportedFlags: u64,
        // }
        //
        // * The `pNext` pointer is used to point to the next
        //   `LookupMap`.  Since the total number of classes in the
        //   DLL is known from the metadata, they are all stored in a
        //   single allocation, and this pointer will always be NULL.
        //
        // * The `dwCount` is the number of elements in the lookup
        //   table.  This will be equal to
        //   `table_sizes.num_rows[MetadataTableKind::TypeDef] + 1`.
        //   Presumably, the extra element is to allow metadata
        //   indices (which use zero to indicate an absent value) to
        //   be used directly as lookup indices.
        //
        //   This may need to be changed for dynamic modules.
        //
        // * The `supportedFlags` is a bitmask, with flags stored of
        //   the low bits of `pTable`.  The exact value depends on the
        //   version, but it can still be used to identify the
        //   `LookupMap` as only the low bits can be set.  And we need
        //   the value anyways in order to know which bits to remove
        //   from `pTable`.
        //
        // Should be at byte 648 relative to the module
        // pointer, but may be different in each .NET version.

        let num_type_defs = metadata.type_def_table()?.num_rows();

        let (ptr_to_table_of_method_tables, supported_flags) = bytes
            .chunks_exact(8)
            .tuple_windows()
            .map(|(a, b, c, d)| {
                let p_next: Pointer = a.try_into().unwrap();
                let p_table: Pointer = b.try_into().unwrap();
                let dw_count =
                    u32::from_ne_bytes(c[..4].try_into().unwrap()) as usize;
                let supported_flags: usize =
                    usize::from_ne_bytes(d.try_into().unwrap());

                (p_next, p_table, dw_count, supported_flags)
            })
            .find(|(p_next, _, dw_count, supported_flags)| {
                *p_next == Pointer::null()
                    && *dw_count == num_type_defs + 1
                    && *supported_flags < 8
            })
            .map(|(_, p_table, _, supported_flags)| (p_table, supported_flags))
            .ok_or(Error::PointerToMethodTableTableNotFound)?;

        let ptr_to_method_tables = {
            let nbytes = (num_type_defs + 1) * Pointer::SIZE;
            ptr_to_table_of_method_tables
                ..ptr_to_table_of_method_tables + nbytes
        };

        let method_tables = reader
            .read_bytes(
                ptr_to_method_tables.start,
                ptr_to_method_tables.end - ptr_to_method_tables.start,
            )?
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

        let base_ptr_of_non_gc_statics = bytes
            .chunks_exact(8)
            .tuple_windows()
            .map(|(a, b, c, d, e)| {
                let domain_local_module: Pointer = a.try_into().unwrap();
                let module_index: u64 =
                    u64::from_ne_bytes(b.try_into().unwrap());
                let regular_statics_offsets: Pointer = c.try_into().unwrap();
                let thread_statics_offsets: Pointer = d.try_into().unwrap();
                let max_rid_statics_allocated =
                    u32::from_ne_bytes(e[..4].try_into().unwrap()) as usize;
                (
                    domain_local_module,
                    module_index,
                    regular_statics_offsets,
                    thread_statics_offsets,
                    max_rid_statics_allocated,
                )
            })
            .filter(
                |(
                    domain_local_module,
                    _,
                    regular_statics_offsets,
                    thread_statics_offsets,
                    max_rid_statics_allocated,
                )| {
                    reader.is_valid_ptr(*domain_local_module)
                        && (regular_statics_offsets.is_null()
                            || reader.is_valid_ptr(*regular_statics_offsets))
                        && (thread_statics_offsets.is_null()
                            || reader.is_valid_ptr(*thread_statics_offsets))
                        && *max_rid_statics_allocated == num_type_defs
                },
            )
            .filter(|(_, module_index, _, _, _)| {
                // Not technically a requirement, but this is a 64-bit
                // value that gets incremented for each module that is
                // loaded.  It's unlikely to have several thousand
                // DLLs loaded at once, so I might as well include
                // this as part of the condition.
                *module_index < 16384
            })
            .map(|(domain_local_module, _, _, _, _)| domain_local_module)
            .next()
            .expect("Could not find DomainLocalModule pointer");

        let base_ptr_of_gc_statics = reader
            .read_byte_array(base_ptr_of_non_gc_statics + 32)?
            .into();

        Ok(Self {
            location,
            dll_region,
            ptr_to_table_of_method_tables,
            method_table_lookup,
            base_ptr_of_non_gc_statics,
            base_ptr_of_gc_statics,
        })
    }

    pub fn iter_method_tables<'a>(
        &'a self,
        reader: &'a MemoryReader,
    ) -> impl Iterator<Item = Result<MethodTable, Error>> + 'a {
        self.method_table_lookup.iter_tables(reader)
    }

    pub fn metadata(&self) -> Result<Metadata, Error> {
        // TODO: Cache the information from the header (offset to CLR
        // metadata, heaps, ans MetadataTableSizes) so that it can be
        // quickly reconstructed.
        let dll_info = dll_unpacker::DLLUnpacker::new(&self.dll_region);
        let metadata = dll_info.metadata()?;
        Ok(metadata)
    }
}

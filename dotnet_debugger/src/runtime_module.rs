use std::collections::HashSet;

use dll_unpacker::Metadata;
use itertools::Itertools as _;
use memory_reader::extensions::*;
use memory_reader::{MemoryReader, Pointer};

use crate::{Error, MethodTable, MethodTableLookup};

/// Contains locations of key structures within the runtime
/// representation of a single CLR DLL.
pub struct RuntimeModule {
    /// Location of the Module object
    pub location: Pointer,

    /// The location of the
    pub ptr_to_table_of_method_tables: Pointer,

    /// The lookup for the MethodTables
    pub method_table_lookup: MethodTableLookup,
}

impl RuntimeModule {
    /// Given the unpacked Metadata from a DLL, attempt to locate its
    /// runtime representation.
    pub fn locate(
        reader: &MemoryReader,
        metadata: &Metadata,
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
                    && reader.find_containing_region(*module_ptr).is_some()
            })
            .map(|(module_ptr, _)| module_ptr)
            .counts()
            .into_iter()
            .max_by_key(|(_, counts)| *counts)
            .map(|(value, _)| value)
            .ok_or(Error::ModulePointerNotFound)
    }

    pub fn build(
        reader: &MemoryReader,
        metadata: &Metadata,
        location: Pointer,
    ) -> Result<Self, Error> {
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

        let num_type_defs = metadata.type_def_table()?.num_rows();

        let (ptr_to_table_of_method_tables, supported_flags) = reader
            .regions
            .iter()
            .find(|region| region.address_range().contains(&location))
            .unwrap()
            .read()?
            .into_iter_as_pointers_from(location)
            // If I haven't found it after 4 kB, then something else
            // is wrong.  Should be at byte 648 relative to the module
            // pointer, but may be different in each .NET version.
            .take(512)
            .tuple_windows()
            .map(|(a, b, c, d)| {
                let p_next: Pointer = a.value;
                let p_table: Pointer = b.value;
                let dw_count = c.value.as_usize() & ((1 << 32) - 1);
                let supported_flags: usize = d.value.as_usize();

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

        Ok(Self {
            location,
            ptr_to_table_of_method_tables,
            method_table_lookup,
        })
    }

    pub fn iter_method_tables<'a>(
        &'a self,
        reader: &'a MemoryReader,
    ) -> impl Iterator<Item = Result<MethodTable, Error>> + 'a {
        self.method_table_lookup.iter_tables(reader)
    }
}

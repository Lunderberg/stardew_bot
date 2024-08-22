use std::collections::HashSet;

use dll_unpacker::Metadata;
use itertools::Itertools as _;
use memory_reader::extensions::*;
use memory_reader::{MemoryReader, Pointer};

use crate::{Error, MethodTable, MethodTableLookup};

pub fn find_module_pointer<'a>(
    reader: &MemoryReader,
    metadata: &Metadata<'a>,
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
            region.is_readable && region.is_writable && !region.is_shared_memory
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

pub fn find_method_table_lookup<'a>(
    reader: &MemoryReader,
    metadata: &Metadata<'a>,
    module_ptr: Pointer,
) -> Result<(Pointer, MethodTableLookup), Error> {
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
        .find(|region| region.address_range().contains(&module_ptr))
        .unwrap()
        .read()?
        .into_iter_as_pointers_from(module_ptr)
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

    let location = {
        let nbytes = (num_type_defs + 1) * Pointer::SIZE;
        ptr_to_table_of_method_tables..ptr_to_table_of_method_tables + nbytes
    };

    let method_tables = reader
        .read_bytes(location.start, location.end - location.start)?
        .into_iter()
        .iter_as::<[u8; Pointer::SIZE]>()
        .map(|arr| -> Pointer {
            let value = usize::from_ne_bytes(arr);
            let value = value & !supported_flags;
            value.into()
        })
        .map(|ptr| ptr..ptr + MethodTable::SIZE)
        .collect();

    Ok((
        ptr_to_table_of_method_tables,
        MethodTableLookup {
            location,
            method_tables,
        },
    ))
}

pub fn find_object_instances<'a>(
    method_table: &MethodTable,
    reader: &'a MemoryReader,
) -> Result<impl Iterator<Item = Pointer> + 'a, Error> {
    let search_ptr = method_table.ptr_range().start;
    let has_finalizer = method_table.has_finalizer();

    let offsets_of_pointers: Vec<usize> = method_table
        .get_field_descriptions(reader)?
        .iter()
        .flatten()
        .filter(|field| !field.is_static())
        .filter(|field| {
            field.runtime_type().map(|ty| ty.is_ptr()).unwrap_or(false)
        })
        .map(|field| field.offset() as usize)
        .map(|offset| {
            // The offset is recorded relative to the location after
            // the MethodTable*, but it's more convenient to have the
            // offset relative to the start of the MethodTable*.
            offset + Pointer::SIZE
        })
        .sorted()
        .collect();

    let iter = reader
        .regions
        .iter()
        .filter(|region| {
            region.is_readable && region.is_writable && !region.is_shared_memory
        })
        .flat_map(|region| region.read().unwrap().into_iter_as_pointers())
        .filter(move |mem_value| {
            // The GC may use the low bits of the MethodTable*
            // pointer to mark objects.  When finding objects that
            // are point to the method table, these bits should be
            // masked out.
            let ptr_mask: usize = if Pointer::SIZE == 8 { !7 } else { !3 };
            let ptr = mem_value.value & ptr_mask;
            ptr == search_ptr
        })
        .map(|mem_value| mem_value.location)
        .filter(move |ptr| {
            // Each .NET Object starts with a pointer to the
            // MethodTable, but just looking for pointer-aligned
            // memory with the correct value would also find
            // anything else that holds a `MethodTable*`, or stack
            // frames that accept a `MethodTable*` argument.
            //
            // Just before the `this` pointer of a .NET object is
            // the `ObjHeader`.  This is 4 bytes of flags, with
            // another 4 bytes of padding on 64-bit systems.  That
            // 4 bytes of padding
            let Ok(preceding): Result<[_; Pointer::SIZE], _> =
                reader.read_byte_array(*ptr - Pointer::SIZE)
            else {
                return false;
            };

            let has_valid_align_pad = Pointer::SIZE == 4
                || (preceding[0] == 0
                    && preceding[1] == 0
                    && preceding[2] == 0
                    && preceding[3] == 0);

            // In the 32 bits preceding the object, there is a
            // per-object header.  While most of these aren't useful
            // for identifying an object, the highest three bits are
            // useful.
            let sigblock = u32::from_le_bytes(
                preceding[Pointer::SIZE - 4..].try_into().unwrap(),
            );

            // An unused bit in release builds, set to one in debug
            // builds.  Since I'm inspecting a release build,
            // anything with this bit set isn't an object.
            let is_unused = sigblock & 0x80000000 > 0;

            // Indicates if the object must run a finalizer before
            // being deallocated.  While an object is alive, this will
            // always be set to one for objects that have a finalizer,
            // and zero for objects that do not.
            let requires_finalizer = sigblock & 0x40000000 > 0;

            // Used for the garbage collector's mark-and-sweep.  When
            // looking for long-lived objects, this will nearly always
            // be unset.
            let is_reserved_for_gc = sigblock & 0x20000000 > 0;

            has_valid_align_pad
                && !is_unused
                && !is_reserved_for_gc
                && (requires_finalizer == has_finalizer)
        })
        .filter(move |ptr| {
            if offsets_of_pointers.is_empty() {
                return true;
            }

            let min_offset = *offsets_of_pointers.first().unwrap();
            let max_offset =
                offsets_of_pointers.last().unwrap() + Pointer::SIZE;
            let Ok(bytes) =
                reader.read_bytes(*ptr + min_offset, max_offset - min_offset)
            else {
                return false;
            };

            offsets_of_pointers.iter().all(|offset| {
                let expected_ptr: Pointer = bytes
                    [offset - min_offset..offset - min_offset + Pointer::SIZE]
                    .try_into()
                    .unwrap();
                expected_ptr.is_null()
                    || reader.find_containing_region(expected_ptr).is_some()
            })
        });

    Ok(iter)
}

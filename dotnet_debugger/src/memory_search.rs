use itertools::Itertools as _;
use memory_reader::{MemoryReader, Pointer};

use crate::{Error, MethodTable};

/// Returns an iterator of pointers that would be valid objects to the
/// specified type.
///
/// Currently, this uses the following heuristics:
///
/// 1. The object has a pointer to `method_table`.  This pointer may
///    have the lowest bit set if located during a garbage-collection
///    step.
///
/// 2. The 32-bit header preceding the object has valid contents.
///
/// 3. On 64-bit machines, the additional 32-bit padding preceding the
///    header is filled with zero.
///
/// 4. For each field that may contain a pointer, the bytes that would
///    correspond to that field contain either a valid pointer or
///    NULL.
pub fn find_object_instances<'a>(
    method_table: &MethodTable,
    reader: &'a MemoryReader,
) -> Result<impl Iterator<Item = Pointer> + 'a, Error> {
    let iter = iter_possible_object_instances(method_table, reader)?
        .map(|(ptr, _)| ptr);

    Ok(iter)
}

/// Find a most likely pointer to an object of a known type.
///
/// In many cases, it is useful to find a representative object of a
/// type.  For example, if all objects of a type hold a reference to a
/// shared resource, or if there should only exist a single instance
/// of the type.
///
/// In `find_object_instances`, valid objects are identified by the
/// presence of either a valid pointer or NULL at offsets that must
/// contain a pointer.  Since any pointer-sized object that is
/// zero-initialized will have the same bit pattern as NULL, this is
/// likely to have false positives.
///
/// This method tracks the number of pointer fields that contain a
/// non-null pointer, and returns the object that has the most
/// non-null pointers.
pub fn find_most_likely_object_instance(
    method_table: &MethodTable,
    reader: &MemoryReader,
) -> Result<Pointer, Error> {
    iter_possible_object_instances(method_table, reader)?
        .max_by_key(|(_, non_null_ptr_count)| *non_null_ptr_count)
        .map(|(ptr, _)| ptr)
        .ok_or(Error::NoObjectInstanceFound)
}

fn iter_possible_object_instances<'a>(
    method_table: &MethodTable,
    reader: &'a MemoryReader,
) -> Result<impl Iterator<Item = (Pointer, usize)> + 'a, Error> {
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
        .map(|field| {
            // The offset is recorded relative to the location after
            // the MethodTable*, but it's more convenient to have the
            // offset relative to the start of the MethodTable*.
            field.offset() + Pointer::SIZE
        })
        .sorted()
        .collect();

    let iter = reader
        .regions
        .iter()
        .filter(|region| {
            region.is_readable && region.is_writable && !region.is_shared_memory
        })
        .filter(|region| {
            region
                .name
                .as_ref()
                .map(|name| name != "[heap]")
                .unwrap_or(true)
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
        .filter_map(move |ptr| -> Option<(Pointer, usize)> {
            if offsets_of_pointers.is_empty() {
                return Some((ptr, 0));
            }

            let min_offset = *offsets_of_pointers.first().unwrap();
            let max_offset =
                offsets_of_pointers.last().unwrap() + Pointer::SIZE;
            let bytes = reader
                .read_bytes(ptr + min_offset, max_offset - min_offset)
                .ok()?;

            let num_non_null_pointers = offsets_of_pointers
                .iter()
                .map(|offset| {
                    let range = offset - min_offset
                        ..offset - min_offset + Pointer::SIZE;
                    bytes[range].try_into().unwrap()
                })
                .map(|expected_ptr: Pointer| -> Option<usize> {
                    if expected_ptr.is_null() {
                        Some(0)
                    } else if reader
                        .find_containing_region(expected_ptr)
                        .map(|region| {
                            region.is_readable
                                && region.is_writable
                                && !region.is_shared_memory
                        })
                        .is_some()
                    {
                        Some(1)
                    } else {
                        None
                    }
                })
                .sum::<Option<usize>>()?;

            Some((ptr, num_non_null_pointers))
        });

    Ok(iter)
}

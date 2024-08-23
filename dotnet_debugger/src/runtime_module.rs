use std::collections::HashSet;

use dll_unpacker::Metadata;
use itertools::Itertools as _;
use memory_reader::{MemoryReader, Pointer};

use crate::Error;

/// Contains locations of key structures within the runtime
/// representation of a single CLR DLL.
pub struct RuntimeModule {}

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
}

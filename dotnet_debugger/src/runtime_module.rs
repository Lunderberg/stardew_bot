use std::borrow::Borrow;
use std::cell::OnceCell;
use std::collections::HashSet;
use std::ops::Range;

use dll_unpacker::{Metadata, MetadataLayout, MetadataTableIndex, TypeDef};
use itertools::Itertools as _;
use memory_reader::{extensions::*, MemoryRegion, OwnedBytes};
use memory_reader::{MemoryReader, Pointer};

use crate::extensions::*;
use crate::{Error, MethodTable, ReadTypedPointer, TypedPointer};

/// Contains locations of key structures within the runtime
/// representation of a single CLR DLL.
pub struct RuntimeModule {
    /// Location of the Module object
    pub location: Pointer,

    /// Location of the DLL in memory
    image_ptr: OnceCell<Pointer>,

    /// The DLL corresponding to this Module.
    dll_region: OnceCell<MemoryRegion>,

    /// The metadata associated with the DLL
    metadata_layout: OnceCell<MetadataLayout>,

    /// The location of the pointer to the MethodTableLookup, and a
    /// bitmask defining flags that may be set on each method table
    /// pointer.
    method_table_info: OnceCell<(Pointer, usize)>,

    /// The lookup for the MethodTables
    method_table_lookup: OnceCell<MethodTableLookup>,

    /// The base pointer of static pointer to non-garbage-collected
    /// static values.  These values are relative to the
    /// DomainLocalModule struct, a pointer to which is in the Module.
    ///
    /// For .NET 9.0 onward, this will need to be updated to find
    /// statics relative to the MethodTable.
    /// https://github.com/dotnet/runtime/commit/eb8f54d9 (2024-06-12)
    base_ptr_of_non_gc_statics: OnceCell<Pointer>,

    /// The base pointer of static pointers to garbage-collected
    /// static values.  These values are relative to an allocation
    /// owned by the DomainLocalModule.
    base_ptr_of_gc_statics: OnceCell<Pointer>,
}

pub struct MethodTableLookup {
    pub location: Range<Pointer>,
    pub method_tables: Vec<Range<Pointer>>,
}

impl RuntimeModule {
    /// Given the unpacked Metadata from a DLL, attempt to locate its
    /// runtime representation.
    pub fn locate(
        metadata: &Metadata,
        reader: &MemoryReader,
    ) -> Result<TypedPointer<Self>, Error> {
        // Pointers to IL method definitions in the loaded DLL.
        let dll_method_def: HashSet<Pointer> = metadata
            .method_def_table()
            .iter_rows()
            .filter_map(|method_def| method_def.cil_method().ok().flatten())
            .filter_map(|cil_method| cil_method.body_range().ok())
            .map(|method_body| method_body.start)
            .collect();

        let ptr = reader
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
            .ok_or(Error::ModulePointerNotFound)?;

        Ok(ptr.into())
    }

    pub fn read(location: Pointer) -> Result<Self, Error> {
        Ok(Self {
            location,
            image_ptr: Default::default(),
            dll_region: Default::default(),
            metadata_layout: Default::default(),
            method_table_info: Default::default(),
            method_table_lookup: Default::default(),
            base_ptr_of_non_gc_statics: Default::default(),
            base_ptr_of_gc_statics: Default::default(),
        })
    }

    pub fn iter_method_tables<'a>(
        &'a self,
        reader: &'a MemoryReader,
    ) -> Result<impl Iterator<Item = Result<MethodTable, Error>> + 'a, Error>
    {
        Ok(self.method_table_lookup(reader)?.iter_tables(reader))
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

    pub fn dll_region(
        &self,
        reader: &MemoryReader,
    ) -> Result<&MemoryRegion, Error> {
        self.dll_region.or_try_init(|| {
            let image_ptr = self.image_ptr(reader)?;

            Ok(reader
                .regions
                .iter()
                .filter(|region| {
                    region.contains(image_ptr) && region.file_offset() == 0
                })
                .max_by_key(|region| region.size_bytes())
                .ok_or(Error::RegionForDLLNotFound(image_ptr))?
                .read()?)
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

    fn method_table_info(
        &self,
        reader: &MemoryReader,
    ) -> Result<&(Pointer, usize), Error> {
        self.method_table_info.or_try_init(|| {
            let num_type_defs =
                self.metadata(reader)?.type_def_table().num_rows();

            // The layout of the Module varies by .NET version, but should
            // be less than 4kB for each.  If I don't find each pointer by
            // then, then something else is probably wrong.
            let bytes = reader.read_bytes(self.location, 4096)?;

            bytes
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
                .map(|(_, p_table, _, supported_flags)| {
                    (p_table, supported_flags)
                })
                .ok_or(Error::PointerToMethodTableTableNotFound)
        })
    }

    pub fn ptr_to_table_of_method_tables(
        &self,
        reader: &MemoryReader,
    ) -> Result<Pointer, Error> {
        self.method_table_info(reader).map(|(ptr, _)| *ptr)
    }

    pub fn method_table_lookup(
        &self,
        reader: impl Borrow<MemoryReader>,
    ) -> Result<&MethodTableLookup, Error> {
        let reader = reader.borrow();

        self.method_table_lookup.or_try_init(|| {
            let num_type_defs =
                self.metadata(reader)?.type_def_table().num_rows();

            let bytes = reader.read_bytes(self.location, 4096)?;

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
                .map(|(_, p_table, _, supported_flags)| {
                    (p_table, supported_flags)
                })
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

            Ok(method_table_lookup)
        })
    }

    pub fn base_ptr_of_non_gc_statics(
        &self,
        reader: &MemoryReader,
    ) -> Result<Pointer, Error> {
        self.base_ptr_of_non_gc_statics
            .or_try_init(|| {
                // The layout of the Module varies by .NET version, but should
                // be less than 4kB for each.  If I don't find each pointer by
                // then, then something else is probably wrong.
                let bytes = reader.read_bytes(self.location, 4096)?;

                let num_type_defs =
                    self.metadata(reader)?.type_def_table().num_rows();

                bytes
                    .chunks_exact(8)
                    .tuple_windows()
                    .map(|(a, b, c, d, e)| {
                        let domain_local_module: Pointer =
                            a.try_into().unwrap();
                        let module_index: u64 =
                            u64::from_ne_bytes(b.try_into().unwrap());
                        let regular_statics_offsets: Pointer =
                            c.try_into().unwrap();
                        let thread_statics_offsets: Pointer =
                            d.try_into().unwrap();
                        let max_rid_statics_allocated =
                            u32::from_ne_bytes(e[..4].try_into().unwrap())
                                as usize;
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
                                    || reader
                                        .is_valid_ptr(*regular_statics_offsets))
                                && (thread_statics_offsets.is_null()
                                    || reader
                                        .is_valid_ptr(*thread_statics_offsets))
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
                    .map(|(domain_local_module, _, _, _, _)| {
                        domain_local_module
                    })
                    .next()
                    .ok_or(Error::PointerToDomainLocalModuleNotFound)
            })
            .copied()
    }

    pub fn base_ptr_of_gc_statics(
        &self,
        reader: &MemoryReader,
    ) -> Result<Pointer, Error> {
        self.base_ptr_of_gc_statics
            .or_try_init(|| {
                let base_ptr_of_non_gc_statics =
                    self.base_ptr_of_non_gc_statics(reader)?;
                let base_ptr_of_gc_statics = reader
                    .read_byte_array(base_ptr_of_non_gc_statics + 32)?
                    .into();

                Ok(base_ptr_of_gc_statics)
            })
            .copied()
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

impl ReadTypedPointer for RuntimeModule {
    fn read_typed_ptr(
        ptr: Pointer,
        _reader: &MemoryReader,
    ) -> Result<Self, Error> {
        RuntimeModule::read(ptr)
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

    pub fn iter_tables<'a>(
        &'a self,
        reader: &'a MemoryReader,
    ) -> impl Iterator<Item = Result<MethodTable, Error>> + 'a {
        self.method_tables
            .iter()
            .cloned()
            .filter(|ptr| !ptr.start.is_null())
            .map(move |location| {
                let nbytes = location.end - location.start;
                let bytes = reader.read_bytes(location.start, nbytes)?;
                Ok(MethodTable {
                    bytes: OwnedBytes::new(location.start, bytes),
                })
            })
    }

    pub fn get(
        &self,
        index: MetadataTableIndex<TypeDef>,
        reader: impl Borrow<MemoryReader>,
    ) -> Result<MethodTable, Error> {
        let reader = reader.borrow();

        let byte_range = &self[index];
        let bytes = reader
            .read_bytes(byte_range.start, byte_range.end - byte_range.start)?;
        let bytes = OwnedBytes::new(byte_range.start, bytes);
        Ok(MethodTable { bytes })
    }

    pub fn get_ptr(
        &self,
        index: MetadataTableIndex<TypeDef>,
    ) -> TypedPointer<MethodTable> {
        self[index].start.into()
    }
}

impl std::ops::Index<MetadataTableIndex<TypeDef>> for MethodTableLookup {
    type Output = Range<Pointer>;

    fn index(&self, index: MetadataTableIndex<TypeDef>) -> &Self::Output {
        let index: usize = index.into();
        &self.method_tables[index + 1]
    }
}

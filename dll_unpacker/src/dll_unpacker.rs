use std::{marker::PhantomData, ops::Range};

use memory_reader::{MemoryRegion, Pointer};

use crate::{Annotation as _, Annotator, Error};

#[derive(Clone)]
pub struct ByteRange<'a> {
    start: Pointer,
    bytes: &'a [u8],
}

#[derive(Clone)]
pub struct UnpackedValue<T> {
    pub loc: Range<Pointer>,
    pub value: T,
}

pub struct Unpacker<'a> {
    bytes: ByteRange<'a>,
}

pub struct PEHeaderUnpacker<'a> {
    bytes: ByteRange<'a>,
}

/// Bitflags for the PE header
///
/// https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#characteristics
pub struct PEHeaderCharacteristics {
    /// Bit 0x0001.  Indicates if the relocation table has been
    /// removed.
    relocations_stripped: bool,

    /// Bit 0x0002.  Indicates if the linking step was successful.
    valid_executable: bool,

    /// Bit 0x0004.  Indicates if the line numbers have been stripped.
    _deprecated_line_nums_stripped: bool,

    /// Bit 0x0008.  Indicates if local symbols have been stripped
    /// from the symbol table.
    _deprecated_local_symbols_stripped: bool,

    /// Bit 0x0010.  Deprecated runtime flag.
    _deprecated_aggressively_trim_working_set: bool,

    /// Bit 0x0020.  Indicates if addresses beyond 2 GB can be
    /// handled.
    large_address_aware: bool,

    /// Bit 0x0040.
    _reserved_bit6: bool,

    /// Bit 0x0080.  Deprecated.
    _deprecated_is_little_endian: bool,

    /// Bit 0x0100.
    is_32bit: bool,

    /// Bit 0x0200.
    debug_info_stripped: bool,

    /// Bit 0x0400.  If launched from a removable drive, copy to swap
    /// before executing.
    run_from_swap_if_removable_drive: bool,

    /// Bit 0x0800.  If launched from a network drive, copy to swap
    /// before executing.
    run_from_swap_if_network_drive: bool,

    /// Bit 0x1000.  If true, file is a system file, not a user file.
    is_system_file: bool,

    /// Bit 0x2000.  If true, the file is a DLL.
    is_dll: bool,

    /// Bit 0x4000.  If true, the file should only be run on a
    /// uniprocessor machine.
    is_uniprocessor_machine: bool,

    /// Bit 0x8000.  Deprecated.
    _deprecated_is_big_endian: bool,
}

pub struct OptionalHeaderUnpacker<'a> {
    /// The bytes for the optional header
    bytes: ByteRange<'a>,

    /// The magic_value at the start of the optional header.  Saved
    /// here, since the location of most fields depend on it.
    magic_value: MagicValue,
}

#[derive(Debug)]
pub enum MagicValue {
    PE32,
    PE32plus,
}

pub struct VirtualRange {
    rva: u32,
    size: u32,
}

#[derive(Debug, Clone, Copy)]
pub enum DataDirectoryKind {
    ExportTable,
    ImportTable,
    ResourceTable,
    ExceptionTable,
    CertificateTable,
    BaseRelocationTable,
    Debug,
    Architecture,
    GlobalPtr,
    TLSTable,
    LoadConfigTable,
    BoundImportTable,
    ImportAddressTable,
    DelayImportDescriptor,
    ClrRuntimeHeader,
    Reserved15,
}

pub struct SectionHeaderUnpacker<'a> {
    bytes: ByteRange<'a>,
}

pub struct ClrRuntimeHeaderUnpacker<'a> {
    bytes: ByteRange<'a>,
}

pub struct MetadataUnpacker<'a> {
    bytes: ByteRange<'a>,
}

pub struct StreamHeader<'a> {
    /// The offset of the stream, in bytes, relative to the start of
    /// the metadata.
    offset: UnpackedValue<u32>,

    /// The size of the stream, in bytes.
    size: UnpackedValue<u32>,

    /// The name of the metadata section.  Max of 32 ASCII characters.
    name: UnpackedValue<&'a str>,
}

pub struct TildeStreamUnpacker<'a> {
    bytes: ByteRange<'a>,
    string_heap: ByteRange<'a>,
    blob_heap: ByteRange<'a>,
}

#[derive(Clone, Copy)]
pub struct HeapSizes {
    string_stream_uses_u32_addr: bool,
    guid_stream_uses_u32_addr: bool,
    blob_stream_uses_u32_addr: bool,
}

#[derive(Clone, Copy, Debug)]
pub enum MetadataTableKind {
    Module,
    TypeRef,
    TypeDef,
    Field,
    MethodDef,
    Param,
    InterfaceImpl,
    MemberRef,
    Constant,
    CustomAttribute,
    FieldMarshal,
    DeclSecurity,
    ClassLayout,
    FieldLayout,
    StandAloneSig,
    EventMap,
    Event,
    PropertyMap,
    Property,
    MethodSemantics,
    MethodImpl,
    ModuleRef,
    TypeSpec,
    ImplMap,
    FieldRVA,
    Assembly,
    AssemblyProcessor,
    AssemblyOS,
    AssemblyRef,
    AssemblyRefProcessor,
    AssemblyRefOS,
    File,
    ExportedType,
    ManifestResource,
    NestedClass,
    GenericParam,
    MethodSpec,
    GenericParamConstraint,
}

pub struct MetadataSizes {
    heap_sizes: HeapSizes,
    table_sizes: [u32; 38],
}

pub struct MetadataTableUnpacker<'a, RowUnpacker> {
    bytes: ByteRange<'a>,

    sizes: &'a MetadataSizes,
    string_heap: ByteRange<'a>,
    blob_heap: ByteRange<'a>,
    row_unpacker: PhantomData<RowUnpacker>,
}

pub struct MetadataIndex {
    table: MetadataTableKind,
    index: usize,
}

pub struct MetadataRowUnpacker<'a, RowUnpacker> {
    /// The bytes that are directly owned by this row of the table.
    bytes: ByteRange<'a>,

    /// The bytes that are directly owned by the next row of the
    /// table.  This is needed to interpret columns that provide a
    /// start index, with the end index provided by the same column in
    /// the next row (e.g. the Field and Method columns of the TypeDef
    /// table).
    next_row_bytes: Option<ByteRange<'a>>,
    sizes: &'a MetadataSizes,
    string_heap: ByteRange<'a>,
    blob_heap: ByteRange<'a>,
    row_unpacker: PhantomData<RowUnpacker>,
}

pub trait RowUnpacker {
    const KIND: MetadataTableKind;
}

pub struct ModuleRowUnpacker;
pub struct TypeRefRowUnpacker;
pub struct TypeDefRowUnpacker;
pub struct FieldRowUnpacker;
pub struct MethodDefRowUnpacker;
pub struct ParamRowUnpacker;
pub struct InterfaceImplRowUnpacker;
pub struct MemberRefRowUnpacker;

impl<T> UnpackedValue<T> {
    pub fn map<U>(self, func: impl FnOnce(T) -> U) -> UnpackedValue<U> {
        UnpackedValue {
            value: func(self.value),
            loc: self.loc,
        }
    }

    pub fn try_map<U, E>(
        self,
        func: impl FnOnce(T) -> Result<U, E>,
    ) -> Result<UnpackedValue<U>, E> {
        let value = func(self.value)?;
        Ok(UnpackedValue {
            value,
            loc: self.loc,
        })
    }
}

trait NormalizeOffset: Copy {
    fn as_offset(self, start: Pointer) -> usize;
    fn as_ptr(self, start: Pointer) -> Pointer;
}
impl NormalizeOffset for usize {
    fn as_offset(self, _start: Pointer) -> usize {
        self
    }

    fn as_ptr(self, start: Pointer) -> Pointer {
        start + self
    }
}
impl NormalizeOffset for Pointer {
    fn as_offset(self, start: Pointer) -> usize {
        self - start
    }

    fn as_ptr(self, _start: Pointer) -> Pointer {
        self
    }
}

impl<'a> ByteRange<'a> {
    fn len(&self) -> usize {
        self.bytes.len()
    }

    fn get_u8(
        &self,
        loc: impl NormalizeOffset,
    ) -> Result<UnpackedValue<u8>, Error> {
        let offset = loc.as_offset(self.start);
        let loc = loc.as_ptr(self.start);
        Ok(UnpackedValue {
            loc: loc..loc + 1,
            value: self.bytes[offset],
        })
    }

    fn get_u16(
        &self,
        loc: impl NormalizeOffset,
    ) -> Result<UnpackedValue<u16>, Error> {
        let byte_offset = loc.as_offset(self.start);
        let byte_range = byte_offset..byte_offset + 2;
        let value = u16::from_le_bytes(
            self.bytes[byte_range.clone()].try_into().unwrap(),
        );
        Ok(UnpackedValue {
            loc: self.address_range(byte_range),
            value,
        })
    }

    fn get_u32(
        &self,
        loc: impl NormalizeOffset,
    ) -> Result<UnpackedValue<u32>, Error> {
        let byte_offset = loc.as_offset(self.start);
        let byte_range = byte_offset..byte_offset + 4;
        let value = u32::from_le_bytes(
            self.bytes[byte_range.clone()].try_into().unwrap(),
        );
        Ok(UnpackedValue {
            loc: self.address_range(byte_range),
            value,
        })
    }

    fn get_u64(
        &self,
        loc: impl NormalizeOffset,
    ) -> Result<UnpackedValue<u64>, Error> {
        let byte_offset = loc.as_offset(self.start);
        let byte_range = byte_offset..byte_offset + 8;
        let value = u64::from_le_bytes(
            self.bytes[byte_range.clone()].try_into().unwrap(),
        );
        Ok(UnpackedValue {
            loc: self.address_range(byte_range),
            value,
        })
    }

    fn get_null_terminated(
        &self,
        loc: impl NormalizeOffset,
    ) -> Result<UnpackedValue<&'a str>, Error> {
        let start = loc.as_offset(self.start);
        let size = self.bytes[start..]
            .iter()
            .enumerate()
            .take_while(|(_, byte)| **byte > 0)
            .map(|(i, _)| i + 1)
            .last()
            .unwrap_or(0);
        let value = std::str::from_utf8(&self.bytes[start..start + size])?;
        let loc = self.address_range(start..start + size);
        Ok(UnpackedValue { value, loc })
    }

    fn get_virtual_range(
        &self,
        loc: impl NormalizeOffset,
    ) -> Result<UnpackedValue<VirtualRange>, Error> {
        let start = loc.as_offset(self.start);
        let rva = self.get_u32(start)?.value;
        let size = self.get_u32(start + 4)?.value;
        Ok(UnpackedValue {
            loc: self.address_range(start..start + 8),
            value: VirtualRange { rva, size },
        })
    }

    fn subrange(&self, range: Range<impl NormalizeOffset>) -> Self {
        let start = range.start.as_offset(self.start);
        let end = range.end.as_offset(self.start);
        Self {
            start: self.start + start,
            bytes: &self.bytes[start..end],
        }
    }

    fn end(&self) -> Pointer {
        self.start + self.bytes.len()
    }

    fn address_range(
        &self,
        range: Range<impl NormalizeOffset>,
    ) -> Range<Pointer> {
        let start = range.start.as_ptr(self.start);
        let end = range.end.as_ptr(self.start);
        start..end
    }

    fn as_range(&self) -> Range<Pointer> {
        self.start..self.start + self.bytes.len()
    }
}

impl<'a> Into<std::ops::Range<Pointer>> for &ByteRange<'a> {
    fn into(self) -> std::ops::Range<Pointer> {
        self.as_range()
    }
}

impl<'a> Into<std::ops::Range<Pointer>> for ByteRange<'a> {
    fn into(self) -> std::ops::Range<Pointer> {
        self.as_range()
    }
}

impl<'a, T: NormalizeOffset> std::ops::Index<T> for ByteRange<'a> {
    type Output = u8;

    fn index(&self, index: T) -> &Self::Output {
        &self.bytes[index.as_offset(self.start)]
    }
}

impl<'a, T: NormalizeOffset> std::ops::Index<Range<T>> for ByteRange<'a> {
    type Output = [u8];

    fn index(&self, index: Range<T>) -> &Self::Output {
        let start = index.start.as_offset(self.start);
        let end = index.end.as_offset(self.start);
        &self.bytes[start..end]
    }
}

impl<'a> Unpacker<'a> {
    pub fn new(region: &'a MemoryRegion) -> Unpacker {
        Self {
            bytes: ByteRange {
                start: region.start(),
                bytes: region.data(),
            },
        }
    }

    pub fn unpacked_so_far(&self) -> Result<Pointer, Error> {
        let metadata = self.metadata()?;
        let stream = metadata.tilde_stream()?;
        let sizes = stream.metadata_sizes()?;
        let table = stream.member_ref_table(&sizes)?;

        Ok(table.bytes.end())
    }

    pub fn address_range(&self, byte_range: Range<usize>) -> Range<Pointer> {
        let start = self.bytes.start;
        start + byte_range.start..start + byte_range.end
    }

    pub fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.range(self.dos_header()?.loc).name("DOS header");
        annotator.value(self.lfanew()?).name("lfanew");
        annotator.range(self.dos_stub()?.loc).name("DOS stub");

        let pe_header = self.pe_header()?;
        pe_header.collect_annotations(annotator)?;

        self.optional_header()?.collect_annotations(annotator)?;

        self.clr_runtime_header()?.collect_annotations(annotator)?;

        annotator
            .group(self.section_header_bytes()?)
            .name("Section headers");
        for i_section in 0..pe_header.num_sections()?.value as usize {
            self.section_header(i_section)?
                .collect_annotations(annotator)?;
        }

        self.metadata()?.collect_annotations(annotator)?;

        Ok(())
    }

    pub fn dos_header(&self) -> Result<UnpackedValue<()>, Error> {
        let dos_header = [
            /*  0-8  */ 0x4D, 0x5A, 0x90, 0x00, 0x03, 0x00, 0x00, 0x00,
            /*  8-16 */ 0x04, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0x00, 0x00,
            /* 16-24 */ 0xB8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            /* 24-32 */ 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            /* 32-40 */ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            /* 40-48 */ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            /* 48-56 */ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            /* 56-60 */ 0x00, 0x00, 0x00, 0x00,
        ];

        let byte_range = 0..60;
        if dos_header == self.bytes[byte_range.clone()] {
            Ok(UnpackedValue {
                loc: self.bytes.address_range(byte_range),
                value: (),
            })
        } else {
            Err(Error::IncorrectDOSHeader)
        }
    }

    pub fn dos_stub(&self) -> Result<UnpackedValue<()>, Error> {
        let dos_stub = [
            /* 64- 72 */ 0x0E, 0x1F, 0xBA, 0x0E, 0x00, 0xB4, 0x09, 0xCD,
            /* 72- 80 */ 0x21, 0xB8, 0x01, 0x4C, 0xCD, 0x21, 0x54, 0x68,
            /* 80- 88 */ 0x69, 0x73, 0x20, 0x70, 0x72, 0x6F, 0x67, 0x72,
            /* 88- 96 */ 0x61, 0x6D, 0x20, 0x63, 0x61, 0x6E, 0x6E, 0x6F,
            /* 96-102 */ 0x74, 0x20, 0x62, 0x65, 0x20, 0x72, 0x75, 0x6E,
            /*102-110 */ 0x20, 0x69, 0x6E, 0x20, 0x44, 0x4F, 0x53, 0x20,
            /*110-118 */ 0x6D, 0x6F, 0x64, 0x65, 0x2E, 0x0D, 0x0D, 0x0A,
            /*118-128 */ 0x24, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ];

        let byte_range = 64..128;
        if dos_stub == self.bytes[byte_range.clone()] {
            Ok(UnpackedValue {
                loc: self.bytes.address_range(byte_range),
                value: (),
            })
        } else {
            Err(Error::IncorrectDOSHeader)
        }
    }

    pub fn lfanew(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(60)
    }

    pub fn pe_header(&self) -> Result<PEHeaderUnpacker, Error> {
        let lfanew = self.lfanew()?.value as usize;
        let bytes = self.bytes.subrange(lfanew..lfanew + 24);
        Ok(PEHeaderUnpacker { bytes })
    }

    pub fn optional_header(&self) -> Result<OptionalHeaderUnpacker, Error> {
        let pe_header = self.pe_header()?;
        let optional_header_size =
            pe_header.optional_header_size()?.value as usize;
        let start = pe_header.bytes.end();
        let bytes = self.bytes.subrange(start..start + optional_header_size);
        OptionalHeaderUnpacker::new(bytes)
    }

    pub fn section_header_bytes(&self) -> Result<ByteRange, Error> {
        let pe_header = self.pe_header()?;
        let num_sections = pe_header.num_sections()?.value as usize;

        let optional_header = self.optional_header()?;
        let section_header_size = 40;

        let start = optional_header.bytes.end();
        let size = num_sections * section_header_size;
        let bytes = self.bytes.subrange(start..start + size);
        Ok(bytes)
    }

    pub fn section_header(
        &self,
        i_section: usize,
    ) -> Result<SectionHeaderUnpacker, Error> {
        let section_header_size = 40;
        let section_headers = self.section_header_bytes()?;
        let num_sections = section_headers.len() / section_header_size;

        if i_section >= num_sections {
            return Err(Error::InvalidSectionNumber {
                num_sections,
                i_section,
            });
        }

        let start = i_section * section_header_size;
        let end = (i_section + 1) * section_header_size;
        let bytes = section_headers.subrange(start..end);
        Ok(SectionHeaderUnpacker { bytes })
    }

    pub fn iter_section_header(
        &self,
    ) -> Result<impl Iterator<Item = SectionHeaderUnpacker>, Error> {
        let pe_header = self.pe_header()?;
        let num_sections = pe_header.num_sections()?.value as usize;
        let optional_header = self.optional_header()?;

        let section_header_base = optional_header.bytes.end();

        let section_header_size = 40;

        let iter = (0..num_sections)
            .map(move |i_section| {
                section_header_base + i_section * section_header_size
            })
            .map(move |start| {
                let bytes =
                    self.bytes.subrange(start..start + section_header_size);
                SectionHeaderUnpacker { bytes }
            });

        Ok(iter)
    }

    fn virtual_address_to_raw(&self, addr: u32) -> Result<Pointer, Error> {
        self.iter_section_header()?
            .find_map(|section| {
                let virtual_range = section.virtual_range().ok()?;
                if virtual_range.contains(&addr) {
                    let addr_within_section =
                        (addr - virtual_range.start) as usize;
                    let raw_section_addr =
                        section.raw_address().ok()?.value as usize;

                    Some(
                        self.bytes.start
                            + raw_section_addr
                            + addr_within_section,
                    )
                } else {
                    None
                }
            })
            .ok_or(Error::InvalidVirtualAddress(addr))
    }

    pub fn clr_runtime_header(
        &self,
    ) -> Result<ClrRuntimeHeaderUnpacker, Error> {
        let optional_header = self.optional_header()?;
        let data_dir = optional_header
            .data_directory(DataDirectoryKind::ClrRuntimeHeader)?
            .value;

        let addr = self.virtual_address_to_raw(data_dir.rva)?;

        let size = data_dir.size as usize;
        let bytes = self.bytes.subrange(addr..addr + size);
        Ok(ClrRuntimeHeaderUnpacker { bytes })
    }

    pub fn metadata(&self) -> Result<MetadataUnpacker, Error> {
        let clr_runtime_header = self.clr_runtime_header()?;
        let metadata_range = clr_runtime_header.metadata_range()?.value;
        let raw_start = self.virtual_address_to_raw(metadata_range.rva)?;
        let num_bytes = metadata_range.size as usize;
        let bytes = self.bytes.subrange(raw_start..raw_start + num_bytes);
        Ok(MetadataUnpacker { bytes })
    }
}

impl<'a> PEHeaderUnpacker<'a> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.group(&self.bytes).name("PE Header");

        annotator
            .range(self.pe_signature()?.loc)
            .name("PE signature");
        annotator.value(self.machine_type()?).name("Machine type");
        annotator.value(self.num_sections()?).name("Num sections");
        annotator
            .value(self.creation_timestamp()?)
            .name("Creation timestamp");
        annotator
            .value(self.symbol_table_pointer()?)
            .name("Symbol table pointer");
        annotator.value(self.num_symbols()?).name("Num symbols");
        annotator
            .value(self.optional_header_size()?)
            .name("Size of optional header");

        annotator
            .value(self.characteristics()?)
            .name("Characteristics");

        Ok(())
    }

    pub fn pe_signature(&self) -> Result<UnpackedValue<()>, Error> {
        let pe_signature = [0x50, 0x45, 0x00, 0x00];

        let byte_range = 0..4;
        if pe_signature == self.bytes[byte_range.clone()] {
            Ok(UnpackedValue {
                loc: self.bytes.address_range(byte_range),
                value: (),
            })
        } else {
            Err(Error::IncorrectPESignature)
        }
    }

    pub fn machine_type(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(4)
    }

    pub fn num_sections(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(6)
    }

    pub fn creation_timestamp(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(8)
    }

    pub fn symbol_table_pointer(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(12)
    }

    pub fn num_symbols(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(16)
    }

    pub fn optional_header_size(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(20)
    }

    pub fn characteristics(
        &self,
    ) -> Result<UnpackedValue<PEHeaderCharacteristics>, Error> {
        self.bytes
            .get_u16(22)
            .map(|unpacked| unpacked.map(PEHeaderCharacteristics::new))
    }
}

impl PEHeaderCharacteristics {
    pub fn new(value: u16) -> Self {
        Self {
            relocations_stripped: value & 0x0001 != 0,
            valid_executable: value & 0x0002 != 0,
            _deprecated_line_nums_stripped: value & 0x0004 != 0,
            _deprecated_local_symbols_stripped: value & 0x0008 != 0,
            _deprecated_aggressively_trim_working_set: value & 0x0010 != 0,
            large_address_aware: value & 0x0020 != 0,
            _reserved_bit6: value & 0x0040 != 0,
            _deprecated_is_little_endian: value & 0x0080 != 0,
            is_32bit: value & 0x0100 != 0,
            debug_info_stripped: value & 0x0200 != 0,
            run_from_swap_if_removable_drive: value & 0x0400 != 0,
            run_from_swap_if_network_drive: value & 0x0800 != 0,
            is_system_file: value & 0x1000 != 0,
            is_dll: value & 0x2000 != 0,
            is_uniprocessor_machine: value & 0x4000 != 0,
            _deprecated_is_big_endian: value & 0x8000 != 0,
        }
    }
}

impl std::fmt::Display for PEHeaderCharacteristics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #![allow(unused_assignments)]
        let mut is_first = true;

        macro_rules! annotate {
            ($field:ident) => {
                if self.$field {
                    if is_first {
                        is_first = false;
                    } else {
                        write!(f, "\n")?;
                    }
                    write!(f, stringify!($field))?;
                }
            };
        }

        annotate! {relocations_stripped};
        annotate! {valid_executable};
        // annotate! {_deprecated_line_nums_stripped};
        // annotate! {_deprecated_local_symbols_stripped};
        // annotate! {_deprecated_aggressive_working_set_trimmed};
        annotate! {large_address_aware};
        // annotate! {_reserved_bit6};
        // annotate! {_deprecated_is_little_endian};
        annotate! {is_32bit};
        annotate! {debug_info_stripped};
        annotate! {run_from_swap_if_removable_drive};
        annotate! {run_from_swap_if_network_drive};
        annotate! {is_system_file};
        annotate! {is_dll};
        annotate! {is_uniprocessor_machine};
        // annotate! {_deprecated_is_big_endian};

        Ok(())
    }
}

impl<'a> OptionalHeaderUnpacker<'a> {
    fn new(bytes: ByteRange<'a>) -> Result<Self, Error> {
        let mut out = Self {
            bytes,
            magic_value: MagicValue::PE32,
        };
        out.magic_value = out.magic_value()?.value;
        Ok(out)
    }

    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.group(&self.bytes).name("Optional PE Header");

        annotator.value(self.magic_value()?).name("Magic value");
        annotator
            .value(self.linker_major_version()?)
            .name("linker_major_version");
        annotator
            .value(self.linker_minor_version()?)
            .name("linker_minor_version");
        annotator.value(self.code_size()?).name("code_size");
        annotator
            .value(self.initialized_data_size()?)
            .name("initialized_data_size");
        annotator
            .value(self.uninitialized_data_size()?)
            .name("uninitialized_data_size");
        annotator
            .value(self.rva_entry_point()?)
            .name("rva_entry_point");
        annotator
            .value(self.rva_code_section()?)
            .name("rva_code_section");
        // annotate! {rva_data_section};
        annotator.value(self.image_base()?).name("image_base");
        annotator
            .value(self.section_alignment()?)
            .name("section_alignment");
        annotator
            .value(self.file_alignment()?)
            .name("file_alignment");
        annotator.value(self.os_major()?).name("os_major");
        annotator.value(self.os_minor()?).name("os_minor");
        annotator.value(self.user_major()?).name("user_major");
        annotator.value(self.user_minor()?).name("user_minor");
        annotator.value(self.subsys_major()?).name("subsys_major");
        annotator.value(self.subsys_minor()?).name("subsys_minor");
        annotator
            .value(self.reserved_value()?)
            .name("reserved_value");
        annotator.value(self.image_size()?).name("image_size");
        annotator.value(self.header_size()?).name("header_size");
        annotator.value(self.file_checksum()?).name("file_checksum");
        annotator.value(self.sub_system()?).name("sub_system");
        annotator.value(self.dll_flags()?).name("dll_flags");
        annotator
            .value(self.stack_reserve_size()?)
            .name("stack_reserve_size");
        annotator
            .value(self.stack_commit_size()?)
            .name("stack_commit_size");
        annotator
            .value(self.heap_reserve_size()?)
            .name("heap_reserve_size");
        annotator
            .value(self.heap_commit_size()?)
            .name("heap_commit_size");
        annotator.value(self.loader_flags()?).name("loader_flags");

        let num_data_directories = self.num_data_directories()?;
        annotator
            .value(num_data_directories.clone())
            .name("Num data directories");

        for i_data_dir in 0..num_data_directories.value {
            let data_dir_kind: DataDirectoryKind = i_data_dir.try_into()?;
            let data_dir = self.data_directory(data_dir_kind)?;
            annotator.value(data_dir).name(format!("{data_dir_kind:?}"));
        }

        Ok(())
    }

    pub fn magic_value(&self) -> Result<UnpackedValue<MagicValue>, Error> {
        self.bytes
            .get_u16(0)
            .and_then(|unpacked| unpacked.try_map(MagicValue::new))
    }

    fn get_size(
        &self,
        pe32_offset: usize,
        pe32_plus_offset: usize,
    ) -> Result<UnpackedValue<u64>, Error> {
        match self.magic_value {
            MagicValue::PE32 => self
                .bytes
                .get_u32(pe32_offset)
                .map(|unpacked| unpacked.map(|value| value as u64)),
            MagicValue::PE32plus => self.bytes.get_u64(pe32_plus_offset),
        }
    }

    fn get_u32(
        &self,
        pe32_offset: usize,
        pe32_plus_offset: usize,
    ) -> Result<UnpackedValue<u32>, Error> {
        match self.magic_value {
            MagicValue::PE32 => self.bytes.get_u32(pe32_offset),
            MagicValue::PE32plus => self.bytes.get_u32(pe32_plus_offset),
        }
    }

    pub fn linker_major_version(&self) -> Result<UnpackedValue<u8>, Error> {
        self.bytes.get_u8(2)
    }

    pub fn linker_minor_version(&self) -> Result<UnpackedValue<u8>, Error> {
        self.bytes.get_u8(3)
    }

    pub fn code_size(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(4)
    }

    pub fn initialized_data_size(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(8)
    }

    pub fn uninitialized_data_size(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(12)
    }

    pub fn rva_entry_point(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(16)
    }

    pub fn rva_code_section(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(20)
    }

    pub fn rva_data_section(
        &self,
    ) -> Option<Result<UnpackedValue<u32>, Error>> {
        match self.magic_value {
            MagicValue::PE32 => Some(self.bytes.get_u32(24)),
            MagicValue::PE32plus => None,
        }
    }

    pub fn image_base(&self) -> Result<UnpackedValue<u64>, Error> {
        self.get_size(28, 24)
    }

    pub fn section_alignment(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(32)
    }

    pub fn file_alignment(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(36)
    }

    pub fn os_major(&self) -> Result<UnpackedValue<u16>, Error> {
        let unpacked = self.bytes.get_u16(40)?;
        if unpacked.value == 4 || unpacked.value == 5 {
            Ok(unpacked)
        } else {
            Err(Error::IncorrectOSMajor(unpacked.value))
        }
    }

    pub fn os_minor(&self) -> Result<UnpackedValue<u16>, Error> {
        let unpacked = self.bytes.get_u16(42)?;
        if unpacked.value == 0 {
            Ok(unpacked)
        } else {
            Err(Error::IncorrectOSMinor(unpacked.value))
        }
    }

    pub fn user_major(&self) -> Result<UnpackedValue<u16>, Error> {
        let unpacked = self.bytes.get_u16(44)?;
        if unpacked.value == 0 {
            Ok(unpacked)
        } else {
            Err(Error::IncorrectUserMajor(unpacked.value))
        }
    }

    pub fn user_minor(&self) -> Result<UnpackedValue<u16>, Error> {
        let unpacked = self.bytes.get_u16(46)?;
        if unpacked.value == 0 {
            Ok(unpacked)
        } else {
            Err(Error::IncorrectUserMinor(unpacked.value))
        }
    }

    pub fn subsys_major(&self) -> Result<UnpackedValue<u16>, Error> {
        let unpacked = self.bytes.get_u16(48)?;
        if unpacked.value == 4 || unpacked.value == 5 {
            Ok(unpacked)
        } else {
            Err(Error::IncorrectSubsysMajor(unpacked.value))
        }
    }

    pub fn subsys_minor(&self) -> Result<UnpackedValue<u16>, Error> {
        let unpacked = self.bytes.get_u16(50)?;
        if unpacked.value == 0 {
            Ok(unpacked)
        } else {
            Err(Error::IncorrectSubsysMinor(unpacked.value))
        }
    }

    pub fn reserved_value(&self) -> Result<UnpackedValue<u32>, Error> {
        let unpacked = self.bytes.get_u32(52)?;
        if unpacked.value == 0 {
            Ok(unpacked)
        } else {
            Err(Error::InvalidReservedValue)
        }
    }

    pub fn image_size(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(56)
    }

    pub fn header_size(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(60)
    }

    pub fn file_checksum(&self) -> Result<UnpackedValue<u32>, Error> {
        let unpacked = self.bytes.get_u32(64)?;
        if unpacked.value == 0 {
            Ok(unpacked)
        } else {
            Err(Error::InvalidFileChecksum)
        }
    }

    pub fn sub_system(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(68)
    }

    pub fn dll_flags(&self) -> Result<UnpackedValue<u16>, Error> {
        let unpacked = self.bytes.get_u16(70)?;
        if unpacked.value & 0x100f == 0 {
            Ok(unpacked)
        } else {
            Err(Error::InvalidDLLFlag)
        }
    }

    pub fn stack_reserve_size(&self) -> Result<UnpackedValue<u64>, Error> {
        self.get_size(72, 72)
    }

    pub fn stack_commit_size(&self) -> Result<UnpackedValue<u64>, Error> {
        self.get_size(76, 80)
    }

    pub fn heap_reserve_size(&self) -> Result<UnpackedValue<u64>, Error> {
        self.get_size(80, 88)
    }

    pub fn heap_commit_size(&self) -> Result<UnpackedValue<u64>, Error> {
        self.get_size(84, 96)
    }

    pub fn loader_flags(&self) -> Result<UnpackedValue<u32>, Error> {
        self.get_u32(88, 104)
    }

    pub fn num_data_directories(&self) -> Result<UnpackedValue<u32>, Error> {
        self.get_u32(92, 108)
    }

    pub fn data_directory(
        &self,
        dir: DataDirectoryKind,
    ) -> Result<UnpackedValue<VirtualRange>, Error> {
        let base = match self.magic_value {
            MagicValue::PE32 => 96,
            MagicValue::PE32plus => 112,
        };
        let dir_size = 8;

        self.bytes.get_virtual_range(base + dir.index() * dir_size)
    }
}

impl MagicValue {
    pub fn new(value: u16) -> Result<Self, Error> {
        match value {
            0x010b => Ok(MagicValue::PE32),
            0x020b => Ok(MagicValue::PE32plus),
            other => Err(Error::InvalidMagicValue(other)),
        }
    }
}

impl std::fmt::Display for VirtualRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "RVA {}\n{} bytes", self.rva, self.size)
    }
}

impl DataDirectoryKind {
    fn index(self) -> usize {
        match self {
            Self::ExportTable => 0,
            Self::ImportTable => 1,
            Self::ResourceTable => 2,
            Self::ExceptionTable => 3,
            Self::CertificateTable => 4,
            Self::BaseRelocationTable => 5,
            Self::Debug => 6,
            Self::Architecture => 7,
            Self::GlobalPtr => 8,
            Self::TLSTable => 9,
            Self::LoadConfigTable => 10,
            Self::BoundImportTable => 11,
            Self::ImportAddressTable => 12,
            Self::DelayImportDescriptor => 13,
            Self::ClrRuntimeHeader => 14,
            Self::Reserved15 => 15,
        }
    }
}

impl TryFrom<u32> for DataDirectoryKind {
    type Error = Error;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::ExportTable),
            1 => Ok(Self::ImportTable),
            2 => Ok(Self::ResourceTable),
            3 => Ok(Self::ExceptionTable),
            4 => Ok(Self::CertificateTable),
            5 => Ok(Self::BaseRelocationTable),
            6 => Ok(Self::Debug),
            7 => Ok(Self::Architecture),
            8 => Ok(Self::GlobalPtr),
            9 => Ok(Self::TLSTable),
            10 => Ok(Self::LoadConfigTable),
            11 => Ok(Self::BoundImportTable),
            12 => Ok(Self::ImportAddressTable),
            13 => Ok(Self::DelayImportDescriptor),
            14 => Ok(Self::ClrRuntimeHeader),
            15 => Ok(Self::Reserved15),
            other => Err(Error::InvalidDataDirectoryIndex(other)),
        }
    }
}

impl std::fmt::Display for MagicValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MagicValue::PE32 => write!(f, "PE32"),
            MagicValue::PE32plus => write!(f, "PE32+"),
        }
    }
}

impl<'a> SectionHeaderUnpacker<'a> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.name()?).name("name");
        annotator.value(self.virtual_size()?).name("virtual_size");
        annotator
            .value(self.virtual_address()?)
            .name("virtual_address");
        annotator.value(self.raw_size()?).name("raw_size");
        annotator.value(self.raw_address()?).name("raw_address");
        annotator
            .value(self.ptr_relocations()?)
            .name("ptr_relocations");
        annotator
            .value(self.ptr_line_numbers()?)
            .name("ptr_line_numbers");
        annotator
            .value(self.num_relocations()?)
            .name("num_relocations");
        annotator
            .value(self.num_line_numbers()?)
            .name("num_line_numbers");
        annotator
            .value(self.characteristics()?)
            .name("characteristics");

        Ok(())
    }

    pub fn name(&self) -> Result<UnpackedValue<&str>, Error> {
        let value =
            std::str::from_utf8(&self.bytes[0..8])?.trim_end_matches('\0');
        Ok(UnpackedValue {
            loc: self.bytes.address_range(0..8),
            value,
        })
    }

    pub fn virtual_size(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(8)
    }

    pub fn virtual_address(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(12)
    }

    pub fn virtual_range(&self) -> Result<Range<u32>, Error> {
        let start = self.virtual_address()?.value;
        let size = self.virtual_size()?.value;
        Ok(start..start + size)
    }

    pub fn raw_size(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(16)
    }

    pub fn raw_address(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(20)
    }

    pub fn raw_range(&self) -> Result<Range<u32>, Error> {
        let start = self.raw_address()?.value;
        let size = self.raw_size()?.value;
        Ok(start..start + size)
    }

    pub fn ptr_relocations(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(24).and_then(|unpacked| {
            if unpacked.value == 0 {
                Ok(unpacked)
            } else {
                Err(Error::InvalidSectionHeader)
            }
        })
    }

    pub fn ptr_line_numbers(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(28).and_then(|unpacked| {
            if unpacked.value == 0 {
                Ok(unpacked)
            } else {
                Err(Error::InvalidSectionHeader)
            }
        })
    }

    pub fn num_relocations(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(32).and_then(|unpacked| {
            if unpacked.value == 0 {
                Ok(unpacked)
            } else {
                Err(Error::InvalidSectionHeader)
            }
        })
    }

    pub fn num_line_numbers(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(34).and_then(|unpacked| {
            if unpacked.value == 0 {
                Ok(unpacked)
            } else {
                Err(Error::InvalidSectionHeader)
            }
        })
    }

    pub fn characteristics(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(36)
    }

    pub fn contains_code(&self) -> Result<bool, Error> {
        Ok(self.characteristics()?.value & 0x20 != 0)
    }

    pub fn contains_initialized_data(&self) -> Result<bool, Error> {
        Ok(self.characteristics()?.value & 0x40 != 0)
    }

    pub fn contains_uninitialized_data(&self) -> Result<bool, Error> {
        Ok(self.characteristics()?.value & 0x80 != 0)
    }

    pub fn is_executable(&self) -> Result<bool, Error> {
        Ok(self.characteristics()?.value & 0x20000000 != 0)
    }

    pub fn is_readable(&self) -> Result<bool, Error> {
        Ok(self.characteristics()?.value & 0x40000000 != 0)
    }

    pub fn is_writable(&self) -> Result<bool, Error> {
        Ok(self.characteristics()?.value & 0x80000000 != 0)
    }
}

impl<'a> ClrRuntimeHeaderUnpacker<'a> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.group(&self.bytes).name("CLR Runtime Header");

        annotator.value(self.header_size()?).name("header_size");
        annotator
            .value(self.major_runtime_version()?)
            .name("major_runtime_version");
        annotator
            .value(self.minor_runtime_version()?)
            .name("minor_runtime_version");
        annotator
            .value(self.metadata_range()?)
            .name("metadata_range");
        annotator.value(self.flags()?).name("flags");
        annotator
            .value(self.entry_point_token()?)
            .name("entry_point_token");
        annotator.value(self.resources()?).name("resources");
        annotator
            .value(self.strong_name_signature()?)
            .name("strong_name_signature");
        annotator
            .value(self.code_manager_table()?)
            .name("code_manager_table");
        annotator.value(self.vtable_fixups()?).name("vtable_fixups");
        annotator
            .value(self.export_address_table_jumps()?)
            .name("export_address_table_jumps");
        annotator
            .value(self.managed_native_header()?)
            .name("managed_native_header");

        Ok(())
    }

    fn header_size(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(0)
    }

    fn major_runtime_version(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(4)
    }

    fn minor_runtime_version(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(6)
    }

    fn metadata_range(&self) -> Result<UnpackedValue<VirtualRange>, Error> {
        self.bytes.get_virtual_range(8)
    }

    fn flags(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(16)
    }

    fn entry_point_token(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(20)
    }

    fn resources(&self) -> Result<UnpackedValue<VirtualRange>, Error> {
        self.bytes.get_virtual_range(24)
    }

    fn strong_name_signature(
        &self,
    ) -> Result<UnpackedValue<VirtualRange>, Error> {
        self.bytes.get_virtual_range(32)
    }

    fn code_manager_table(&self) -> Result<UnpackedValue<VirtualRange>, Error> {
        self.bytes.get_virtual_range(40)
    }

    fn vtable_fixups(&self) -> Result<UnpackedValue<VirtualRange>, Error> {
        self.bytes.get_virtual_range(48)
    }

    fn export_address_table_jumps(
        &self,
    ) -> Result<UnpackedValue<VirtualRange>, Error> {
        self.bytes.get_virtual_range(56)
    }

    fn managed_native_header(
        &self,
    ) -> Result<UnpackedValue<VirtualRange>, Error> {
        self.bytes.get_virtual_range(64)
    }
}

impl<'a> MetadataUnpacker<'a> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.group(&self.bytes).name("CLR Metadata");

        annotator
            .range(self.metadata_signature()?.loc)
            .name("Metadata signature");
        annotator.value(self.major_version()?).name("major_version");
        annotator.value(self.minor_version()?).name("minor_version");
        annotator.value(self.reserved()?).name("reserved");
        annotator
            .value(self.version_str_len()?)
            .name("version_str_len");
        annotator.value(self.version_str()?).name("version_str");
        annotator.value(self.flags()?).name("flags");
        annotator.value(self.num_streams()?).name("num_streams");

        self.iter_stream_header()?.try_for_each(
            |stream_header| -> Result<_, Error> {
                let header = stream_header?;
                header.collect_annotations(annotator)?;
                let start = self.bytes.start + (header.offset.value as usize);
                let size = header.size.value as usize;
                annotator
                    .group(start..start + size)
                    .name(format!("{} Stream", header.name.value));

                Ok(())
            },
        )?;

        self.tilde_stream()?.collect_annotations(annotator)?;

        Ok(())
    }

    pub fn metadata_signature(&self) -> Result<UnpackedValue<()>, Error> {
        let metadata_signature = [0x42, 0x53, 0x4A, 0x42];

        let byte_range = 0..4;
        if metadata_signature == self.bytes[byte_range.clone()] {
            Ok(UnpackedValue {
                loc: self.bytes.address_range(byte_range),
                value: (),
            })
        } else {
            Err(Error::IncorrectMetadataSignature)
        }
    }

    pub fn major_version(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(4)
    }

    pub fn minor_version(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(6)
    }

    pub fn reserved(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(8)
    }

    pub fn version_str_len(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(12)
    }

    fn padded_version_str_len(&self) -> Result<usize, Error> {
        let len = self.version_str_len()?.value as usize;
        Ok(len.div_ceil(4) * 4)
    }

    pub fn version_str(&self) -> Result<UnpackedValue<&str>, Error> {
        let len = self.version_str_len()?.value as usize;
        let start = 16;

        let padded_len = self.padded_version_str_len()?;
        let value = std::str::from_utf8(&self.bytes[start..start + len])?
            .trim_end_matches('\0');
        Ok(UnpackedValue {
            loc: self.bytes.address_range(start..start + padded_len),
            value,
        })
    }

    pub fn flags(&self) -> Result<UnpackedValue<u16>, Error> {
        let start = 16 + self.padded_version_str_len()?;
        self.bytes.get_u16(start)
    }

    pub fn num_streams(&self) -> Result<UnpackedValue<u16>, Error> {
        let start = 18 + self.padded_version_str_len()?;
        self.bytes.get_u16(start)
    }

    pub fn iter_stream_header(
        &self,
    ) -> Result<impl Iterator<Item = Result<StreamHeader, Error>> + '_, Error>
    {
        let num_streams = self.num_streams()?.value;

        let mut curr_offset = 20 + self.padded_version_str_len()?;

        let iter = (0..num_streams).map(move |_| {
            let stream_offset = self.bytes.get_u32(curr_offset)?;
            let stream_size = self.bytes.get_u32(curr_offset + 4)?;

            let name_start = curr_offset + 8;
            let name_len = self.bytes[name_start..name_start + 32]
                .iter()
                .take_while(|byte| **byte > 0)
                .count();
            assert!(name_len > 0);

            let padded_name_len = (name_len + 1).div_ceil(4) * 4;

            let stream_name = UnpackedValue {
                loc: self
                    .bytes
                    .address_range(name_start..name_start + padded_name_len),
                value: std::str::from_utf8(
                    &self.bytes[name_start..name_start + name_len],
                )?,
            };

            curr_offset += 8 + padded_name_len;

            Ok(StreamHeader {
                offset: stream_offset,
                size: stream_size,
                name: stream_name,
            })
        });

        Ok(iter)
    }

    pub fn tilde_stream(&self) -> Result<TildeStreamUnpacker, Error> {
        let mut string_stream = None;
        let mut tilde_stream = None;
        let mut blob_stream = None;

        for res in self.iter_stream_header()? {
            let header = res?;
            let offset = header.offset.value as usize;
            let size = header.size.value as usize;
            let bytes = self.bytes.subrange(offset..offset + size);

            if header.name.value == "#~" {
                tilde_stream = Some(bytes);
            } else if header.name.value == "#Strings" {
                string_stream = Some(bytes);
            } else if header.name.value == "#Blob" {
                blob_stream = Some(bytes);
            }
        }

        if tilde_stream.is_none() {
            Err(Error::MissingStream("#~"))
        } else if string_stream.is_none() {
            Err(Error::MissingStream("#Strings"))
        } else if blob_stream.is_none() {
            Err(Error::MissingStream("#Blob"))
        } else {
            Ok(TildeStreamUnpacker {
                bytes: tilde_stream.unwrap(),
                string_heap: string_stream.unwrap(),
                blob_heap: blob_stream.unwrap(),
            })
        }
    }
}

impl<'a> StreamHeader<'a> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .group(self.offset.loc.start..self.name.loc.end)
            .name("CLR Stream Header");

        annotator
            .range(self.offset.loc.clone())
            .name("Stream Offset");
        annotator.range(self.size.loc.clone()).name("Stream Size");
        annotator.range(self.name.loc.clone()).name("Stream Name");

        Ok(())
    }
}

impl<'a> TildeStreamUnpacker<'a> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.reserved_0()?).name("reserved_0");
        annotator.value(self.major_version()?).name("major_version");
        annotator.value(self.minor_version()?).name("minor_version");
        annotator.value(self.heap_sizes()?).name("heap_sizes");
        annotator.value(self.reserved_1()?).name("reserved_1");
        annotator
            .value(self.valid_table_bitfield()?)
            .name("valid_table_bitfield");
        annotator
            .value(self.sorted_table_bitfield()?)
            .name("sorted_table_bitfield");

        let sizes = self.metadata_sizes()?;

        self.iter_num_rows()?
            .try_for_each(|res| -> Result<_, Error> {
                let (value, kind) = res?;
                annotator.value(value).name(format!("Num {kind:?} rows"));
                annotator
                    .group(self.metadata_table_bytes(kind, &sizes)?)
                    .name(format!("{kind:?} metadata table"));
                Ok(())
            })?;

        self.module_table(&sizes)?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.type_ref_table(&sizes)?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        let field_table = self.field_table(&sizes)?;
        let method_table = self.method_def_table(&sizes)?;
        let param_table = self.param_table(&sizes)?;
        self.type_def_table(&sizes)?
            .iter_rows()
            .try_for_each(|row| {
                row.collect_annotations(
                    annotator,
                    &field_table,
                    &method_table,
                    &param_table,
                )
            })?;

        self.interface_impl_table(&sizes)?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.member_ref_table(&sizes)?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        Ok(())
    }

    pub fn reserved_0(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(0)
    }

    pub fn major_version(&self) -> Result<UnpackedValue<u8>, Error> {
        self.bytes.get_u8(4)
    }

    pub fn minor_version(&self) -> Result<UnpackedValue<u8>, Error> {
        self.bytes.get_u8(5)
    }

    pub fn heap_sizes(&self) -> Result<UnpackedValue<HeapSizes>, Error> {
        let UnpackedValue { loc, value } = self.bytes.get_u8(6)?;
        let heap_sizes = HeapSizes {
            string_stream_uses_u32_addr: value & 0x1 > 0,
            guid_stream_uses_u32_addr: value & 0x2 > 0,
            blob_stream_uses_u32_addr: value & 0x4 > 0,
        };

        Ok(UnpackedValue {
            value: heap_sizes,
            loc,
        })
    }

    pub fn reserved_1(&self) -> Result<UnpackedValue<u8>, Error> {
        self.bytes.get_u8(7)
    }

    /// Bitfield indicating which tables are present.
    pub fn valid_table_bitfield(&self) -> Result<UnpackedValue<u64>, Error> {
        self.bytes.get_u64(8)
    }

    /// Bitfield indicating which tables are sorted.  Should have
    /// exactly 14 entries, matching the tables listed in section
    /// II.22 of ECMA-335.
    pub fn sorted_table_bitfield(&self) -> Result<UnpackedValue<u64>, Error> {
        self.bytes.get_u64(16)
    }

    pub fn iter_num_rows(
        &self,
    ) -> Result<
        impl Iterator<
                Item = Result<(UnpackedValue<u32>, MetadataTableKind), Error>,
            > + '_,
        Error,
    > {
        let bitfield = self.valid_table_bitfield()?.value;

        let iter = (0..64)
            .filter(move |i_bit| bitfield & (1 << i_bit) > 0)
            .scan(24, |offset, i_bit| {
                let row_offset = *offset;
                *offset += 4;

                let num_rows = match self.bytes.get_u32(row_offset) {
                    Ok(val) => val,
                    Err(err) => return Some(Err(err)),
                };
                let kind = match MetadataTableKind::from_bit_index(i_bit) {
                    Ok(val) => val,
                    Err(err) => return Some(Err(err)),
                };
                Some(Ok((num_rows, kind)))
            });

        Ok(iter)
    }

    pub fn metadata_sizes(&self) -> Result<MetadataSizes, Error> {
        let heap_sizes = self.heap_sizes()?.value;
        let mut metadata_sizes = MetadataSizes {
            heap_sizes,
            table_sizes: [0; 38],
        };

        for res in self.iter_num_rows()? {
            let (num_rows, kind) = res?;
            metadata_sizes[kind] = num_rows.value;
        }

        Ok(metadata_sizes)
    }

    fn metadata_table_bytes<'b>(
        &'b self,
        kind: MetadataTableKind,
        sizes: &'b MetadataSizes,
    ) -> Result<ByteRange<'b>, Error> {
        let start = self
            .iter_num_rows()?
            .map(|res| {
                res.map(|(rows, other_kind)| {
                    let rows = rows.value as usize;
                    let table_offset = if other_kind.as_metadata_sizes_index()
                        < kind.as_metadata_sizes_index()
                    {
                        rows * other_kind.bytes_per_row(sizes)
                    } else {
                        0
                    };
                    table_offset + 4
                })
            })
            .try_fold(24, |sum, res_bytes| {
                res_bytes.map(|bytes| bytes + sum)
            })?;

        let num_rows = sizes.num_rows(kind);
        let bytes_per_row = kind.bytes_per_row(sizes);

        let bytes =
            self.bytes.subrange(start..start + num_rows * bytes_per_row);

        Ok(bytes)
    }

    fn get_table<'b, Unpacker>(
        &'b self,
        sizes: &'b MetadataSizes,
    ) -> Result<MetadataTableUnpacker<'b, Unpacker>, Error>
    where
        Unpacker: RowUnpacker,
    {
        let bytes = self.metadata_table_bytes(Unpacker::KIND, sizes)?;
        Ok(MetadataTableUnpacker {
            bytes,
            sizes,
            string_heap: self.string_heap.clone(),
            blob_heap: self.blob_heap.clone(),
            row_unpacker: PhantomData,
        })
    }

    pub fn module_table<'b>(
        &'b self,
        sizes: &'b MetadataSizes,
    ) -> Result<MetadataTableUnpacker<'b, ModuleRowUnpacker>, Error> {
        self.get_table(sizes)
    }

    pub fn type_ref_table<'b>(
        &'b self,
        sizes: &'b MetadataSizes,
    ) -> Result<MetadataTableUnpacker<'b, TypeRefRowUnpacker>, Error> {
        self.get_table(sizes)
    }

    pub fn type_def_table<'b>(
        &'b self,
        sizes: &'b MetadataSizes,
    ) -> Result<MetadataTableUnpacker<'b, TypeDefRowUnpacker>, Error> {
        self.get_table(sizes)
    }

    pub fn field_table<'b>(
        &'b self,
        sizes: &'b MetadataSizes,
    ) -> Result<MetadataTableUnpacker<'b, FieldRowUnpacker>, Error> {
        self.get_table(sizes)
    }

    pub fn method_def_table<'b>(
        &'b self,
        sizes: &'b MetadataSizes,
    ) -> Result<MetadataTableUnpacker<'b, MethodDefRowUnpacker>, Error> {
        self.get_table(sizes)
    }

    pub fn param_table<'b>(
        &'b self,
        sizes: &'b MetadataSizes,
    ) -> Result<MetadataTableUnpacker<'b, ParamRowUnpacker>, Error> {
        self.get_table(sizes)
    }

    pub fn interface_impl_table<'b>(
        &'b self,
        sizes: &'b MetadataSizes,
    ) -> Result<MetadataTableUnpacker<'b, InterfaceImplRowUnpacker>, Error>
    {
        self.get_table(sizes)
    }

    pub fn member_ref_table<'b>(
        &'b self,
        sizes: &'b MetadataSizes,
    ) -> Result<MetadataTableUnpacker<'b, MemberRefRowUnpacker>, Error> {
        self.get_table(sizes)
    }
}

impl std::fmt::Display for HeapSizes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "HeapSizes{{")?;

        let mut need_comma = false;
        if self.string_stream_uses_u32_addr {
            write!(f, "#Strings")?;
            need_comma = true;
        }

        if self.guid_stream_uses_u32_addr {
            if need_comma {
                write!(f, ", ")?;
            }
            write!(f, "#GUID")?;
            need_comma = true;
        }

        if self.blob_stream_uses_u32_addr {
            if need_comma {
                write!(f, ", ")?;
            }
            write!(f, "#Blob")?;
        }

        write!(f, "}}")
    }
}

impl std::fmt::Display for MetadataTableKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl MetadataTableKind {
    #[allow(dead_code)]
    const TYPE_DEF_OR_REF: [Self; 3] =
        [Self::TypeDef, Self::TypeRef, Self::TypeSpec];

    #[allow(dead_code)]
    const HAS_CONSTANT: [Self; 3] = [Self::Field, Self::Param, Self::Property];

    #[allow(dead_code)]
    const HAS_CUSTOM_ATTRIBUTE: [Self; 22] = [
        Self::MethodDef,
        Self::Field,
        Self::TypeRef,
        Self::TypeDef,
        Self::Param,
        Self::InterfaceImpl,
        Self::MemberRef,
        Self::Module,
        // This option is listed as "Permission" in II.24.2.6 of
        // ECMA-335, but no such metadata table exists.  It looks like
        // it might be a holdover from an earlier draft.  For now,
        // assuming that it refers to the "DeclSecurity" table, which
        // handles object permissions.
        Self::DeclSecurity,
        Self::Property,
        Self::Event,
        Self::StandAloneSig,
        Self::ModuleRef,
        Self::TypeSpec,
        Self::Assembly,
        Self::AssemblyRef,
        Self::File,
        Self::ExportedType,
        Self::ManifestResource,
        Self::GenericParam,
        Self::GenericParamConstraint,
        Self::MethodSpec,
    ];

    #[allow(dead_code)]
    const HAS_FIELD_MARSHAL: [Self; 2] = [Self::Field, Self::Param];

    #[allow(dead_code)]
    const HAS_DECL_SECURITY: [Self; 3] =
        [Self::TypeDef, Self::MethodDef, Self::Assembly];

    #[allow(dead_code)]
    const MEMBER_REF_PARENT: [Self; 5] = [
        Self::TypeDef,
        Self::TypeRef,
        Self::ModuleRef,
        Self::MethodDef,
        Self::TypeSpec,
    ];

    #[allow(dead_code)]
    const HAS_SEMANTICS: [Self; 2] = [Self::Event, Self::Property];

    #[allow(dead_code)]
    const METHOD_DEF_OR_REF: [Self; 2] = [Self::MethodDef, Self::MemberRef];

    #[allow(dead_code)]
    const MEMBER_FORWARDED: [Self; 2] = [Self::Field, Self::MethodDef];

    #[allow(dead_code)]
    const IMPLEMENTATION: [Self; 3] =
        [Self::File, Self::AssemblyRef, Self::ExportedType];

    // This is the only one of the type-tag sets that has unused
    // values.  Once I get around to unpacking it, will need to adjust
    // the "coded_index_size" and "get_coded_index" methods to handle
    // it.
    #[allow(dead_code)]
    const CUSTOM_ATTRIBUTE_TYPE: [Option<Self>; 5] = [
        None,
        None,
        Some(Self::MethodDef),
        Some(Self::MemberRef),
        None,
    ];

    const RESOLUTION_SCOPE: [Self; 4] = [
        Self::Module,
        Self::ModuleRef,
        Self::AssemblyRef,
        Self::TypeRef,
    ];

    #[allow(dead_code)]
    const TYPE_OR_METHOD_DEF: [Self; 2] = [Self::TypeDef, Self::MethodDef];

    fn from_bit_index(bit: u8) -> Result<Self, Error> {
        match bit {
            0x00 => Ok(Self::Module),
            0x01 => Ok(Self::TypeRef),
            0x02 => Ok(Self::TypeDef),
            0x04 => Ok(Self::Field),
            0x06 => Ok(Self::MethodDef),
            0x08 => Ok(Self::Param),
            0x09 => Ok(Self::InterfaceImpl),
            0x0A => Ok(Self::MemberRef),
            0x0B => Ok(Self::Constant),
            0x0C => Ok(Self::CustomAttribute),
            0x0D => Ok(Self::FieldMarshal),
            0x0E => Ok(Self::DeclSecurity),
            0x0F => Ok(Self::ClassLayout),
            0x10 => Ok(Self::FieldLayout),
            0x11 => Ok(Self::StandAloneSig),
            0x12 => Ok(Self::EventMap),
            0x14 => Ok(Self::Event),
            0x15 => Ok(Self::PropertyMap),
            0x17 => Ok(Self::Property),
            0x18 => Ok(Self::MethodSemantics),
            0x19 => Ok(Self::MethodImpl),
            0x1A => Ok(Self::ModuleRef),
            0x1B => Ok(Self::TypeSpec),
            0x1C => Ok(Self::ImplMap),
            0x1D => Ok(Self::FieldRVA),
            0x20 => Ok(Self::Assembly),
            0x21 => Ok(Self::AssemblyProcessor),
            0x22 => Ok(Self::AssemblyOS),
            0x23 => Ok(Self::AssemblyRef),
            0x24 => Ok(Self::AssemblyRefProcessor),
            0x25 => Ok(Self::AssemblyRefOS),
            0x26 => Ok(Self::File),
            0x27 => Ok(Self::ExportedType),
            0x28 => Ok(Self::ManifestResource),
            0x29 => Ok(Self::NestedClass),
            0x2A => Ok(Self::GenericParam),
            0x2B => Ok(Self::MethodSpec),
            0x2C => Ok(Self::GenericParamConstraint),
            _ => Err(Error::InvalidMetadataTable(bit)),
        }
    }

    fn as_metadata_sizes_index(self) -> usize {
        match self {
            MetadataTableKind::Module => 0,
            MetadataTableKind::TypeRef => 1,
            MetadataTableKind::TypeDef => 2,
            MetadataTableKind::Field => 3,
            MetadataTableKind::MethodDef => 4,
            MetadataTableKind::Param => 5,
            MetadataTableKind::InterfaceImpl => 6,
            MetadataTableKind::MemberRef => 7,
            MetadataTableKind::Constant => 8,
            MetadataTableKind::CustomAttribute => 9,
            MetadataTableKind::FieldMarshal => 10,
            MetadataTableKind::DeclSecurity => 11,
            MetadataTableKind::ClassLayout => 12,
            MetadataTableKind::FieldLayout => 13,
            MetadataTableKind::StandAloneSig => 14,
            MetadataTableKind::EventMap => 15,
            MetadataTableKind::Event => 16,
            MetadataTableKind::PropertyMap => 17,
            MetadataTableKind::Property => 18,
            MetadataTableKind::MethodSemantics => 19,
            MetadataTableKind::MethodImpl => 20,
            MetadataTableKind::ModuleRef => 21,
            MetadataTableKind::TypeSpec => 22,
            MetadataTableKind::ImplMap => 23,
            MetadataTableKind::FieldRVA => 24,
            MetadataTableKind::Assembly => 25,
            MetadataTableKind::AssemblyProcessor => 26,
            MetadataTableKind::AssemblyOS => 27,
            MetadataTableKind::AssemblyRef => 28,
            MetadataTableKind::AssemblyRefProcessor => 29,
            MetadataTableKind::AssemblyRefOS => 30,
            MetadataTableKind::File => 31,
            MetadataTableKind::ExportedType => 32,
            MetadataTableKind::ManifestResource => 33,
            MetadataTableKind::NestedClass => 34,
            MetadataTableKind::GenericParam => 35,
            MetadataTableKind::MethodSpec => 36,
            MetadataTableKind::GenericParamConstraint => 37,
        }
    }

    fn bytes_per_row(self, sizes: &MetadataSizes) -> usize {
        match self {
            MetadataTableKind::Module => {
                2 + sizes.str_index_size() + 3 * sizes.guid_index_size()
            }
            MetadataTableKind::TypeRef => {
                sizes.coded_index_size(MetadataTableKind::RESOLUTION_SCOPE)
                    + 2 * sizes.str_index_size()
            }
            MetadataTableKind::TypeDef => {
                4 + 2 * sizes.str_index_size()
                    + sizes.coded_index_size(MetadataTableKind::TYPE_DEF_OR_REF)
                    + sizes.index_size(MetadataTableKind::Field)
                    + sizes.index_size(MetadataTableKind::MethodDef)
            }
            MetadataTableKind::Field => {
                2 + sizes.str_index_size() + sizes.blob_index_size()
            }
            MetadataTableKind::MethodDef => {
                8 + sizes.str_index_size()
                    + sizes.blob_index_size()
                    + sizes.index_size(MetadataTableKind::Param)
            }
            MetadataTableKind::Param => 4 + sizes.str_index_size(),
            MetadataTableKind::InterfaceImpl => {
                sizes.index_size(MetadataTableKind::TypeDef)
                    + sizes.coded_index_size(MetadataTableKind::TYPE_DEF_OR_REF)
            }
            MetadataTableKind::MemberRef => {
                sizes.coded_index_size(MetadataTableKind::MEMBER_REF_PARENT)
                    + sizes.str_index_size()
                    + sizes.blob_index_size()
            }
            MetadataTableKind::Constant => {
                2 + sizes.coded_index_size(MetadataTableKind::HAS_CONSTANT)
                    + sizes.blob_index_size()
            }
            MetadataTableKind::CustomAttribute => {
                2 * sizes
                    .coded_index_size(MetadataTableKind::HAS_CUSTOM_ATTRIBUTE)
                    + sizes.blob_index_size()
            }
            MetadataTableKind::FieldMarshal => {
                sizes.coded_index_size(MetadataTableKind::HAS_FIELD_MARSHAL)
                    + sizes.blob_index_size()
            }
            MetadataTableKind::DeclSecurity => {
                2 + sizes.coded_index_size(MetadataTableKind::HAS_DECL_SECURITY)
                    + sizes.blob_index_size()
            }
            MetadataTableKind::ClassLayout => {
                2 + 4 + sizes.index_size(MetadataTableKind::TypeDef)
            }
            MetadataTableKind::FieldLayout => {
                4 + sizes.index_size(MetadataTableKind::Field)
            }
            MetadataTableKind::StandAloneSig => sizes.blob_index_size(),
            MetadataTableKind::EventMap => {
                sizes.index_size(MetadataTableKind::TypeDef)
                    + sizes.index_size(MetadataTableKind::Event)
            }
            MetadataTableKind::Event => {
                2 + sizes.str_index_size()
                    + sizes.coded_index_size(MetadataTableKind::TYPE_DEF_OR_REF)
            }
            MetadataTableKind::PropertyMap => {
                sizes.index_size(MetadataTableKind::TypeDef)
                    + sizes.index_size(MetadataTableKind::Property)
            }
            MetadataTableKind::Property => {
                2 + sizes.str_index_size() + sizes.blob_index_size()
            }
            MetadataTableKind::MethodSemantics => {
                2 + sizes.index_size(MetadataTableKind::MethodDef)
                    + sizes.coded_index_size(MetadataTableKind::HAS_SEMANTICS)
            }
            MetadataTableKind::MethodImpl => {
                sizes.index_size(MetadataTableKind::TypeDef)
                    + 2 * sizes
                        .coded_index_size(MetadataTableKind::METHOD_DEF_OR_REF)
            }
            MetadataTableKind::ModuleRef => sizes.str_index_size(),
            MetadataTableKind::TypeSpec => sizes.blob_index_size(),
            MetadataTableKind::ImplMap => {
                2 + sizes.coded_index_size(MetadataTableKind::MEMBER_FORWARDED)
                    + sizes.str_index_size()
                    + sizes.index_size(MetadataTableKind::MethodDef)
            }
            MetadataTableKind::FieldRVA => {
                4 + sizes.index_size(MetadataTableKind::Field)
            }
            MetadataTableKind::Assembly => {
                4 + 2 * 4
                    + 4
                    + sizes.blob_index_size()
                    + 2 * sizes.str_index_size()
            }
            MetadataTableKind::AssemblyProcessor => 4,
            MetadataTableKind::AssemblyOS => 4 * 3,
            MetadataTableKind::AssemblyRef => {
                2 * 4
                    + 4
                    + 2 * sizes.blob_index_size()
                    + 2 * sizes.str_index_size()
            }
            MetadataTableKind::AssemblyRefProcessor => {
                4 + sizes.index_size(MetadataTableKind::AssemblyRef)
            }
            MetadataTableKind::AssemblyRefOS => {
                4 * 3 + sizes.index_size(MetadataTableKind::AssemblyRef)
            }
            MetadataTableKind::File => {
                4 + sizes.str_index_size() + sizes.blob_index_size()
            }
            MetadataTableKind::ExportedType => {
                4 + 4
                    + sizes.index_size(MetadataTableKind::TypeRef)
                    + 2 * sizes.str_index_size()
                    + sizes.coded_index_size(MetadataTableKind::IMPLEMENTATION)
            }
            MetadataTableKind::ManifestResource => {
                4 + 4
                    + sizes.str_index_size()
                    + sizes.coded_index_size(MetadataTableKind::IMPLEMENTATION)
            }
            MetadataTableKind::NestedClass => {
                2 * sizes.index_size(MetadataTableKind::TypeDef)
            }
            MetadataTableKind::GenericParam => {
                2 + 2
                    + sizes
                        .coded_index_size(MetadataTableKind::TYPE_OR_METHOD_DEF)
                    + sizes.str_index_size()
            }
            MetadataTableKind::MethodSpec => {
                sizes.coded_index_size(MetadataTableKind::METHOD_DEF_OR_REF)
                    + sizes.blob_index_size()
            }
            MetadataTableKind::GenericParamConstraint => {
                sizes.index_size(MetadataTableKind::GenericParam)
                    + sizes.coded_index_size(MetadataTableKind::TYPE_DEF_OR_REF)
            }
        }
    }
}

impl std::ops::Index<MetadataTableKind> for MetadataSizes {
    type Output = u32;

    fn index(&self, index: MetadataTableKind) -> &Self::Output {
        &self.table_sizes[index.as_metadata_sizes_index()]
    }
}

impl std::ops::IndexMut<MetadataTableKind> for MetadataSizes {
    fn index_mut(&mut self, index: MetadataTableKind) -> &mut Self::Output {
        &mut self.table_sizes[index.as_metadata_sizes_index()]
    }
}

impl MetadataSizes {
    fn num_rows(&self, table: MetadataTableKind) -> usize {
        self[table] as usize
    }

    fn index_size(&self, table: MetadataTableKind) -> usize {
        self.coded_index_size([table])
    }

    fn get_index(
        &self,
        kind: MetadataTableKind,
        bytes: &ByteRange,
        offset: impl NormalizeOffset,
    ) -> Result<UnpackedValue<usize>, Error> {
        let num_rows = self[kind];
        Self::get_heap_index(bytes, offset, num_rows >= 65536)
    }

    fn coded_index_bit_widths<const N: usize>(
        &self,
        tables: [MetadataTableKind; N],
    ) -> (usize, usize) {
        assert!(N > 0, "Must have at least one table");
        let table_bits = N.next_power_of_two().ilog2();
        let max_rows = tables.into_iter().map(|kind| self[kind]).max().unwrap();
        let row_bits = (max_rows + 1).next_power_of_two().ilog2();

        (table_bits as usize, row_bits as usize)
    }

    fn coded_index_size<const N: usize>(
        &self,
        tables: [MetadataTableKind; N],
    ) -> usize {
        let (table_bits, row_bits) = self.coded_index_bit_widths(tables);

        if table_bits + row_bits <= 16 {
            2
        } else {
            4
        }
    }

    fn get_coded_index<const N: usize>(
        &self,
        tables: [MetadataTableKind; N],
        bytes: &ByteRange,
        offset: usize,
    ) -> Result<UnpackedValue<MetadataIndex>, Error> {
        let (table_bits, row_bits) = self.coded_index_bit_widths(tables);
        let UnpackedValue { value, loc } =
            Self::get_heap_index(bytes, offset, table_bits + row_bits > 16)?;

        let table_mask = (1 << table_bits) - 1;
        let table_index = value & table_mask;

        let table = if table_index < N {
            tables[table_index]
        } else {
            return Err(Error::InvalidCodedIndex {
                index: table_index,
                num_tables: N,
            });
        };
        let index = value >> table_bits;

        Ok(UnpackedValue {
            value: MetadataIndex { table, index },
            loc,
        })
    }

    fn get_heap_index(
        bytes: &ByteRange,
        offset: impl NormalizeOffset,
        is_u32_addr: bool,
    ) -> Result<UnpackedValue<usize>, Error> {
        if is_u32_addr {
            Ok(bytes.get_u32(offset)?.map(|val| val as usize))
        } else {
            Ok(bytes.get_u16(offset)?.map(|val| val as usize))
        }
    }

    fn get_str_index(
        &self,
        bytes: &ByteRange,
        offset: usize,
    ) -> Result<UnpackedValue<usize>, Error> {
        Self::get_heap_index(
            bytes,
            offset,
            self.heap_sizes.string_stream_uses_u32_addr,
        )
    }

    fn get_guid_index(
        &self,
        bytes: &ByteRange,
        offset: usize,
    ) -> Result<UnpackedValue<usize>, Error> {
        Self::get_heap_index(
            bytes,
            offset,
            self.heap_sizes.guid_stream_uses_u32_addr,
        )
    }

    fn get_blob_index(
        &self,
        bytes: &ByteRange,
        offset: usize,
    ) -> Result<UnpackedValue<usize>, Error> {
        Self::get_heap_index(
            bytes,
            offset,
            self.heap_sizes.blob_stream_uses_u32_addr,
        )
    }

    fn str_index_size(&self) -> usize {
        if self.heap_sizes.string_stream_uses_u32_addr {
            4
        } else {
            2
        }
    }

    fn guid_index_size(&self) -> usize {
        if self.heap_sizes.guid_stream_uses_u32_addr {
            4
        } else {
            2
        }
    }

    fn blob_index_size(&self) -> usize {
        if self.heap_sizes.blob_stream_uses_u32_addr {
            4
        } else {
            2
        }
    }
}

impl std::fmt::Display for MetadataIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}[{}]", self.table, self.index)
    }
}

impl<'a, Unpacker> MetadataTableUnpacker<'a, Unpacker> {
    pub fn iter_rows<'b>(
        &'b self,
    ) -> impl Iterator<Item = MetadataRowUnpacker<'b, Unpacker>> + 'b
    where
        'a: 'b,
        Unpacker: RowUnpacker,
    {
        let kind = Unpacker::KIND;
        let num_rows = self.sizes.num_rows(kind);
        let bytes_per_row = kind.bytes_per_row(self.sizes);
        (0..num_rows).map(move |i_row| {
            let bytes = self
                .bytes
                .subrange(i_row * bytes_per_row..(i_row + 1) * bytes_per_row);
            let next_row_bytes = (i_row + 1 < num_rows).then(|| {
                self.bytes.subrange(
                    (i_row + 1) * bytes_per_row..(i_row + 2) * bytes_per_row,
                )
            });
            MetadataRowUnpacker {
                bytes,
                next_row_bytes,
                sizes: self.sizes,
                string_heap: self.string_heap.clone(),
                blob_heap: self.blob_heap.clone(),
                row_unpacker: self.row_unpacker,
            }
        })
    }

    pub fn address_range(
        &self,
        indices: Range<usize>,
    ) -> Result<Range<Pointer>, Error>
    where
        Unpacker: RowUnpacker,
    {
        let kind = Unpacker::KIND;
        let num_rows = self.sizes[kind] as usize;

        let bytes_per_row = kind.bytes_per_row(self.sizes) as usize;

        if indices.end <= num_rows {
            Ok(self.bytes.address_range(
                indices.start * bytes_per_row..indices.end * bytes_per_row,
            ))
        } else {
            Err(Error::InvalidMetadataTableIndex {
                kind,
                index: indices.end,
                num_rows,
            })
        }
    }

    pub fn get_row<'b>(
        &'b self,
        index: usize,
    ) -> Result<MetadataRowUnpacker<'a, Unpacker>, Error>
    where
        Unpacker: RowUnpacker,
    {
        let kind = Unpacker::KIND;
        let num_rows = self.sizes.num_rows(kind);

        if index < num_rows {
            let bytes_per_row = kind.bytes_per_row(self.sizes);
            let bytes = self
                .bytes
                .subrange(index * bytes_per_row..(index + 1) * bytes_per_row);
            let next_row_bytes = (index + 1 < num_rows).then(|| {
                self.bytes.subrange(
                    (index + 1) * bytes_per_row..(index + 2) * bytes_per_row,
                )
            });
            Ok(MetadataRowUnpacker {
                bytes,
                next_row_bytes,
                sizes: self.sizes,
                string_heap: self.string_heap.clone(),
                blob_heap: self.blob_heap.clone(),
                row_unpacker: self.row_unpacker,
            })
        } else {
            Err(Error::InvalidMetadataTableIndex {
                kind,
                index,
                num_rows,
            })
        }
    }
}

impl<'a, Unpacker> MetadataRowUnpacker<'a, Unpacker> {
    pub fn get_blob<'b>(&'b self, index: usize) -> Result<ByteRange, Error> {
        let byte: u8 = self.blob_heap[index];

        let leading_ones = byte.leading_ones();
        let (size_size, size) = match leading_ones {
            0 => {
                let size: usize = (byte & 0x7f).into();
                (1, size)
            }
            1 => {
                let high: usize = (byte & 0x3f).into();
                let low: usize = self.blob_heap[index + 1].into();
                (2, (high << 8) + low)
            }
            2 => {
                let high: usize = (byte & 0x1f).into();
                let mid1: usize = self.blob_heap[index + 1].into();
                let mid2: usize = self.blob_heap[index + 2].into();
                let low: usize = self.blob_heap[index + 3].into();
                (4, (high << 24) + (mid1 << 16) + (mid2 << 8) + low)
            }
            _ => {
                return Err(Error::InvalidBlobHeader { leading_ones });
            }
        };

        let bytes = self.blob_heap.subrange(index..index + size_size + size);
        Ok(bytes)
    }
}

impl RowUnpacker for ModuleRowUnpacker {
    const KIND: MetadataTableKind = MetadataTableKind::Module;
}
impl<'a> MetadataRowUnpacker<'a, ModuleRowUnpacker> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.generation()?).name("generation");
        {
            let index = self.name_index()?;
            let name = self.name()?.value;
            annotator
                .range(index.loc)
                .name("Name")
                .value(format!("{}\n{}", index.value, name));
        }
        annotator.value(self.module_id()?).name("module_id");
        annotator.value(self.enc_id()?).name("enc_id");
        annotator.value(self.enc_base_id()?).name("enc_base_id");

        Ok(())
    }

    pub fn generation(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(0)
    }

    pub fn name_index(&self) -> Result<UnpackedValue<usize>, Error> {
        self.sizes.get_str_index(&self.bytes, 2)
    }

    pub fn name(&self) -> Result<UnpackedValue<&str>, Error> {
        let index = self.name_index()?.value;
        self.string_heap.get_null_terminated(index)
    }

    pub fn module_id(&self) -> Result<UnpackedValue<usize>, Error> {
        self.sizes
            .get_str_index(&self.bytes, 2 + self.sizes.str_index_size())
    }

    pub fn enc_id(&self) -> Result<UnpackedValue<usize>, Error> {
        self.sizes.get_guid_index(
            &self.bytes,
            2 + self.sizes.str_index_size() + self.sizes.guid_index_size(),
        )
    }

    pub fn enc_base_id(&self) -> Result<UnpackedValue<usize>, Error> {
        self.sizes.get_guid_index(
            &self.bytes,
            2 + self.sizes.str_index_size() + 2 * self.sizes.guid_index_size(),
        )
    }
}

impl RowUnpacker for TypeRefRowUnpacker {
    const KIND: MetadataTableKind = MetadataTableKind::TypeRef;
}
impl<'a> MetadataRowUnpacker<'a, TypeRefRowUnpacker> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.resolution_scope()?)
            .name("resolution_scope");

        {
            let index = self.type_name_index()?;
            let name = self.type_name()?.value;
            annotator
                .range(index.loc)
                .name("Type name")
                .value(format!("{}\n{}", index.value, name));
        }
        {
            let index = self.type_namespace_index()?;
            let name = self.type_namespace()?.value;
            annotator
                .range(index.loc)
                .name("Type namespace")
                .value(format!("{}\n{}", index.value, name));
        }

        Ok(())
    }

    fn resolution_scope(&self) -> Result<UnpackedValue<MetadataIndex>, Error> {
        self.sizes.get_coded_index(
            MetadataTableKind::RESOLUTION_SCOPE,
            &self.bytes,
            0,
        )
    }

    fn type_name_index(&self) -> Result<UnpackedValue<usize>, Error> {
        let offset = self
            .sizes
            .coded_index_size(MetadataTableKind::RESOLUTION_SCOPE);
        self.sizes.get_str_index(&self.bytes, offset)
    }

    fn type_name(&self) -> Result<UnpackedValue<&str>, Error> {
        let index = self.type_name_index()?.value;
        self.string_heap.get_null_terminated(index)
    }

    fn type_namespace_index(&self) -> Result<UnpackedValue<usize>, Error> {
        let offset = self
            .sizes
            .coded_index_size(MetadataTableKind::RESOLUTION_SCOPE)
            + self.sizes.str_index_size();
        self.sizes.get_str_index(&self.bytes, offset)
    }

    fn type_namespace(&self) -> Result<UnpackedValue<&str>, Error> {
        let index = self.type_namespace_index()?.value;
        self.string_heap.get_null_terminated(index)
    }
}

impl RowUnpacker for TypeDefRowUnpacker {
    const KIND: MetadataTableKind = MetadataTableKind::TypeDef;
}

impl<'a> MetadataRowUnpacker<'a, TypeDefRowUnpacker> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
        field_table: &MetadataTableUnpacker<FieldRowUnpacker>,
        method_def_table: &MetadataTableUnpacker<MethodDefRowUnpacker>,
        param_table: &MetadataTableUnpacker<ParamRowUnpacker>,
    ) -> Result<(), Error> {
        annotator.value(self.flags()?).name("flags");
        let type_name = {
            let index = self.type_name_index()?;
            let name = self.type_name()?.value;
            annotator
                .range(index.loc)
                .name("Type name")
                .value(format!("{}\n{}", index.value, name));
            name
        };
        let type_namespace = {
            let index = self.type_namespace_index()?;
            let namespace = self.type_namespace()?.value;
            annotator
                .range(index.loc)
                .name("Type namespace")
                .value(format!("{}\n{}", index.value, namespace));
            namespace
        };

        annotator.value(self.extends()?).name("extends");
        let field_indices = self.field_indices()?;
        annotator
            .range(field_indices.loc)
            .name("field_indices")
            .value(format!(
                "{}..{}",
                field_indices.value.start, field_indices.value.end
            ));

        annotator
            .group(field_table.address_range(field_indices.value.clone())?)
            .name(format!("Class '{type_name}'"));

        field_indices.value.clone().try_for_each(
            |field_index| -> Result<_, Error> {
                let field = field_table.get_row(field_index)?;
                field.collect_annotations(
                    annotator,
                    type_name,
                    type_namespace,
                )?;
                Ok(())
            },
        )?;

        let method_indices = self.method_indices()?;
        annotator
            .range(method_indices.loc)
            .name("method_indices")
            .value(format!(
                "{}..{}",
                method_indices.value.start, method_indices.value.end
            ));

        annotator
            .group(
                method_def_table.address_range(method_indices.value.clone())?,
            )
            .name(format!("Class '{type_name}'"));

        method_indices.value.clone().try_for_each(
            |method_index| -> Result<_, Error> {
                let method = method_def_table.get_row(method_index)?;
                method.collect_annotations(
                    annotator,
                    type_name,
                    type_namespace,
                    param_table,
                )?;
                Ok(())
            },
        )?;

        {
            let method_range = method_indices.value;
            let param_start = method_def_table
                .get_row(method_range.start)?
                .param_indices()?
                .value
                .start;
            let param_end = if method_range.end
                == self.sizes.num_rows(MetadataTableKind::MethodDef)
            {
                self.sizes.num_rows(MetadataTableKind::Param)
            } else {
                method_def_table
                    .get_row(method_range.end)?
                    .param_indices()?
                    .value
                    .end
            };
            annotator
                .group(param_table.address_range(param_start..param_end)?)
                .name(format!("Class '{type_name}'"));
        }

        Ok(())
    }

    fn flags(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(0)
    }

    fn type_name_index(&self) -> Result<UnpackedValue<usize>, Error> {
        self.sizes.get_str_index(&self.bytes, 4)
    }

    fn type_name(&self) -> Result<UnpackedValue<&str>, Error> {
        let index = self.type_name_index()?.value;
        self.string_heap.get_null_terminated(index)
    }

    fn type_namespace_index(&self) -> Result<UnpackedValue<usize>, Error> {
        let offset = 4 + self.sizes.str_index_size();
        self.sizes.get_str_index(&self.bytes, offset)
    }

    fn type_namespace(&self) -> Result<UnpackedValue<&str>, Error> {
        let index = self.type_namespace_index()?.value;
        self.string_heap.get_null_terminated(index)
    }

    fn extends(&self) -> Result<UnpackedValue<MetadataIndex>, Error> {
        let offset = 4 + 2 * self.sizes.str_index_size();
        self.sizes.get_coded_index(
            MetadataTableKind::TYPE_DEF_OR_REF,
            &self.bytes,
            offset,
        )
    }

    fn field_indices(&self) -> Result<UnpackedValue<Range<usize>>, Error> {
        let offset = 4
            + 2 * self.sizes.str_index_size()
            + self
                .sizes
                .coded_index_size(MetadataTableKind::TYPE_DEF_OR_REF);

        let UnpackedValue { value: start, loc } = self.sizes.get_index(
            MetadataTableKind::Field,
            &self.bytes,
            offset,
        )?;

        let end = self
            .next_row_bytes
            .as_ref()
            .map(|next_row| -> Result<_, Error> {
                Ok(self
                    .sizes
                    .get_index(MetadataTableKind::Field, &next_row, offset)?
                    .value)
            })
            .unwrap_or_else(|| {
                Ok(self.sizes.num_rows(MetadataTableKind::Field))
            })?;

        Ok(UnpackedValue {
            value: start..end,
            loc,
        })
    }

    fn method_indices(&self) -> Result<UnpackedValue<Range<usize>>, Error> {
        let offset = 4
            + 2 * self.sizes.str_index_size()
            + self
                .sizes
                .coded_index_size(MetadataTableKind::TYPE_DEF_OR_REF)
            + self.sizes.index_size(MetadataTableKind::Field);

        let UnpackedValue { value: start, loc } = self.sizes.get_index(
            MetadataTableKind::MethodDef,
            &self.bytes,
            offset,
        )?;
        let end = self
            .next_row_bytes
            .as_ref()
            .map(|next_row| -> Result<_, Error> {
                Ok(self
                    .sizes
                    .get_index(MetadataTableKind::MethodDef, &next_row, offset)?
                    .value)
            })
            .unwrap_or_else(|| {
                Ok(self.sizes.num_rows(MetadataTableKind::MethodDef))
            })?;

        Ok(UnpackedValue {
            value: start..end,
            loc,
        })
    }
}

impl RowUnpacker for FieldRowUnpacker {
    const KIND: MetadataTableKind = MetadataTableKind::Field;
}

impl<'a> MetadataRowUnpacker<'a, FieldRowUnpacker> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
        type_name: &str,
        _type_namespace: &str,
    ) -> Result<(), Error> {
        annotator.value(self.flags()?).name("flags");

        let name = self.name()?.value;
        {
            let index = self.name_index()?;
            annotator
                .range(index.loc)
                .name("name")
                .value(format!("{}\n{}", index.value, name));
        }
        let signature_index = self.signature_index()?;
        annotator
            .value(signature_index.clone())
            .name("signature_index");

        annotator
            .range(self.get_blob(signature_index.value)?.as_range())
            .name(format!("'{type_name}.{name}' signature"));

        Ok(())
    }

    fn flags(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(0)
    }

    fn name_index(&self) -> Result<UnpackedValue<usize>, Error> {
        self.sizes.get_str_index(&self.bytes, 2)
    }

    fn name(&self) -> Result<UnpackedValue<&str>, Error> {
        let index = self.name_index()?.value;
        self.string_heap.get_null_terminated(index)
    }

    fn signature_index(&self) -> Result<UnpackedValue<usize>, Error> {
        let offset = 2 + self.sizes.str_index_size();
        self.sizes.get_blob_index(&self.bytes, offset)
    }
}

impl RowUnpacker for MethodDefRowUnpacker {
    const KIND: MetadataTableKind = MetadataTableKind::MethodDef;
}

impl<'a> MetadataRowUnpacker<'a, MethodDefRowUnpacker> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
        type_name: &str,
        type_namespace: &str,
        param_table: &MetadataTableUnpacker<ParamRowUnpacker>,
    ) -> Result<(), Error> {
        annotator.value(self.rva()?).name("rva");
        annotator.value(self.impl_flags()?).name("impl_flags");
        annotator.value(self.flags()?).name("flags");

        let name = self.name()?.value;
        {
            let index = self.name_index()?;
            annotator
                .range(index.loc)
                .name("name")
                .value(format!("{}\n{}", index.value, name));
        }

        let signature_index = self.signature_index()?;
        annotator
            .value(signature_index.clone())
            .name("signature_index");
        annotator
            .range(self.get_blob(signature_index.value)?.as_range())
            .name(format!("'{type_name}.{name}' signature"));

        let param_indices = self.param_indices()?;
        annotator
            .range(param_indices.loc.clone())
            .name("param_indices")
            .value(format!(
                "{}..{}",
                param_indices.value.start, param_indices.value.end
            ));

        annotator
            .range(param_indices.loc)
            .name("Param names")
            .value(
                param_indices
                    .value
                    .clone()
                    .map(|param_index| -> Result<_, Error> {
                        Ok(param_table.get_row(param_index)?.name()?.value)
                    })
                    .collect::<Result<String, Error>>()?,
            );

        param_indices.value.clone().try_for_each(
            |param_index| -> Result<_, Error> {
                let param = param_table.get_row(param_index)?;
                param.collect_annotations(
                    annotator,
                    type_name,
                    type_namespace,
                    name,
                )?;
                Ok(())
            },
        )?;

        annotator
            .group(param_table.address_range(param_indices.value.clone())?)
            .name(format!("Method '{name}'"));

        Ok(())
    }

    fn rva(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(0)
    }

    fn impl_flags(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(4)
    }

    fn flags(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(6)
    }

    fn name_index(&self) -> Result<UnpackedValue<usize>, Error> {
        self.sizes.get_str_index(&self.bytes, 8)
    }

    fn name(&self) -> Result<UnpackedValue<&str>, Error> {
        let index = self.name_index()?.value;
        self.string_heap.get_null_terminated(index)
    }

    fn signature_index(&self) -> Result<UnpackedValue<usize>, Error> {
        let offset = 8 + self.sizes.str_index_size();
        self.sizes.get_blob_index(&self.bytes, offset)
    }

    fn param_indices(&self) -> Result<UnpackedValue<Range<usize>>, Error> {
        let offset =
            8 + self.sizes.str_index_size() + self.sizes.blob_index_size();

        let UnpackedValue { value: start, loc } = self.sizes.get_index(
            MetadataTableKind::Param,
            &self.bytes,
            offset,
        )?;
        let end = self
            .next_row_bytes
            .as_ref()
            .map(|next_row| -> Result<_, Error> {
                Ok(self
                    .sizes
                    .get_index(MetadataTableKind::Param, &next_row, offset)?
                    .value)
            })
            .unwrap_or_else(|| {
                Ok(self.sizes.num_rows(MetadataTableKind::Param))
            })?;

        Ok(UnpackedValue {
            value: start..end,
            loc,
        })
    }
}

impl RowUnpacker for ParamRowUnpacker {
    const KIND: MetadataTableKind = MetadataTableKind::Param;
}

impl<'a> MetadataRowUnpacker<'a, ParamRowUnpacker> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
        _type_name: &str,
        _type_namespace: &str,
        _method_name: &str,
    ) -> Result<(), Error> {
        annotator.value(self.flags()?).name("flags");
        annotator.value(self.sequence()?).name("sequence");

        let name = self.name()?.value;
        {
            let index = self.name_index()?;
            annotator
                .range(index.loc)
                .name("name")
                .value(format!("{}\n{}", index.value, name));
        }

        Ok(())
    }

    fn flags(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(0)
    }

    fn sequence(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(2)
    }

    fn name_index(&self) -> Result<UnpackedValue<usize>, Error> {
        self.sizes.get_str_index(&self.bytes, 4)
    }

    fn name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        let index = self.name_index()?.value;
        self.string_heap.get_null_terminated(index)
    }
}

impl RowUnpacker for InterfaceImplRowUnpacker {
    const KIND: MetadataTableKind = MetadataTableKind::InterfaceImpl;
}

impl<'a> MetadataRowUnpacker<'a, InterfaceImplRowUnpacker> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.class_index()?).name("class_index");
        annotator.value(self.interface()?).name("interface");

        Ok(())
    }

    fn class_index(&self) -> Result<UnpackedValue<usize>, Error> {
        self.sizes
            .get_index(MetadataTableKind::TypeDef, &self.bytes, 0)
    }

    fn interface(&self) -> Result<UnpackedValue<MetadataIndex>, Error> {
        let offset = self.sizes.index_size(MetadataTableKind::TypeDef);
        self.sizes.get_coded_index(
            MetadataTableKind::TYPE_DEF_OR_REF,
            &self.bytes,
            offset,
        )
    }
}

impl RowUnpacker for MemberRefRowUnpacker {
    const KIND: MetadataTableKind = MetadataTableKind::MemberRef;
}

impl<'a> MetadataRowUnpacker<'a, MemberRefRowUnpacker> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.class_index()?).name("class_index");

        let name = self.name()?.value;
        {
            let index = self.name_index()?;
            annotator
                .range(index.loc)
                .name("name")
                .value(format!("{}\n{}", index.value, name));
        }

        let signature_index = self.signature_index()?;
        annotator
            .value(signature_index.clone())
            .name("signature_index");

        annotator
            .range(self.get_blob(signature_index.value.clone())?.as_range())
            .name(format!("MemberRef {name}"));

        Ok(())
    }

    fn class_index(&self) -> Result<UnpackedValue<MetadataIndex>, Error> {
        self.sizes.get_coded_index(
            MetadataTableKind::MEMBER_REF_PARENT,
            &self.bytes,
            0,
        )
    }

    fn name_index(&self) -> Result<UnpackedValue<usize>, Error> {
        let offset = self
            .sizes
            .coded_index_size(MetadataTableKind::MEMBER_REF_PARENT);
        self.sizes.get_str_index(&self.bytes, offset)
    }

    fn name(&self) -> Result<UnpackedValue<&str>, Error> {
        let index = self.name_index()?.value;
        self.string_heap.get_null_terminated(index)
    }

    fn signature_index(&self) -> Result<UnpackedValue<usize>, Error> {
        let offset = self
            .sizes
            .coded_index_size(MetadataTableKind::MEMBER_REF_PARENT)
            + self.sizes.str_index_size();
        self.sizes.get_blob_index(&self.bytes, offset)
    }
}

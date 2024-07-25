use std::ops::Range;

use memory_reader::{MemoryRegion, Pointer};

use crate::Error;

struct ByteRange<'a> {
    start: Pointer,
    bytes: &'a [u8],
}

pub struct UnpackedValue<T> {
    pub loc: Range<Pointer>,
    pub value: T,
}

pub struct Unpacker<'a> {
    bytes: ByteRange<'a>,
    pub offset_so_far: usize,
}

pub struct PEHeaderUnpacker<'a> {
    bytes: ByteRange<'a>,
}

/// Bitflags for the PE header
///
/// https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#characteristics
pub struct PEHeaderCharacteristics {
    /// The raw 16-bit value.
    raw: u16,

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

pub struct ModuleTableUnpacker<'a> {
    bytes: ByteRange<'a>,
    sizes: &'a MetadataSizes,
}

pub struct ModuleTableRowUnpacker<'a> {
    bytes: ByteRange<'a>,
    sizes: &'a MetadataSizes,
}

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

    fn subrange(&self, range: Range<impl NormalizeOffset>) -> ByteRange {
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
            offset_so_far: 3240004 + 384,
        }
    }

    pub fn address_range(&self, byte_range: Range<usize>) -> Range<Pointer> {
        let start = self.bytes.start;
        start + byte_range.start..start + byte_range.end
    }

    pub fn collect_annotations(
        &self,
        mut callback: impl FnMut(Range<Pointer>, &'static str, String),
    ) -> Result<(), Error> {
        callback(self.dos_header()?.loc, "DOS header", "".to_string());

        let lfanew = self.lfanew()?;
        callback(lfanew.loc, "lfanew", format!("{0}", lfanew.value));

        callback(self.dos_stub()?.loc, "DOS stub", "".to_string());

        let pe_header = self.pe_header()?;
        pe_header.collect_annotations(&mut callback)?;

        self.optional_header()?.collect_annotations(&mut callback)?;

        self.clr_runtime_header()?
            .collect_annotations(&mut callback)?;

        for i_section in 0..pe_header.num_sections()?.value as usize {
            self.section_header(i_section)?
                .collect_annotations(&mut callback)?;
        }

        self.metadata()?.collect_annotations(&mut callback)?;

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

    pub fn section_header(
        &self,
        i_section: usize,
    ) -> Result<SectionHeaderUnpacker, Error> {
        let pe_header = self.pe_header()?;
        let num_sections = pe_header.num_sections()?.value as usize;
        if i_section >= num_sections {
            return Err(Error::InvalidSectionNumber {
                num_sections,
                i_section,
            });
        }

        let optional_header = self.optional_header()?;

        let section_header_size = 40;

        let start =
            optional_header.bytes.end() + i_section * section_header_size;
        let bytes = self.bytes.subrange(start..start + section_header_size);
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
        callback: &mut impl FnMut(Range<Pointer>, &'static str, String),
    ) -> Result<(), Error> {
        macro_rules! annotate {
            ($field:ident) => {
                let field = self.$field()?;
                callback(
                    field.loc,
                    stringify!($field),
                    format!("{}", field.value),
                );
            };
        }

        callback(self.pe_signature()?.loc, "PE signature", "".to_string());
        annotate! {machine_type};
        annotate! {num_sections};
        annotate! {creation_timestamp};
        annotate! {symbol_table_pointer};
        annotate! {num_symbols};
        annotate! {optional_header_size};

        let characteristics = self.characteristics()?;
        callback(
            characteristics.loc.clone(),
            "Characteristics",
            format!("{}", characteristics.value.raw),
        );
        characteristics.value.collect_annotations(
            |name: &'static str, value: String| {
                callback(characteristics.loc.clone(), name, value)
            },
        )?;

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
            raw: value,
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

    fn collect_annotations(
        &self,
        mut callback: impl FnMut(&'static str, String),
    ) -> Result<(), Error> {
        macro_rules! annotate {
            ($field:ident) => {
                if self.$field {
                    callback("Flag", stringify!($field).to_string());
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
        callback: &mut impl FnMut(Range<Pointer>, &'static str, String),
    ) -> Result<(), Error> {
        macro_rules! annotate {
            ($field:ident) => {
                let field = self.$field()?;
                callback(
                    field.loc,
                    stringify!($field),
                    format!("{}", field.value),
                );
            };
        }

        annotate! {magic_value};
        annotate! {linker_major_version};
        annotate! {linker_minor_version};
        annotate! {code_size};
        annotate! {initialized_data_size};
        annotate! {uninitialized_data_size};
        annotate! {rva_entry_point};
        annotate! {rva_code_section};
        // annotate! {rva_data_section};
        annotate! {image_base};
        annotate! {section_alignment};
        annotate! {file_alignment};
        annotate! {os_major};
        annotate! {os_minor};
        annotate! {user_major};
        annotate! {user_minor};
        annotate! {subsys_major};
        annotate! {subsys_minor};
        annotate! {reserved_value};
        annotate! {image_size};
        annotate! {header_size};
        annotate! {file_checksum};
        annotate! {sub_system};
        annotate! {dll_flags};
        annotate! {stack_reserve_size};
        annotate! {stack_commit_size};
        annotate! {heap_reserve_size};
        annotate! {heap_commit_size};
        annotate! {loader_flags};

        let num_data_directories = self.num_data_directories()?;
        callback(
            num_data_directories.loc,
            "num_data_directories",
            format!("{}", num_data_directories.value),
        );

        for i_data_dir in 0..num_data_directories.value {
            let data_dir_kind: DataDirectoryKind = i_data_dir.try_into()?;
            let data_dir = self.data_directory(data_dir_kind)?;
            callback(
                data_dir.loc,
                "data dir",
                format!(
                    "{data_dir_kind:?}\nRVA {},\n{} bytes",
                    data_dir.value.rva, data_dir.value.size
                ),
            );
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
        callback: &mut impl FnMut(Range<Pointer>, &'static str, String),
    ) -> Result<(), Error> {
        macro_rules! annotate {
            ($field:ident) => {
                let field = self.$field()?;
                callback(
                    field.loc,
                    stringify!($field),
                    format!("{}", field.value),
                );
            };
        }

        annotate! {name};
        annotate! {virtual_size};
        annotate! {virtual_address};
        annotate! {raw_size};
        annotate! {raw_address};
        annotate! {ptr_relocations};
        annotate! {ptr_line_numbers};
        annotate! {num_relocations};
        annotate! {num_line_numbers};
        annotate! {characteristics};

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
        callback: &mut impl FnMut(Range<Pointer>, &'static str, String),
    ) -> Result<(), Error> {
        macro_rules! annotate {
            ($field:ident) => {
                let field = self.$field()?;
                callback(
                    field.loc,
                    stringify!($field),
                    format!("{}", field.value),
                );
            };
        }

        annotate! {header_size};
        annotate! {major_runtime_version};
        annotate! {minor_runtime_version};
        annotate! {metadata_range};
        annotate! {flags};
        annotate! {entry_point_token};
        annotate! {resources};
        annotate! {strong_name_signature};
        annotate! {code_manager_table};
        annotate! {vtable_fixups};
        annotate! {export_address_table_jumps};
        annotate! {managed_native_header};

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
        callback: &mut impl FnMut(Range<Pointer>, &'static str, String),
    ) -> Result<(), Error> {
        macro_rules! annotate {
            ($field:ident) => {
                let field = self.$field()?;
                callback(
                    field.loc,
                    stringify!($field),
                    format!("{}", field.value),
                );
            };
        }

        callback(
            self.metadata_signature()?.loc,
            "Metadata signature",
            "".to_string(),
        );
        annotate! {major_version};
        annotate! {minor_version};
        annotate! {reserved};
        annotate! {version_str_len};
        annotate! {version_str};
        annotate! {flags};
        annotate! {num_streams};

        self.iter_stream_header()?.try_for_each(
            |stream_header| -> Result<_, Error> {
                stream_header?.collect_annotations(callback)
            },
        )?;

        self.tilde_stream()?.collect_annotations(callback)?;

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
        let header = self
            .iter_stream_header()?
            .filter_map(|res| res.ok())
            .find(|stream_header| stream_header.name.value == "#~")
            .ok_or(Error::NoTildeStream)?;

        let offset = header.offset.value as usize;
        let size = header.size.value as usize;

        let bytes = self.bytes.subrange(offset..offset + size);

        Ok(TildeStreamUnpacker { bytes })
    }
}

impl<'a> StreamHeader<'a> {
    fn collect_annotations(
        &self,
        callback: &mut impl FnMut(Range<Pointer>, &'static str, String),
    ) -> Result<(), Error> {
        callback(
            self.offset.loc.clone(),
            "Stream Offset",
            format!("{}", self.offset.value),
        );
        callback(
            self.size.loc.clone(),
            "Stream Size",
            format!("{}", self.size.value),
        );
        callback(
            self.name.loc.clone(),
            "Stream Name",
            format!("{}", self.name.value),
        );

        Ok(())
    }
}

impl<'a> TildeStreamUnpacker<'a> {
    fn collect_annotations(
        &self,
        callback: &mut impl FnMut(Range<Pointer>, &'static str, String),
    ) -> Result<(), Error> {
        macro_rules! annotate {
            ($field:ident) => {
                let field = self.$field()?;
                callback(
                    field.loc,
                    stringify!($field),
                    format!("{}", field.value),
                );
            };
        }

        annotate! {reserved_0};
        annotate! {major_version};
        annotate! {minor_version};
        annotate! {heap_sizes};
        annotate! {reserved_1};
        annotate! {valid_table_bitfield};
        annotate! {sorted_table_bitfield};

        self.iter_num_rows()?
            .try_for_each(|res| -> Result<_, Error> {
                let (
                    UnpackedValue {
                        loc,
                        value: num_rows,
                    },
                    kind,
                ) = res?;
                callback(loc, "Num rows", format!("{num_rows} ({kind:?})"));
                Ok(())
            })?;

        let sizes = self.metadata_sizes()?;
        self.module_table(&sizes)?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(callback))?;

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

    pub fn metadata_table_start(
        &self,
        kind_start: MetadataTableKind,
        sizes: &MetadataSizes,
    ) -> Result<usize, Error> {
        self.iter_num_rows()?
            .map(|res| {
                res.map(|(rows, kind)| {
                    let rows = rows.value as usize;
                    let table_offset = if kind.as_metadata_sizes_index()
                        < kind_start.as_metadata_sizes_index()
                    {
                        rows * kind.bytes_per_row(sizes)
                    } else {
                        0
                    };
                    table_offset + 4
                })
            })
            .try_fold(24, |sum, res_bytes| Ok(sum + res_bytes?))
    }

    pub fn module_table<'b>(
        &'b self,
        sizes: &'b MetadataSizes,
    ) -> Result<ModuleTableUnpacker<'b>, Error> {
        let kind = MetadataTableKind::Module;
        let start = self.metadata_table_start(kind, sizes)?;
        let size = kind.table_bytes(sizes);
        let bytes = self.bytes.subrange(start..start + size);
        Ok(ModuleTableUnpacker { bytes, sizes })
    }
}

impl std::fmt::Display for HeapSizes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "HeapSizes{{")?;

        let mut need_comma = false;
        if self.string_stream_uses_u32_addr {
            write!(f, "#String")?;
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

impl MetadataTableKind {
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

    fn table_bytes(self, sizes: &MetadataSizes) -> usize {
        let num_rows = sizes[self] as usize;
        num_rows * self.bytes_per_row(sizes)
    }

    fn bytes_per_row(self, sizes: &MetadataSizes) -> usize {
        match self {
            MetadataTableKind::Module => {
                2 + sizes.str_index_size() + 3 * sizes.guid_index_size()
            }
            MetadataTableKind::TypeRef => todo!(),
            MetadataTableKind::TypeDef => todo!(),
            MetadataTableKind::Field => todo!(),
            MetadataTableKind::MethodDef => todo!(),
            MetadataTableKind::Param => todo!(),
            MetadataTableKind::InterfaceImpl => todo!(),
            MetadataTableKind::MemberRef => todo!(),
            MetadataTableKind::Constant => todo!(),
            MetadataTableKind::CustomAttribute => todo!(),
            MetadataTableKind::FieldMarshal => todo!(),
            MetadataTableKind::DeclSecurity => todo!(),
            MetadataTableKind::ClassLayout => todo!(),
            MetadataTableKind::FieldLayout => todo!(),
            MetadataTableKind::StandAloneSig => todo!(),
            MetadataTableKind::EventMap => todo!(),
            MetadataTableKind::Event => todo!(),
            MetadataTableKind::PropertyMap => todo!(),
            MetadataTableKind::Property => todo!(),
            MetadataTableKind::MethodSemantics => todo!(),
            MetadataTableKind::MethodImpl => todo!(),
            MetadataTableKind::ModuleRef => todo!(),
            MetadataTableKind::TypeSpec => todo!(),
            MetadataTableKind::ImplMap => todo!(),
            MetadataTableKind::FieldRVA => todo!(),
            MetadataTableKind::Assembly => todo!(),
            MetadataTableKind::AssemblyProcessor => todo!(),
            MetadataTableKind::AssemblyOS => todo!(),
            MetadataTableKind::AssemblyRef => todo!(),
            MetadataTableKind::AssemblyRefProcessor => todo!(),
            MetadataTableKind::AssemblyRefOS => todo!(),
            MetadataTableKind::File => todo!(),
            MetadataTableKind::ExportedType => todo!(),
            MetadataTableKind::ManifestResource => todo!(),
            MetadataTableKind::NestedClass => todo!(),
            MetadataTableKind::GenericParam => todo!(),
            MetadataTableKind::MethodSpec => todo!(),
            MetadataTableKind::GenericParamConstraint => todo!(),
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
    fn index_size(&self, table: MetadataTableKind) -> usize {
        self.coded_index_size([table])
    }

    fn coded_index_size<const N: usize>(
        &self,
        tables: [MetadataTableKind; N],
    ) -> usize {
        assert!(N > 0, "Must have at least one table");
        let table_bits = N.next_power_of_two().ilog2();
        let max_rows = tables.into_iter().map(|kind| self[kind]).max().unwrap();
        let row_bits = (max_rows + 1).next_power_of_two().ilog2();

        if table_bits + row_bits <= 16 {
            2
        } else {
            4
        }
    }

    fn get_heap_index(
        bytes: &ByteRange,
        offset: usize,
        is_u32_addr: bool,
    ) -> Result<UnpackedValue<u32>, Error> {
        if is_u32_addr {
            bytes.get_u32(offset)
        } else {
            Ok(bytes.get_u16(offset)?.map(|val| val as u32))
        }
    }

    fn get_str_index(
        &self,
        bytes: &ByteRange,
        offset: usize,
    ) -> Result<UnpackedValue<u32>, Error> {
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
    ) -> Result<UnpackedValue<u32>, Error> {
        Self::get_heap_index(
            bytes,
            offset,
            self.heap_sizes.guid_stream_uses_u32_addr,
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
}

impl<'a> ModuleTableUnpacker<'a> {
    pub fn iter_rows(
        &self,
    ) -> impl Iterator<Item = ModuleTableRowUnpacker> + '_ {
        let kind = MetadataTableKind::Module;
        let num_rows = self.sizes[kind] as usize;
        let bytes_per_row = kind.bytes_per_row(self.sizes) as usize;
        (0..num_rows).map(move |i_row| {
            let bytes = self
                .bytes
                .subrange(i_row * bytes_per_row..(i_row + 1) * bytes_per_row);
            ModuleTableRowUnpacker {
                bytes,
                sizes: self.sizes,
            }
        })
    }
}

impl<'a> ModuleTableRowUnpacker<'a> {
    fn collect_annotations(
        &self,
        callback: &mut impl FnMut(Range<Pointer>, &'static str, String),
    ) -> Result<(), Error> {
        macro_rules! annotate {
            ($field:ident) => {
                let field = self.$field()?;
                callback(
                    field.loc,
                    stringify!($field),
                    format!("{}", field.value),
                );
            };
        }

        annotate! {generation};
        annotate! {name_index};
        annotate! {module_id};
        annotate! {enc_id};
        annotate! {enc_base_id};

        Ok(())
    }

    pub fn generation(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(0)
    }

    pub fn name_index(&self) -> Result<UnpackedValue<u32>, Error> {
        self.sizes.get_str_index(&self.bytes, 2)
    }

    pub fn module_id(&self) -> Result<UnpackedValue<u32>, Error> {
        self.sizes
            .get_str_index(&self.bytes, 2 + self.sizes.str_index_size())
    }

    pub fn enc_id(&self) -> Result<UnpackedValue<u32>, Error> {
        self.sizes.get_guid_index(
            &self.bytes,
            2 + self.sizes.str_index_size() + self.sizes.guid_index_size(),
        )
    }

    pub fn enc_base_id(&self) -> Result<UnpackedValue<u32>, Error> {
        self.sizes.get_guid_index(
            &self.bytes,
            2 + self.sizes.str_index_size() + 2 * self.sizes.guid_index_size(),
        )
    }
}

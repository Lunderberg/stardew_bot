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

pub struct DataDirectory {
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
    CLRRuntimeHeader,
    Reserved15,
}

pub struct SectionHeaderUnpacker<'a> {
    bytes: ByteRange<'a>,
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
            offset_so_far: 0,
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
        for i_section in 0..pe_header.num_sections()?.value as usize {
            self.section_header(i_section)?
                .collect_annotations(&mut callback)?;
        }

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
    ) -> Result<UnpackedValue<DataDirectory>, Error> {
        let base = match self.magic_value {
            MagicValue::PE32 => 96,
            MagicValue::PE32plus => 112,
        };
        let dir_size = 8;
        let start = base + dir.index() * dir_size;

        let rva = self.bytes.get_u32(start)?.value;
        let size = self.bytes.get_u32(start + 4)?.value;
        let loc = self.bytes.address_range(start..start + dir_size);
        Ok(UnpackedValue {
            value: DataDirectory { rva, size },
            loc,
        })
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
            Self::CLRRuntimeHeader => 14,
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
            14 => Ok(Self::CLRRuntimeHeader),
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
        let value = std::str::from_utf8(&self.bytes[0..8])?;
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

    pub fn raw_size(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(16)
    }

    pub fn raw_address(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(20)
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

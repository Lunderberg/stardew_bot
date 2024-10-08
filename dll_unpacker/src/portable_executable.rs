use std::ops::Range;

use memory_reader::{ByteRange, Pointer, UnpackedValue};

use crate::relative_virtual_address::{
    RelativeVirtualAddress, VirtualAddressRelocation, VirtualRange,
};
use crate::Annotation as _;
use crate::DLLUnpacker;
use crate::{Annotator, Error};

/// Top-level methods for the DLLUnpacker, related to the unwrapping
/// of the PE format.
impl<'a> DLLUnpacker<'a> {
    pub(crate) fn pe_header(&self) -> Result<PEHeaderUnpacker, Error> {
        let dos_header = self.dos_header()?;
        let bytes = self.bytes.subrange(dos_header.pe_header_range()?);
        Ok(PEHeaderUnpacker::new(bytes))
    }

    pub(crate) fn optional_header(
        &self,
    ) -> Result<OptionalHeaderUnpacker, Error> {
        let pe_header = self.pe_header()?;
        let bytes = self.bytes.subrange(pe_header.optional_header_range()?);
        OptionalHeaderUnpacker::new(bytes)
    }

    pub(crate) fn iter_section_header(
        &self,
    ) -> Result<impl Iterator<Item = SectionHeaderUnpacker>, Error> {
        let pe_header = self.pe_header()?;
        let num_sections = pe_header.num_sections()?.value() as usize;

        let section_header_base = pe_header.optional_header_range()?.end;

        let section_header_size = 40;

        let file_start = self.bytes.start();

        let iter = (0..num_sections)
            .map(move |i_section| {
                section_header_base + i_section * section_header_size
            })
            .map(move |start| {
                let bytes =
                    self.bytes.subrange(start..start + section_header_size);
                SectionHeaderUnpacker::new(bytes, file_start)
            });

        Ok(iter)
    }

    pub(crate) fn virtual_address_relocations(
        &self,
    ) -> Result<Vec<VirtualAddressRelocation>, Error> {
        self.iter_section_header()?
            .map(|header| header.virtual_address_relocation())
            .collect()
    }

    pub(crate) fn virtual_address_to_raw(
        &self,
        addr: RelativeVirtualAddress,
    ) -> Result<Pointer, Error> {
        self.iter_section_header()?
            .find_map(|section| section.map_address(addr).ok()?)
            .ok_or(Error::InvalidVirtualAddress(addr))
    }
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
    file_start: Pointer,
}

impl<'a> PEHeaderUnpacker<'a> {
    pub(crate) fn new(bytes: ByteRange<'a>) -> Self {
        Self { bytes }
    }

    pub(crate) fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.group(self.bytes).name("PE Header");

        annotator
            .range(self.pe_signature()?.loc())
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
            Ok(UnpackedValue::new(self.bytes.address_range(byte_range), ()))
        } else {
            Err(Error::IncorrectPESignature)
        }
    }

    pub fn machine_type(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(4).map_err(Into::into)
    }

    pub fn num_sections(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(6).map_err(Into::into)
    }

    pub fn creation_timestamp(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(8).map_err(Into::into)
    }

    pub fn symbol_table_pointer(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(12).map_err(Into::into)
    }

    pub fn num_symbols(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(16).map_err(Into::into)
    }

    pub fn optional_header_size(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(20).map_err(Into::into)
    }

    pub fn characteristics(
        &self,
    ) -> Result<UnpackedValue<PEHeaderCharacteristics>, Error> {
        let unpacked = self.bytes.get_u16(22)?;
        Ok(unpacked.map(PEHeaderCharacteristics::new))
    }

    pub fn optional_header_range(&self) -> Result<Range<Pointer>, Error> {
        let optional_header_size =
            self.optional_header_size()?.value() as usize;
        let start = self.bytes.end();
        Ok(start..start + optional_header_size)
    }

    pub fn _iter_section_headers(
        &self,
    ) -> Result<impl Iterator<Item = Range<Pointer>>, Error> {
        let num_sections = self.num_sections()?.value() as usize;
        let section_header_base = self.optional_header_range()?.end;

        let iter = (0..num_sections)
            .map(move |i_section| {
                section_header_base + i_section * SectionHeaderUnpacker::SIZE
            })
            .map(|start| start..start + SectionHeaderUnpacker::SIZE);

        Ok(iter)
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
    pub(crate) fn new(bytes: ByteRange<'a>) -> Result<Self, Error> {
        let mut out = Self {
            bytes,
            magic_value: MagicValue::PE32,
        };
        out.magic_value = out.magic_value()?.value();
        Ok(out)
    }

    pub(crate) fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.group(self.bytes).name("Optional PE Header");

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
        if let Some(rva_data_section) = self.rva_data_section() {
            annotator.value(rva_data_section?).name("rva_data_section");
        }
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
            .value(num_data_directories)
            .name("Num data directories");

        for i_data_dir in 0..num_data_directories.value() {
            let data_dir_kind: DataDirectoryKind = i_data_dir.try_into()?;
            let data_dir = self.data_directory(data_dir_kind)?;
            annotator
                .opt_value(data_dir)
                .name(format!("{data_dir_kind:?}"));
        }

        Ok(())
    }

    pub fn magic_value(&self) -> Result<UnpackedValue<MagicValue>, Error> {
        let unpacked = self.bytes.get_u16(0)?;
        unpacked.try_map(MagicValue::new)
    }

    fn get_size(
        &self,
        pe32_offset: usize,
        pe32_plus_offset: usize,
    ) -> Result<UnpackedValue<u64>, Error> {
        match self.magic_value {
            MagicValue::PE32 => {
                Ok(self.bytes.get_u32(pe32_offset)?.map(|value| value as u64))
            }
            MagicValue::PE32plus => {
                self.bytes.get_u64(pe32_plus_offset).map_err(Into::into)
            }
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
        .map_err(Into::into)
    }

    pub fn linker_major_version(&self) -> Result<UnpackedValue<u8>, Error> {
        self.bytes.get_u8(2).map_err(Into::into)
    }

    pub fn linker_minor_version(&self) -> Result<UnpackedValue<u8>, Error> {
        self.bytes.get_u8(3).map_err(Into::into)
    }

    pub fn code_size(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(4).map_err(Into::into)
    }

    pub fn initialized_data_size(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(8).map_err(Into::into)
    }

    pub fn uninitialized_data_size(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(12).map_err(Into::into)
    }

    pub fn rva_entry_point(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(16).map_err(Into::into)
    }

    pub fn rva_code_section(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(20).map_err(Into::into)
    }

    pub fn rva_data_section(
        &self,
    ) -> Option<Result<UnpackedValue<u32>, Error>> {
        match self.magic_value {
            MagicValue::PE32 => {
                Some(self.bytes.get_u32(24).map_err(Into::into))
            }
            MagicValue::PE32plus => None,
        }
    }

    pub fn image_base(&self) -> Result<UnpackedValue<u64>, Error> {
        self.get_size(28, 24)
    }

    pub fn section_alignment(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(32).map_err(Into::into)
    }

    pub fn file_alignment(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(36).map_err(Into::into)
    }

    pub fn os_major(&self) -> Result<UnpackedValue<u16>, Error> {
        let unpacked = self.bytes.get_u16(40)?;
        let value = unpacked.value();
        if value == 4 || value == 5 {
            Ok(unpacked)
        } else {
            Err(Error::IncorrectOSMajor(value))
        }
    }

    pub fn os_minor(&self) -> Result<UnpackedValue<u16>, Error> {
        let unpacked = self.bytes.get_u16(42)?;
        let value = unpacked.value();
        if value == 0 {
            Ok(unpacked)
        } else {
            Err(Error::IncorrectOSMinor(value))
        }
    }

    pub fn user_major(&self) -> Result<UnpackedValue<u16>, Error> {
        let unpacked = self.bytes.get_u16(44)?;
        let value = unpacked.value();
        if value == 0 {
            Ok(unpacked)
        } else {
            Err(Error::IncorrectUserMajor(value))
        }
    }

    pub fn user_minor(&self) -> Result<UnpackedValue<u16>, Error> {
        let unpacked = self.bytes.get_u16(46)?;
        let value = unpacked.value();
        if value == 0 {
            Ok(unpacked)
        } else {
            Err(Error::IncorrectUserMinor(value))
        }
    }

    pub fn subsys_major(&self) -> Result<UnpackedValue<u16>, Error> {
        let unpacked = self.bytes.get_u16(48)?;
        let value = unpacked.value();
        if value == 4 || value == 5 {
            Ok(unpacked)
        } else {
            Err(Error::IncorrectSubsysMajor(value))
        }
    }

    pub fn subsys_minor(&self) -> Result<UnpackedValue<u16>, Error> {
        let unpacked = self.bytes.get_u16(50)?;
        let value = unpacked.value();
        if value == 0 {
            Ok(unpacked)
        } else {
            Err(Error::IncorrectSubsysMinor(value))
        }
    }

    pub fn reserved_value(&self) -> Result<UnpackedValue<u32>, Error> {
        let unpacked = self.bytes.get_u32(52)?;
        if unpacked.value() == 0 {
            Ok(unpacked)
        } else {
            Err(Error::InvalidReservedValue)
        }
    }

    pub fn image_size(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(56).map_err(Into::into)
    }

    pub fn header_size(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(60).map_err(Into::into)
    }

    pub fn file_checksum(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(64).map_err(Into::into)
    }

    pub fn sub_system(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(68).map_err(Into::into)
    }

    pub fn dll_flags(&self) -> Result<UnpackedValue<u16>, Error> {
        let unpacked = self.bytes.get_u16(70)?;
        if unpacked.value() & 0x100f == 0 {
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
    ) -> Result<UnpackedValue<Option<VirtualRange>>, Error> {
        let base = match self.magic_value {
            MagicValue::PE32 => 96,
            MagicValue::PE32plus => 112,
        };
        let dir_size = 8;
        let start = base + dir.index() * dir_size;

        self.bytes.subrange(start..start + dir_size).unpack()
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

impl std::fmt::Display for MagicValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MagicValue::PE32 => write!(f, "PE32"),
            MagicValue::PE32plus => write!(f, "PE32+"),
        }
    }
}

impl DataDirectoryKind {
    pub(crate) fn index(self) -> usize {
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

impl<'a> SectionHeaderUnpacker<'a> {
    #![allow(dead_code)]

    const SIZE: usize = 40;

    pub(crate) fn new(bytes: ByteRange<'a>, file_start: Pointer) -> Self {
        Self { bytes, file_start }
    }

    pub fn header_range(&self) -> Range<Pointer> {
        self.bytes.into()
    }

    pub fn section_range(&self) -> Result<Range<Pointer>, Error> {
        let file_offset = self.raw_address()?.value() as usize;
        let size = self.raw_size()?.value() as usize;

        let start = self.file_start + file_offset;
        Ok(start..start + size)
    }

    pub fn virtual_address_relocation(
        &self,
    ) -> Result<VirtualAddressRelocation, Error> {
        let virtual_start = self.virtual_address()?.value();
        let virtual_size = self.virtual_size()?.value();
        let section_start = self.raw_address()?.value() as usize;

        let range = VirtualRange::new(virtual_start, virtual_size);
        let location = self.file_start + section_start;
        let relocation = VirtualAddressRelocation::new(range, location);

        Ok(relocation)
    }

    pub(crate) fn map_address(
        &self,
        rva: RelativeVirtualAddress,
    ) -> Result<Option<Pointer>, Error> {
        let virtual_start = self.virtual_address()?.value();
        let virtual_size = self.virtual_size()?.value();
        let virtual_end = virtual_start + virtual_size;

        if virtual_start <= rva && rva < virtual_end {
            let section_offset = rva - virtual_start;
            let section_start = self.raw_address()?.value() as usize;

            Ok(Some(self.file_start + section_start + section_offset))
        } else {
            Ok(None)
        }
    }

    pub(crate) fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        let name = self.name()?.value();

        annotator
            .group(self.bytes)
            .name(format!("{name} PE section header"));

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

    pub fn name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        let value = std::str::from_utf8(self.bytes.subrange(0..8).bytes())?
            .trim_end_matches('\0');
        Ok(UnpackedValue::new(self.bytes.address_range(0..8), value))
    }

    pub fn virtual_size(&self) -> Result<UnpackedValue<usize>, Error> {
        Ok(self.bytes.get_u32(8)?.map(|val| val as usize))
    }

    pub fn virtual_address(
        &self,
    ) -> Result<UnpackedValue<RelativeVirtualAddress>, Error> {
        Ok(self
            .bytes
            .get_u32(12)?
            .map(|value| RelativeVirtualAddress::new(value as usize)))
    }

    pub fn virtual_range(
        &self,
    ) -> Result<Range<RelativeVirtualAddress>, Error> {
        let start = self.virtual_address()?.value();
        let size = self.virtual_size()?.value();
        let end = start + size;

        Ok(start..end)
    }

    pub fn raw_size(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(16).map_err(Into::into)
    }

    pub fn raw_address(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(20).map_err(Into::into)
    }

    pub fn raw_range(&self) -> Result<Range<u32>, Error> {
        let start = self.raw_address()?.value();
        let size = self.raw_size()?.value();
        Ok(start..start + size)
    }

    pub fn ptr_relocations(&self) -> Result<UnpackedValue<u32>, Error> {
        let unpacked = self.bytes.get_u32(24)?;
        if unpacked.value() == 0 {
            Ok(unpacked)
        } else {
            Err(Error::InvalidSectionHeader)
        }
    }

    pub fn ptr_line_numbers(&self) -> Result<UnpackedValue<u32>, Error> {
        let unpacked = self.bytes.get_u32(28)?;
        if unpacked.value() == 0 {
            Ok(unpacked)
        } else {
            Err(Error::InvalidSectionHeader)
        }
    }

    pub fn num_relocations(&self) -> Result<UnpackedValue<u16>, Error> {
        let unpacked = self.bytes.get_u16(32)?;
        if unpacked.value() == 0 {
            Ok(unpacked)
        } else {
            Err(Error::InvalidSectionHeader)
        }
    }

    pub fn num_line_numbers(&self) -> Result<UnpackedValue<u16>, Error> {
        let unpacked = self.bytes.get_u16(34)?;
        if unpacked.value() == 0 {
            Ok(unpacked)
        } else {
            Err(Error::InvalidSectionHeader)
        }
    }

    pub fn characteristics(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(36).map_err(Into::into)
    }

    pub fn contains_code(&self) -> Result<bool, Error> {
        Ok(self.characteristics()?.value() & 0x20 != 0)
    }

    pub fn contains_initialized_data(&self) -> Result<bool, Error> {
        Ok(self.characteristics()?.value() & 0x40 != 0)
    }

    pub fn contains_uninitialized_data(&self) -> Result<bool, Error> {
        Ok(self.characteristics()?.value() & 0x80 != 0)
    }

    pub fn is_executable(&self) -> Result<bool, Error> {
        Ok(self.characteristics()?.value() & 0x20000000 != 0)
    }

    pub fn is_readable(&self) -> Result<bool, Error> {
        Ok(self.characteristics()?.value() & 0x40000000 != 0)
    }

    pub fn is_writable(&self) -> Result<bool, Error> {
        Ok(self.characteristics()?.value() & 0x80000000 != 0)
    }
}

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

pub struct OptionalHeaderUnpacker<'a> {
    bytes: ByteRange<'a>,
}

pub struct SectionHeaderUnpacker<'a> {
    bytes: ByteRange<'a>,
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
        Ok(OptionalHeaderUnpacker { bytes })
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
        annotate! {characteristics};

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

    pub fn characteristics(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(22)
    }
}

impl<'a> OptionalHeaderUnpacker<'a> {
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

        callback(self.magic_value()?.loc, "Magic value", "".to_string());
        annotate! {linker_major_version};
        annotate! {linker_minor_version};
        annotate! {code_size};
        annotate! {initialized_data_size};
        annotate! {uninitialized_data_size};
        annotate! {rva_entry_point};
        annotate! {rva_code_section};
        annotate! {rva_data_section};
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
        annotate! {num_data_directories};

        Ok(())
    }

    pub fn magic_value(&self) -> Result<UnpackedValue<()>, Error> {
        let unpacked = self.bytes.get_u16(0)?;
        if unpacked.value == 0x010B || unpacked.value == 0x020B {
            Ok(UnpackedValue {
                value: (),
                loc: unpacked.loc,
            })
        } else {
            Err(Error::IncorrectMagicValue)
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

    pub fn rva_data_section(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(24)
    }

    pub fn image_base(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(28)
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

    pub fn stack_reserve_size(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(72)
    }

    pub fn stack_commit_size(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(76)
    }

    pub fn heap_reserve_size(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(80)
    }

    pub fn heap_commit_size(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(84)
    }

    pub fn loader_flags(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(88)
    }

    pub fn num_data_directories(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(92)
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

use std::ops::Range;

use thiserror::Error;

use crate::dll_unpacker::MetadataTableKind;
use crate::relative_virtual_address::RelativeVirtualAddress;

#[derive(Error)]
pub enum Error {
    #[error("IncorrectDOSHeader")]
    IncorrectDOSHeader,
    #[error("IncorrectDOSStub")]
    IncorrectDOSStub,
    #[error("IncorrectPESignature")]
    IncorrectPESignature,
    #[error(
        "InvalidMagicValue(0x{0:x}), should be 0x10b (PE32) or 0x20b (PE32+)"
    )]
    InvalidMagicValue(u16),
    #[error("IncorrectOSMajor({0}), should be 5")]
    IncorrectOSMajor(u16),
    #[error("IncorrectOSMinor({0}), should be 0")]
    IncorrectOSMinor(u16),
    #[error("IncorrectUserMajor({0}), should be 0")]
    IncorrectUserMajor(u16),
    #[error("IncorrectUserMinor({0}), should be 0")]
    IncorrectUserMinor(u16),
    #[error("IncorrectSubsysMajor({0}), should be 5")]
    IncorrectSubsysMajor(u16),
    #[error("IncorrectSubsysMinor({0}), should be 0")]
    IncorrectSubsysMinor(u16),
    #[error("InvalidReservedValue")]
    InvalidReservedValue,
    #[error("InvalidFileChecksum")]
    InvalidFileChecksum,
    #[error("InvalidDLLFlag")]
    InvalidDLLFlag,
    #[error("IncorrectStackReserveSize(0x{0:x}), should be 0x100000")]
    IncorrectStackReserveSize(u32),
    #[error("IncorrectStackCommitSize(0x{0:x}), should be 0x1000")]
    IncorrectStackCommitSize(u32),
    #[error("IncorrectHeapReserveSize(0x{0:x}), should be 0x100000")]
    IncorrectHeapReserveSize(u32),
    #[error("IncorrectHeapCommitSize(0x{0:x}), should be 0x1000")]
    IncorrectHeapCommitSize(u32),
    #[error("IncorrectLoaderFlags(0x{0:x}), should be 0")]
    IncorrectLoaderFlags(u32),
    #[error("IncorrectNumberOfDataDirectories(0x{0:x}), should be 0x10")]
    IncorrectNumberOfDataDirectories(u32),
    #[error("Invalid data directory {0}, should be in range [0,16)")]
    InvalidDataDirectoryIndex(u32),
    #[error("The CLR runtime header was not found")]
    MissingClrRuntimeHeader,
    #[error("InvalidSectionHeader")]
    InvalidSectionHeader,
    #[error("InvalidUTF8")]
    InvalidUTF8(#[from] std::str::Utf8Error),
    #[error("Cannot access section {i_section}, only {num_sections} sections present.")]
    InvalidSectionNumber {
        num_sections: usize,
        i_section: usize,
    },

    #[error("Virtual address {0} was not found in any section")]
    InvalidVirtualAddress(RelativeVirtualAddress),

    #[error("Virtual address was null, when non-null address expected")]
    UnexpectedNullVirtualAddress,

    #[error("IncorrectMetadataSignature")]
    IncorrectMetadataSignature,

    #[error("No stream header was found with name '{0}'")]
    MissingStream(&'static str),

    #[error("Table 0x{0:x} is marked as present, but no metadata table uses this value")]
    InvalidMetadataTable(u8),

    #[error("Coded index of {table_index} but only {num_tables} present")]
    InvalidCodedIndex {
        table_index: usize,
        num_tables: usize,
    },

    #[error("Coded index points to a reserved table entry.")]
    CodedIndexRefersToReservedTableIndex,

    #[error(
        "Blob header must start with 0, 1, or 2 leading ones, \
         but header had {leading_ones} leading ones."
    )]
    InvalidBlobHeader { leading_ones: u32 },

    #[error(
        "Index {index} is out of bounds for table {kind}, \
         which only has {num_rows} rows."
    )]
    InvalidMetadataTableIndex {
        kind: MetadataTableKind,
        index: usize,
        num_rows: usize,
    },

    #[error(
        "Index range {}..{} \
         is out of bounds for table {kind}, \
         which only has {num_rows} rows.",
        indices.start, indices.end
    )]
    InvalidMetadataTableIndexRange {
        kind: MetadataTableKind,
        indices: Range<usize>,
        num_rows: usize,
    },

    #[error(
        "GUID index zero is reserved for NULL indices, \
         but occurred in a context that requires a valid GUID."
    )]
    InvalidGuidIndexZero,

    #[error(
        "Metadata index zero is reserved for NULL indices, \
         but occurred in a context that \
         requires a valid table index."
    )]
    InvalidMetadataTableIndexZero { kind: MetadataTableKind },

    #[error(
        "First two bits of CIL Method header must be 0x2 or 0x3, \
         indicating a tiny header or fat header, respectively.  \
         However, instead found 0x{0:x}."
    )]
    InvalidCILMethodHeader(u8),
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

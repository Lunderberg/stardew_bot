use thiserror::Error;

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
    #[error("InvalidSectionHeader")]
    InvalidSectionHeader,
    #[error("InvalidUTF8")]
    InvalidUTF8(#[from] std::str::Utf8Error),
    #[error("Cannot access section {i_section}, only {num_sections} sections present.")]
    InvalidSectionNumber {
        num_sections: usize,
        i_section: usize,
    },
    #[error("Virtual address 0x{0:x} was not found in any section")]
    InvalidVirtualAddress(u32),

    #[error("IncorrectMetadataSignature")]
    IncorrectMetadataSignature,
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

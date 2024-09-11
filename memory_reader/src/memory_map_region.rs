use std::path::Path;
use std::{fmt::Display, ops::Range};

use crate::Symbol;

use super::{Error, MemoryRegion, Pointer, Result};

#[derive(Debug, Clone)]
pub struct MemoryMapRegion {
    pid: u32,
    start: Pointer,
    end: Pointer,
    file_offset: usize,
    pub name: Option<String>,
    pub is_executable: bool,
    pub is_readable: bool,
    pub is_writable: bool,
    pub is_shared_memory: bool,
}

impl MemoryMapRegion {
    pub fn new(map_range: proc_maps::MapRange, pid: u32) -> Result<Self> {
        let name = map_range
            .filename()
            .map(|p| {
                p.to_str()
                    .ok_or(Error::InvalidUTF8InPath)
                    .map(|s| s.to_string())
            })
            .transpose()?;
        Ok(Self {
            pid,
            start: map_range.start().into(),
            end: (map_range.start() + map_range.size()).into(),
            file_offset: map_range.offset,
            name,
            is_readable: map_range.is_read(),
            is_writable: map_range.is_write(),
            is_executable: map_range.is_exec(),
            is_shared_memory: &map_range.flags[3..4] == "s",
        })
    }

    pub(crate) fn empty() -> Self {
        Self {
            pid: 0,
            start: Pointer::null(),
            end: Pointer::null(),
            file_offset: 0,
            name: None,
            is_executable: false,
            is_readable: false,
            is_writable: false,
            is_shared_memory: false,
        }
    }

    pub fn name(&self) -> Option<&str> {
        self.name.as_ref().map(|s| s.as_str())
    }

    pub fn short_name(&self) -> &str {
        self.name
            .as_ref()
            .map(|name: &String| {
                Path::new(name)
                    .file_name()
                    .expect("mmap region ends in ..")
                    .to_str()
                    .unwrap_or("Non-UTF8 string after splitting path")
            })
            .unwrap_or("[anon]")
    }

    pub fn permissions_str(&self) -> &'static str {
        match (
            self.is_readable,
            self.is_writable,
            self.is_executable,
            self.is_shared_memory,
        ) {
            (true, true, true, true) => "rwxs",
            (true, true, true, false) => "rwxp",
            (true, true, false, true) => "rw-s",
            (true, true, false, false) => "rw-p",
            (true, false, true, true) => "r-xs",
            (true, false, true, false) => "r-xp",
            (true, false, false, true) => "r--s",
            (true, false, false, false) => "r--p",
            (false, true, true, true) => "-wxs",
            (false, true, true, false) => "-wxp",
            (false, true, false, true) => "-w-s",
            (false, true, false, false) => "-w-p",
            (false, false, true, true) => "--xs",
            (false, false, true, false) => "--xp",
            (false, false, false, true) => "---s",
            (false, false, false, false) => "---p",
        }
    }

    pub fn size_bytes(&self) -> usize {
        self.end - self.start
    }

    pub fn mmap_start_address(&self) -> Pointer {
        self.start - self.file_offset
    }

    pub fn address_range(&self) -> Range<Pointer> {
        self.start..self.end
    }

    pub fn file_offset(&self) -> usize {
        self.file_offset
    }

    pub fn contains(&self, ptr: Pointer) -> bool {
        (self.start <= ptr) && (ptr < self.end)
    }

    pub fn matches_name(&self, search_name: &str) -> bool {
        self.name
            .as_ref()
            .map(|name| name == search_name)
            .unwrap_or(false)
    }

    pub fn flag_str(&self) -> String {
        format!(
            "{}{}{}{}",
            if self.is_readable { 'r' } else { '-' },
            if self.is_writable { 'w' } else { '-' },
            if self.is_executable { 'x' } else { '-' },
            if self.is_shared_memory { 's' } else { 'p' },
        )
    }

    pub fn read(&self) -> Result<MemoryRegion> {
        let bytes = self
            .start
            .read_bytes(self.pid, self.size_bytes())
            .map_err(|err| match err {
                Error::MemoryReadBadAddress(..) => Error::MemoryReadBadRegion {
                    name: self.name.clone(),
                    start: self.start,
                    end: self.end,
                },
                _ => err,
            })?;
        Ok(MemoryRegion::new(self.start, bytes, self.clone()))
    }

    pub fn iter_symbols(&self) -> impl Iterator<Item = Symbol> {
        Symbol::iter_symbols(self)
    }
}

impl Display for MemoryMapRegion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = self.name.as_deref().unwrap_or("???");
        write!(
            f,
            "Region(PID {}, {} - {}, \"{}\")",
            self.pid, self.start, self.end, name
        )
    }
}

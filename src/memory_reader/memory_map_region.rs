use std::fmt::Display;

use super::{MemoryRegion, Pointer, Result};

#[derive(Debug)]
pub struct MemoryMapRegion {
    pid: u32,
    start: Pointer,
    end: Pointer,
    name: Option<String>,
    pub is_executable: bool,
    pub is_readable: bool,
    pub is_writable: bool,
}

impl MemoryMapRegion {
    pub fn new(map_range: proc_maps::MapRange, pid: u32) -> Self {
        Self {
            pid,
            start: map_range.start().into(),
            end: (map_range.start() + map_range.size()).into(),
            name: map_range
                .filename()
                .map(|p| p.to_str().unwrap().to_string()),
            is_executable: map_range.is_exec(),
            is_readable: map_range.is_read(),
            is_writable: map_range.is_write(),
        }
    }

    pub fn size_bytes(&self) -> u64 {
        self.end - self.start
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

    pub fn read(&self) -> Result<MemoryRegion> {
        let bytes = self
            .start
            .read_bytes(self.pid, self.size_bytes() as usize)?;
        Ok(MemoryRegion::new(self.start, bytes))
    }
}

impl Display for MemoryMapRegion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = self.name.as_ref().map(|n| n.as_str()).unwrap_or("???");
        write!(
            f,
            "Region(PID {}, {} - {}, \"{}\")",
            self.pid, self.start, self.end, name
        )
    }
}

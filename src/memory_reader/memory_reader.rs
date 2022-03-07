use std::collections::HashMap;
use std::fs::OpenOptions;
use std::io::{BufWriter, Write};
use std::path::PathBuf;

use super::{
    CollectBytes, Error, MemoryMapRegion, MemoryRegion, MemoryValue, Pointer,
    Result,
};

use itertools::Itertools;

pub struct MemoryReader {
    pub pid: u32,
    regions: Vec<MemoryMapRegion>,
}

impl MemoryReader {
    pub fn new(pid: u32) -> Result<Self> {
        let regions = Self::get_memory_regions(pid)?;
        Ok(Self { pid, regions })
    }

    fn get_memory_regions(pid: u32) -> Result<Vec<MemoryMapRegion>> {
        let process_maps = proc_maps::get_process_maps(pid.try_into().unwrap())
            .map_err(|_| Error::MemoryMapNotFound(pid))?
            .into_iter()
            .map(|map_range| MemoryMapRegion::new(map_range, pid))
            .collect();
        Ok(process_maps)
    }

    fn find_region(&self, name: &str) -> Option<&MemoryMapRegion> {
        self.regions.iter().find(|region| region.matches_name(name))
    }

    pub fn total_memory(&self) -> u64 {
        self.regions.iter().map(|region| region.size_bytes()).sum()
    }

    pub fn total_readable_memory(&self) -> u64 {
        self.regions
            .iter()
            .filter(|region| region.is_readable)
            .map(|region| region.size_bytes())
            .sum()
    }

    pub fn full_dump<P: Into<PathBuf>>(&self, filename: P) -> Result<()> {
        let write_file = OpenOptions::new()
            .write(true)
            .create(true)
            .open(filename.into())
            .expect("Could not open output file");
        let mut writer = BufWriter::new(write_file);

        self.regions
            .iter()
            .filter(|region| region.is_readable)
            .filter(|region| !region.matches_name("[vvar]"))
            .map(|region| region.read())
            .try_for_each(|data| -> Result<()> {
                writer
                    .write(&data?)
                    .expect("Could not write to output file");
                Ok(())
            })?;

        Ok(())
    }

    pub fn stack(&self) -> Result<&MemoryMapRegion> {
        self.find_region("[stack]").ok_or(Error::StackNotFound)
    }

    pub fn read_stack(&self) -> Result<MemoryRegion> {
        self.stack()?.read()
    }

    pub fn find_containing_region(
        &self,
        ptr: Pointer,
    ) -> Option<&MemoryMapRegion> {
        if ptr.is_null() {
            return None;
        }

        self.regions.iter().find(|region| region.contains(ptr))
    }

    pub fn pointers_in_stack(
        &self,
    ) -> Result<impl Iterator<Item = MemoryValue<Pointer>> + '_> {
        let stack = self.read_stack()?;

        Ok(stack
            .into_iter_bytes()
            .iter_byte_arr()
            .map(|arr_val: MemoryValue<[u8; 8]>| -> MemoryValue<Pointer> {
                arr_val.map(|arr| u64::from_ne_bytes(arr).into())
            })
            .filter(|ptr_ptr: &MemoryValue<Pointer>| -> bool {
                self.find_containing_region(ptr_ptr.value).is_some()
            }))
    }

    pub fn potential_frame_pointers(
        &self,
    ) -> Result<Vec<MemoryValue<Pointer>>> {
        let stack_to_stack: Vec<_> = self
            .pointers_in_stack()?
            .filter(|ptr_ptr| {
                self.find_containing_region(ptr_ptr.value)
                    .map(|region| region.matches_name("[stack]"))
                    .unwrap_or(false)
            })
            .collect();

        let counts: HashMap<Pointer, usize> =
            stack_to_stack.iter().counts_by(|ptr_ptr| ptr_ptr.value);

        let unique_stack_to_stack = stack_to_stack
            .into_iter()
            .filter(|ptr_ptr| counts.get(&ptr_ptr.value).unwrap() == &1)
            .collect();

        Ok(unique_stack_to_stack)
    }
}

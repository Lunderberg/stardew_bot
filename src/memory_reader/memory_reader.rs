use std::collections::HashMap;
use std::fs::OpenOptions;
use std::io::{BufWriter, Write};
use std::ops::Range;
use std::path::PathBuf;

use super::{
    CollectBytes, Error, MemoryMapRegion, MemoryRegion, MemoryValue, Pointer,
    Result,
};

use itertools::Itertools;

pub struct MemoryReader {
    pid: u32,
    regions: Vec<MemoryMapRegion>,
}

impl MemoryReader {
    pub fn new(pid: u32) -> Result<Self> {
        let regions = Self::get_memory_regions(pid)?;
        Ok(Self { pid, regions })
    }

    pub fn pid(&self) -> u32 {
        self.pid
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

    pub fn total_memory(&self) -> usize {
        self.regions.iter().map(|region| region.size_bytes()).sum()
    }

    pub fn total_readable_memory(&self) -> usize {
        self.regions
            .iter()
            .filter(|region| region.is_readable)
            .map(|region| region.size_bytes())
            .sum()
    }

    pub fn total_writable_memory(&self) -> usize {
        self.regions
            .iter()
            .filter(|region| region.is_writable)
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
            .filter(|map_region| map_region.is_readable)
            .filter(|map_region| !map_region.matches_name("[vvar]"))
            .map(|map_region| map_region.read())
            .try_for_each(|region| -> Result<()> {
                writer
                    .write_all(region?.data())
                    .expect("Could not write to output file");
                Ok(())
            })?;

        Ok(())
    }

    pub fn stack(&self) -> Result<&MemoryMapRegion> {
        self.find_region("[stack]").ok_or(Error::StackNotFound)
    }

    pub fn libc_address_ranges(&self) -> Vec<Range<Pointer>> {
        self.regions
            .iter()
            .filter(|region| {
                let name = region.short_name();
                name.starts_with("libc-") && name.ends_with(".so")
            })
            .map(|region| region.address_range())
            .collect()
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

    pub fn print_stack(&self) -> Result<()> {
        self.read_stack()?
            .into_iter_bytes()
            .iter_byte_arr()
            .for_each(|arr: MemoryValue<[u8; 8]>| {
                let as_pointer: Pointer = arr.value.into();
                if let Some(region) = self.find_containing_region(as_pointer) {
                    let name = region.name.as_deref().unwrap_or("???");
                    println!(
                        "Pointer at {} points to {}, located in {} ({})",
                        arr.location,
                        as_pointer,
                        name,
                        region.flag_str(),
                    );
                } else {
                    println!(
                        "Value   at {} is        {}",
                        arr.location, as_pointer
                    );
                }
                //
            });
        Ok(())
    }

    pub fn pointers_in_stack_with_region(
        &self,
    ) -> Result<
        impl Iterator<Item = (MemoryValue<Pointer>, &MemoryMapRegion)> + '_,
    > {
        let stack = self.read_stack()?;

        Ok(stack
            .into_iter_bytes()
            .iter_byte_arr()
            .map(|arr_val| arr_val.map(|arr| usize::from_ne_bytes(arr).into()))
            .filter_map(|ptr_ptr| {
                self.find_containing_region(ptr_ptr.value)
                    .map(move |region| (ptr_ptr, region))
            }))
    }

    pub fn pointers_in_stack(
        &self,
    ) -> Result<impl Iterator<Item = MemoryValue<Pointer>> + '_> {
        let stack = self.read_stack()?;

        Ok(stack
            .into_iter_bytes()
            .iter_byte_arr()
            .map(|arr_val: MemoryValue<[u8; 8]>| -> MemoryValue<Pointer> {
                arr_val.map(|arr| usize::from_ne_bytes(arr).into())
            })
            .filter(|ptr_ptr: &MemoryValue<Pointer>| -> bool {
                self.find_containing_region(ptr_ptr.value).is_some()
            }))
    }

    pub fn potential_frame_pointers(
        &self,
    ) -> Result<Vec<MemoryValue<Pointer>>> {
        let stack_to_stack: Vec<_> = self
            .pointers_in_stack_with_region()?
            .filter_map(|(ptr_ptr, region)| {
                region.matches_name("[stack]").then_some(ptr_ptr)
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

    pub fn potential_return_instruction_pointers(
        &self,
    ) -> Result<
        impl Iterator<Item = (MemoryValue<Pointer>, &MemoryMapRegion)> + '_,
    > {
        Ok(self
            .pointers_in_stack_with_region()?
            .filter(|(_ptr_ptr, region)| region.is_executable)
            .filter(|(_ptr_ptr, region)| !region.matches_name("[stack]")))
    }
}

use proc_maps::MapRange;
use process_vm_io::ProcessVirtualMemoryIO;

use std::fs::OpenOptions;
use std::io::{BufWriter, Read, Write};
use std::path::PathBuf;

use itertools::Itertools;

use crate::error::{Error, Result};

pub struct MemoryReader {
    pid: u32,
    maps: Vec<MapRange>,
}

impl MemoryReader {
    pub fn new(pid: u32) -> Result<Self> {
        let maps = Self::get_memory_maps(pid)?;
        Ok(Self { pid, maps })
    }

    fn get_memory_maps(pid: u32) -> Result<Vec<MapRange>> {
        let process_maps = proc_maps::get_process_maps(pid.try_into().unwrap())
            .map_err(|_| Error::MemoryMapNotFound(pid))?;
        Ok(process_maps)
    }

    fn find_map(&self, filename: &str) -> Result<MapRange> {
        self.maps
            .iter()
            .find(|map| {
                map.filename().map(|p| p.to_str()).flatten() == Some(filename)
            })
            .map(|map| map.clone())
            .ok_or_else(|| Error::MissingMemoryMapSection(filename.to_string()))
    }

    pub fn total_memory(&self) -> usize {
        self.maps
            .iter()
            .filter(|map| map.is_read())
            .map(|map| map.size())
            .sum()
    }

    pub fn total_readable_memory(&self) -> usize {
        self.maps.iter().map(|map| map.size()).sum()
    }

    pub fn full_dump<P: Into<PathBuf>>(&self, filename: P) -> Result<()> {
        let write_file = OpenOptions::new()
            .write(true)
            .create(true)
            .open(filename.into())
            .expect("Could not open output file");
        let mut writer = BufWriter::new(write_file);

        self.maps
            .iter()
            .filter(|map_range| map_range.is_read())
            .filter(|map_range| map_range.filename().map(|p| p.to_str()).flatten() != Some("[vvar]"))
            .filter_map(|map_range| {
                let res_data = self.read_map_range(&map_range);
                match res_data {
                    Ok(data) => Some(data),
                    Err(err) => {
                        println!(
                            "Couldn't read from mmap of {name:?}, range 0x{start:02x} - 0x{end:02x}, error '{err:?}', is_read={is_read}",
                            name=map_range.filename(),
                            start=map_range.start(),
                            end=map_range.start() + map_range.size(),
                            err=err,
                            is_read=map_range.is_read(),
                        );
                        None
                    }
                }
            })
            .for_each(|data| {
                writer.write(&data).expect("Could not write to output file");
            });

        Ok(())
    }

    // fn read_address(&self, address: u64, size: usize) -> Result<Vec<u8>> {

    // }

    fn read_map_range(&self, map_range: &MapRange) -> Result<Vec<u8>> {
        let mut process_io = unsafe {
            ProcessVirtualMemoryIO::new(self.pid, map_range.start() as u64)
        }
        .unwrap();

        let mut buffer = vec![0u8; map_range.size()];

        process_io.read_exact(&mut buffer).map_err(|err| {
            let err_string = format!("{}", err);
            if err_string.contains("Operation not permitted") {
                Error::MemoryReadPermissionError
            } else if err_string.contains("Bad address") {
                Error::MemoryReadBadAddress
            } else {
                Error::MemoryReadOtherError { err }
            }
        })?;

        Ok(buffer)
    }

    pub fn stack_map(&self) -> Result<MapRange> {
        self.find_map("[stack]")
    }

    pub fn heap_map(&self) -> Result<MapRange> {
        self.find_map("[heap]")
    }

    pub fn read_stack(&self) -> Result<Vec<u8>> {
        let stack_map = self.stack_map()?;
        self.read_map_range(&stack_map)
    }

    pub fn read_heap(&self) -> Result<Vec<u8>> {
        let heap_map = self.heap_map()?;
        self.read_map_range(&heap_map)
    }

    fn find_containing_map(&self, ptr: u64) -> Option<&MapRange> {
        if ptr == 0 {
            return None;
        }

        self.maps.iter().find_map(|map| {
            let start: u64 = map.start() as u64;
            let end: u64 = (map.start() + map.size()) as u64;
            (ptr >= start && ptr < end).then(|| map)
        })
    }

    pub fn pointers_in_stack(&self) -> Result<Vec<(MapRange, u64)>> {
        let stack = self.read_stack()?;

        println!("stack size: {}", stack.len());

        Ok(stack
            .iter()
            .chunks(8)
            .into_iter()
            .map(|chunk| {
                let mut bytes: [u8; 8] = [0; 8];
                chunk.into_iter().zip(bytes.iter_mut()).for_each(
                    |(val, out)| {
                        *out = *val;
                    },
                );
                u64::from_ne_bytes(bytes)
            })
            .filter_map(|ptr| {
                self.find_containing_map(ptr).map(|map| (map.clone(), ptr))
            })
            .collect())
    }
}

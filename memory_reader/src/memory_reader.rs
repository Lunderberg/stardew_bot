use std::collections::HashMap;
use std::fs::OpenOptions;
use std::io::{BufWriter, IoSliceMut, Write};
use std::ops::Range;
use std::path::PathBuf;

use crate::Symbol;
use crate::{extensions::*, OwnedBytes};

use super::{
    Error, MemoryMapRegion, MemoryRegion, MemoryValue, Pointer, Result,
};

use itertools::Itertools;
use nix::sys::uio::{process_vm_readv, RemoteIoVec};
use nix::unistd::SysconfVar;

use env_var_flag::env_var_flag;

pub struct MemoryReader {
    pid: u32,
    regions: Vec<MemoryMapRegion>,
    region_ranges: Vec<Range<Pointer>>,
    page_size: usize,
    max_num_iovec: usize,
}

impl MemoryReader {
    pub fn new(pid: u32) -> Result<Self> {
        let regions = Self::get_memory_regions(pid)?;

        let region_ranges = regions
            .iter()
            .map(|region| region.address_range())
            .sorted_by_key(|range| range.start)
            .collect();

        let page_size = nix::unistd::sysconf(SysconfVar::PAGE_SIZE)?
            .map(|c_long| c_long as usize)
            .filter(|_| env_var_flag("READ_ON_PAGE_BOUNDARIES"))
            .unwrap_or(usize::MAX);

        let max_num_iovec = nix::unistd::sysconf(SysconfVar::IOV_MAX) //?
            .unwrap()
            .map(|c_long| c_long as usize)
            .unwrap_or(1);

        Ok(Self {
            pid,
            regions,
            region_ranges,
            page_size,
            max_num_iovec,
        })
    }

    pub fn pid(&self) -> u32 {
        self.pid
    }

    pub fn iter_regions(&self) -> impl Iterator<Item = &MemoryMapRegion> + '_ {
        self.regions.iter()
    }

    fn get_memory_regions(pid: u32) -> Result<Vec<MemoryMapRegion>> {
        proc_maps::get_process_maps(
            pid.try_into().unwrap_or_else(|_| {
                panic!("Could not convert PID {pid} to pid_t")
            }),
        )
        .map_err(|_| Error::MemoryMapNotFound(pid))?
        .into_iter()
        .map(|map_range| MemoryMapRegion::new(map_range, pid))
        .collect()
    }

    pub fn read_exact(
        &self,
        ptr: impl Into<Pointer>,
        buffer: &mut [u8],
    ) -> Result<()> {
        let ptr: Pointer = ptr.into();

        if ptr.is_null() {
            return Err(Error::MemoryReadNullPointer);
        }

        // The length is often larger than a single page.  The manpage
        // for `process_vm_readv` recommends breaking up larger reads
        // into page-sized reads to maximize the size of a partial
        // read, since permissions are checked prior to reading each
        // page.  However, this quickly exceeds the maximum number of
        // iovec instances allowed in a single call, and the bot
        // typically reads within a single memmap region anyways.
        let remotes = vec![RemoteIoVec {
            base: ptr.address,
            len: buffer.len(),
        }];

        let remotes = if self.page_size != usize::MAX {
            remotes
                .into_iter()
                .flat_map(|remote| self.split_on_page_boundaries(remote))
                .collect()
        } else {
            remotes
        };

        let mut locals = vec![IoSliceMut::new(buffer)];

        assert!(
            remotes.len() <= self.max_num_iovec,
            "Read of {} bytes would use {} RemoteIoVec instances, \
             but the system can only use {} instances.",
            buffer.len(),
            remotes.len(),
            self.max_num_iovec,
        );

        process_vm_readv(
            nix::unistd::Pid::from_raw(self.pid as i32),
            &mut locals,
            &remotes,
        )
        .map_err(|err| match err {
            nix::errno::Errno::EPERM => Error::MemoryReadInsufficientPermission,
            _ => err.into(),
        })?;

        Ok(())
    }

    pub fn read_regions(
        &self,
        regions: &mut [(Pointer, &mut [u8])],
    ) -> Result<()> {
        let remotes: Vec<RemoteIoVec> = regions
            .iter()
            .map(|(ptr, output)| RemoteIoVec {
                base: ptr.address,
                len: output.len(),
            })
            .flat_map(|remote_io_vec| {
                self.split_on_page_boundaries(remote_io_vec)
            })
            .collect();

        assert!(
            remotes.len() <= self.max_num_iovec,
            "Read of {} bytes from {} regions \
             would use {} RemoteIoVec instances, \
             but the system can only use {} instances.",
            regions
                .iter()
                .map(|(_, output)| output.len())
                .sum::<usize>(),
            regions.len(),
            remotes.len(),
            self.max_num_iovec,
        );

        let mut locals = Vec::<IoSliceMut>::new();
        {
            let mut remaining = &mut regions[..];
            while !remaining.is_empty() {
                let (left, right) = remaining.split_at_mut(1);
                locals.push(IoSliceMut::new(left[0].1));
                remaining = right;
            }
        }

        process_vm_readv(
            nix::unistd::Pid::from_raw(self.pid as i32),
            &mut locals,
            &remotes,
        )
        .map_err(|err| match err {
            nix::errno::Errno::EPERM => Error::MemoryReadInsufficientPermission,
            _ => err.into(),
        })?;

        Ok(())
    }

    fn split_on_page_boundaries(
        &self,
        remote: RemoteIoVec,
    ) -> impl Iterator<Item = RemoteIoVec> {
        let RemoteIoVec { mut base, mut len } = remote;
        let page_size = self.page_size;

        std::iter::from_fn(move || {
            if len == 0 {
                None
            } else {
                let next_page = (base + 1).next_multiple_of(page_size);
                let this_len = (next_page - base).min(len);
                let this_page = RemoteIoVec {
                    base,
                    len: this_len,
                };
                base += this_len;
                len -= this_len;
                Some(this_page)
            }
        })
    }

    pub fn read_byte_array<const N: usize>(
        &self,
        pointer: impl Into<Pointer>,
    ) -> Result<[u8; N]> {
        let pointer: Pointer = pointer.into();
        let mut buffer = [0u8; N];

        self.read_exact(pointer, &mut buffer)?;
        Ok(buffer)
    }

    pub fn read_bytes(&self, range: Range<Pointer>) -> Result<OwnedBytes> {
        let mut buffer = vec![0u8; range.end - range.start];
        self.read_exact(range.start, &mut buffer)?;
        Ok(OwnedBytes::new(range.start, buffer))
    }

    pub fn find_region(
        &self,
        mut filter: impl FnMut(&MemoryMapRegion) -> bool,
    ) -> Option<&MemoryMapRegion> {
        self.iter_regions().find(|reg| filter(reg))
    }

    pub fn is_in_executable_region(&self, address: Pointer) -> bool {
        self.iter_regions()
            .filter(|region| region.is_executable)
            .map(|region| region.address_range())
            .any(|range| range.contains(&address))
    }

    pub fn iter_symbols(&self) -> impl Iterator<Item = Symbol> + '_ {
        self.iter_regions().flat_map(|region| region.iter_symbols())
    }

    pub fn total_memory(&self) -> usize {
        self.iter_regions().map(|region| region.size_bytes()).sum()
    }

    pub fn total_readable_memory(&self) -> usize {
        self.iter_regions()
            .filter(|region| region.is_readable)
            .map(|region| region.size_bytes())
            .sum()
    }

    pub fn total_writable_memory(&self) -> usize {
        self.iter_regions()
            .filter(|region| region.is_writable)
            .map(|region| region.size_bytes())
            .sum()
    }

    pub fn full_dump<P: Into<PathBuf>>(&self, filename: P) -> Result<()> {
        let write_file = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(filename.into())
            .expect("Could not open output file");
        let mut writer = BufWriter::new(write_file);

        self.iter_regions()
            .filter(|map_region| map_region.is_readable)
            .filter(|map_region| !map_region.matches_name("[vvar]"))
            .map(|map_region| map_region.read(self))
            .try_for_each(|region| -> Result<()> {
                writer
                    .write_all(region?.data())
                    .expect("Could not write to output file");
                Ok(())
            })?;

        Ok(())
    }

    pub fn stack(&self) -> Result<&MemoryMapRegion> {
        self.find_region(|reg| reg.matches_name("[stack]"))
            .ok_or(Error::StackNotFound)
    }

    pub fn read_stack(&self) -> Result<MemoryRegion> {
        self.stack()?.read(self)
    }

    pub fn is_valid_ptr(&self, ptr: Pointer) -> bool {
        self.iter_regions().any(|region| region.contains(ptr))
    }

    pub fn find_containing_region(
        &self,
        ptr: Pointer,
    ) -> Option<&MemoryMapRegion> {
        if ptr.is_null() {
            return None;
        }

        self.iter_regions().find(|region| region.contains(ptr))
    }

    pub fn find_containing_region_range(
        &self,
        ptr: Pointer,
    ) -> Option<Range<Pointer>> {
        if ptr.is_null() {
            return None;
        }

        match self
            .region_ranges
            .binary_search_by_key(&ptr, |range| range.start)
        {
            Ok(i) => Some(i),
            Err(0) => None,
            Err(i) => Some(i - 1),
        }
        .map(|i| self.region_ranges[i].clone())
        .filter(|range| range.contains(&ptr))
    }

    pub fn print_stack(&self) -> Result<()> {
        self.read_stack()?
            .into_iter_bytes()
            .iter_as::<MemoryValue<Pointer>>()
            .for_each(|value| {
                let as_pointer: Pointer = value.value;

                if let Some(region) = self.find_containing_region(as_pointer) {
                    let name = region.name.as_deref().unwrap_or("???");
                    println!(
                        "Pointer at {} points to {}, located in {} ({})",
                        value.location,
                        as_pointer,
                        name,
                        region.flag_str(),
                    );
                } else {
                    println!(
                        "Value   at {} is        {}",
                        value.location, as_pointer
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
            .iter_as::<MemoryValue<Pointer>>()
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
            .iter_as::<MemoryValue<Pointer>>()
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
            .filter(|ptr_ptr| matches!(counts.get(&ptr_ptr.value), Some(1)))
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

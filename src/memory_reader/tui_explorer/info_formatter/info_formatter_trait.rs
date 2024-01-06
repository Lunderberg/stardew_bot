use crate::{
    memory_reader::{MemoryRegion, Pointer},
    MemoryReader,
};

use std::fmt::Display;

pub trait InfoFormatter {
    fn format(
        &self,
        reader: &MemoryReader,
        region: &MemoryRegion,
        location: Pointer,
    ) -> Option<impl Display>;
}

use std::{fmt::Display, ops::Range};

use memory_reader::Pointer;

use crate::dll_unpacker::UnpackedValue;

pub trait Annotator {
    fn range(&mut self, range: Range<Pointer>) -> &mut impl Annotation;

    fn value<T: Display>(
        &mut self,
        value: UnpackedValue<T>,
    ) -> &mut impl Annotation {
        self.range(value.loc).value(value.value)
    }
}

pub trait Annotation: Sized {
    fn name(&mut self, name: impl Into<String>) -> &mut Self;

    fn value(&mut self, value: impl Display) -> &mut Self;

    fn disable_highlight(&mut self) -> &mut Self;
}

use std::ops::{Range, RangeFrom, RangeFull, RangeTo};

use crate::Pointer;

pub trait NormalizeOffset: Copy {
    fn as_offset(self, start: Pointer) -> usize;
    fn as_ptr(self, start: Pointer) -> Pointer;
}
impl NormalizeOffset for usize {
    fn as_offset(self, _start: Pointer) -> usize {
        self
    }

    fn as_ptr(self, start: Pointer) -> Pointer {
        start + self
    }
}
impl NormalizeOffset for Pointer {
    fn as_offset(self, start: Pointer) -> usize {
        self - start
    }

    fn as_ptr(self, _start: Pointer) -> Pointer {
        self
    }
}

pub trait NormalizeRange {
    fn normalize_to_offset(self, buf_range: Range<Pointer>) -> Range<usize>;
    fn normalize_to_ptr(self, buf_range: Range<Pointer>) -> Range<Pointer>;
}
impl<T> NormalizeRange for Range<T>
where
    T: NormalizeOffset,
{
    fn normalize_to_offset(self, buf_range: Range<Pointer>) -> Range<usize> {
        let start = self.start.as_offset(buf_range.start);
        let end = self.end.as_offset(buf_range.start);
        start..end
    }

    fn normalize_to_ptr(self, buf_range: Range<Pointer>) -> Range<Pointer> {
        let start = self.start.as_ptr(buf_range.start);
        let end = self.end.as_ptr(buf_range.start);
        start..end
    }
}

impl<T> NormalizeRange for RangeFrom<T>
where
    T: NormalizeOffset,
{
    fn normalize_to_offset(self, buf_range: Range<Pointer>) -> Range<usize> {
        let start = self.start.as_offset(buf_range.start);
        let end = buf_range.end - buf_range.start;
        start..end
    }

    fn normalize_to_ptr(self, buf_range: Range<Pointer>) -> Range<Pointer> {
        let start = self.start.as_ptr(buf_range.start);
        start..buf_range.end
    }
}

impl<T> NormalizeRange for RangeTo<T>
where
    T: NormalizeOffset,
{
    fn normalize_to_offset(self, buf_range: Range<Pointer>) -> Range<usize> {
        let end = self.end.as_offset(buf_range.start);
        0..end
    }

    fn normalize_to_ptr(self, buf_range: Range<Pointer>) -> Range<Pointer> {
        let end = self.end.as_ptr(buf_range.start);
        buf_range.start..end
    }
}

impl NormalizeRange for RangeFull {
    fn normalize_to_offset(self, buf_range: Range<Pointer>) -> Range<usize> {
        let size = buf_range.end - buf_range.start;
        0..size
    }

    fn normalize_to_ptr(self, buf_range: Range<Pointer>) -> Range<Pointer> {
        buf_range
    }
}

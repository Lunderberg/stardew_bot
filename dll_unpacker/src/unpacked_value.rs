use std::{borrow::Borrow, ops::Range};

use memory_reader::Pointer;

#[derive(Clone, Copy)]
pub struct UnpackedValue<T> {
    /// Inclusive start of the value's location.  Not stored as a
    /// `Range<Pointer>`, because that would prevent `UnpackedValue`
    /// from implemented Copy.
    pub(crate) start: Pointer,

    /// Inclusive end of the value's location.  Not stored as a
    /// `Range<Pointer>`, because that would prevent `UnpackedValue`
    /// from implemented Copy.
    pub(crate) end: Pointer,

    /// The unpacked value.
    pub(crate) value: T,
}

impl<T> UnpackedValue<T> {
    pub fn new(loc: Range<Pointer>, value: T) -> Self {
        Self {
            start: loc.start,
            end: loc.end,
            value,
        }
    }

    pub fn map<U>(self, func: impl FnOnce(T) -> U) -> UnpackedValue<U> {
        UnpackedValue::new(self.loc(), func(self.value))
    }

    pub fn try_map<U, E>(
        self,
        func: impl FnOnce(T) -> Result<U, E>,
    ) -> Result<UnpackedValue<U>, E> {
        Ok(UnpackedValue::new(self.loc(), func(self.value)?))
    }

    pub fn loc(&self) -> Range<Pointer> {
        self.start..self.end
    }

    pub fn value(self) -> T {
        self.value
    }
}
impl<T> Into<(Range<Pointer>, T)> for UnpackedValue<T> {
    fn into(self) -> (Range<Pointer>, T) {
        (self.loc(), self.value())
    }
}

impl<T> Borrow<T> for UnpackedValue<T> {
    fn borrow(&self) -> &T {
        &self.value
    }
}

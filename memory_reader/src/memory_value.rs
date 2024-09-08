use std::mem::MaybeUninit;

use crate::extensions::IterConversion;

use super::Pointer;

#[derive(Debug, Clone)]
pub struct MemoryValue<T> {
    pub location: Pointer,
    pub value: T,
}

impl<T> MemoryValue<T> {
    #[inline]
    pub fn new(location: Pointer, value: T) -> Self {
        Self { location, value }
    }

    pub fn map<U, F>(self, f: F) -> MemoryValue<U>
    where
        F: FnOnce(T) -> U,
    {
        MemoryValue::new(self.location, f(self.value))
    }
}

pub trait CollectBytes: Iterator<Item = MemoryValue<u8>> {
    fn iter_byte_arr<const N: usize>(self) -> ByteCollector<Self, N>
    where
        Self: Sized,
    {
        ByteCollector::new(self)
    }
}

impl<T> CollectBytes for T where T: Iterator<Item = MemoryValue<u8>> {}

pub struct ByteCollector<I, const N: usize> {
    iter: I,
}

impl<I, const N: usize> ByteCollector<I, N> {
    fn new(iter: I) -> Self {
        Self { iter }
    }
}

impl<I, const N: usize> Iterator for ByteCollector<I, N>
where
    I: Iterator<Item = MemoryValue<u8>>,
{
    type Item = MemoryValue<[u8; N]>;
    fn next(&mut self) -> Option<Self::Item> {
        let mut location = Pointer::null();
        let mut value = [0; N];

        for (i, element) in value.iter_mut().enumerate() {
            let val = self.iter.next()?;
            if i == 0 {
                location = val.location;
            }
            *element = val.value;
        }
        Some(Self::Item { location, value })
    }
}

impl<I, const N: usize> DoubleEndedIterator for ByteCollector<I, N>
where
    I: DoubleEndedIterator<Item = MemoryValue<u8>>,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        let mut location = Pointer::null();
        let mut value = [0; N];

        for (i, element) in value.iter_mut().enumerate().rev() {
            let val = self.iter.next_back()?;
            if i == 0 {
                location = val.location;
            }
            *element = val.value;
        }
        Some(Self::Item { location, value })
    }
}

impl<const N: usize, T> IterConversion<MemoryValue<T>> for MemoryValue<[T; N]> {
    fn convert_next<Iter: Iterator<Item = MemoryValue<T>>>(
        iter: &mut Iter,
    ) -> Option<Self> {
        let mut location: Option<Pointer> = None;
        let mut wrapper = std::iter::from_fn(|| {
            let item = iter.next()?;
            location.get_or_insert(item.location);
            Some(item.value)
        });
        <[T; N] as IterConversion<T>>::convert_next(&mut wrapper).map(|value| {
            MemoryValue {
                location: location.unwrap(),
                value,
            }
        })
    }

    fn convert_next_back<Iter: DoubleEndedIterator<Item = MemoryValue<T>>>(
        iter: &mut Iter,
    ) -> Option<Self> {
        let mut array = std::array::from_fn(|_| MaybeUninit::uninit());
        let mut location = None;

        for (i, element) in array.iter_mut().enumerate().rev() {
            if let Some(val) = iter.next_back() {
                location = Some(val.location);
                element.write(val.value);
            } else {
                array[i + 1..]
                    .iter_mut()
                    .for_each(|item| unsafe { item.assume_init_drop() });
                return None;
            }
        }
        let value = array.map(|item| unsafe { item.assume_init() });
        Some(MemoryValue {
            location: location.unwrap(),
            value,
        })
    }
}

impl<T: std::fmt::Display> std::fmt::Display for MemoryValue<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{} => {}]", self.location, self.value)
    }
}

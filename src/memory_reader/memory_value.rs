use super::Pointer;

pub struct MemoryValue<T> {
    pub location: Pointer,
    pub value: T,
}

impl<T> MemoryValue<T> {
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

// pub trait CollectBytes<U>: Iterator<Item = MemoryValue<U>>
// where
//     U: Into<u8>,
// {
//     fn iter_byte_arr<const N: usize>(self) -> ByteCollector<Self, U, N>
//     where
//         Self: Sized,
//     {
//         ByteCollector::new(self)
//     }
// }

// impl<T, U> CollectBytes<U> for T
// where
//     T: Iterator<Item = MemoryValue<U>>,
//     U: Into<u8>,
// {
// }

// pub struct ByteCollector<I, U, const N: usize>
// where
//     I: Iterator<Item = MemoryValue<U>>,
//     U: Into<u8>,
// {
//     iter: I,
// }

// impl<I, U, const N: usize> ByteCollector<I, U, N>
// where
//     I: Iterator<Item = MemoryValue<U>>,
//     U: Into<u8>,
// {
//     fn new(iter: I) -> Self {
//         Self { iter }
//     }
// }

// impl<I, U, const N: usize> Iterator for ByteCollector<I, U, N>
// where
//     I: Iterator<Item = MemoryValue<U>>,
//     U: Into<u8>,
// {
//     type Item = MemoryValue<[u8; N]>;
//     fn next(&mut self) -> Option<Self::Item> {
//         let mut location = Pointer::null();
//         let mut value = [0; N];
//         for i in 0..N {
//             if let Some(val) = self.iter.next() {
//                 if i == 0 {
//                     location = val.location;
//                 }
//                 value[i] = val.value.into();
//             } else {
//                 return None;
//             }
//         }
//         Some(Self::Item { location, value })
//     }
// }

pub trait CollectBytes: Iterator<Item = MemoryValue<u8>> {
    fn iter_byte_arr<const N: usize>(self) -> ByteCollector<Self, N>
    where
        Self: Sized,
    {
        ByteCollector::new(self)
    }
}

impl<T> CollectBytes for T where T: Iterator<Item = MemoryValue<u8>> {}

pub struct ByteCollector<I, const N: usize>
where
    I: Iterator<Item = MemoryValue<u8>>,
{
    iter: I,
}

impl<I, const N: usize> ByteCollector<I, N>
where
    I: Iterator<Item = MemoryValue<u8>>,
{
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
        for i in 0..N {
            if let Some(val) = self.iter.next() {
                if i == 0 {
                    location = val.location;
                }
                value[i] = val.value;
            } else {
                return None;
            }
        }
        Some(Self::Item { location, value })
    }
}

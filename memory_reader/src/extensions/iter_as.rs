use std::{marker::PhantomData, mem::MaybeUninit};

pub trait IterAs: Sized {
    fn iter_as<T>(self) -> IterConverter<Self, T> {
        IterConverter {
            iter: self,
            _phantom: PhantomData,
        }
    }
}

impl<Iter> IterAs for Iter where Iter: Iterator {}

pub trait IterConversion<T>: Sized {
    fn convert_next<Iter: Iterator<Item = T>>(iter: &mut Iter) -> Option<Self>;

    fn convert_next_back<Iter: DoubleEndedIterator<Item = T>>(
        iter: &mut Iter,
    ) -> Option<Self>;
}

pub struct IterConverter<Iter, T> {
    iter: Iter,
    _phantom: PhantomData<T>,
}

impl<Iter, T> Iterator for IterConverter<Iter, T>
where
    Iter: Iterator,
    T: IterConversion<Iter::Item>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        <T as IterConversion<Iter::Item>>::convert_next(&mut self.iter)
    }
}

impl<Iter, T> DoubleEndedIterator for IterConverter<Iter, T>
where
    Iter: DoubleEndedIterator,
    T: IterConversion<Iter::Item>,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        <T as IterConversion<Iter::Item>>::convert_next_back(&mut self.iter)
    }
}

impl<const N: usize, T> IterConversion<T> for [T; N] {
    fn convert_next<Iter: Iterator<Item = T>>(iter: &mut Iter) -> Option<Self> {
        let mut array = std::array::from_fn(|_| MaybeUninit::uninit());

        for (i, element) in array.iter_mut().enumerate() {
            if let Some(val) = iter.next() {
                element.write(val);
            } else {
                array[..i]
                    .iter_mut()
                    .for_each(|item| unsafe { item.assume_init_drop() });
                return None;
            }
        }
        Some(array.map(|item| unsafe { item.assume_init() }))
    }

    fn convert_next_back<Iter: DoubleEndedIterator<Item = T>>(
        iter: &mut Iter,
    ) -> Option<Self> {
        let mut array = std::array::from_fn(|_| MaybeUninit::uninit());

        for (i, element) in array.iter_mut().enumerate().rev() {
            if let Some(val) = iter.next_back() {
                element.write(val);
            } else {
                array[i + 1..]
                    .iter_mut()
                    .for_each(|item| unsafe { item.assume_init_drop() });
                return None;
            }
        }
        Some(array.map(|item| unsafe { item.assume_init() }))
    }
}

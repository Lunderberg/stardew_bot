use std::mem::MaybeUninit;

pub trait IterAsExtension: Sized {
    fn iter_as<T>(self) -> <Self as IterAs<T>>::OutputIter
    where
        Self: IterAs<T>,
    {
        <Self as IterAs<T>>::iter_as_impl(self)
    }
}

pub trait IterAs<T> {
    type OutputIter: Iterator<Item = T>;
    fn iter_as_impl(self) -> Self::OutputIter;
}

impl<const N: usize, T, Iter> IterAs<[T; N]> for Iter
where
    Iter: Iterator<Item = T>,
{
    type OutputIter = IterAsArray<Iter, N>;
    fn iter_as_impl(self) -> Self::OutputIter {
        IterAsArray { iter: self }
    }
}

pub struct IterAsArray<I, const N: usize> {
    iter: I,
}

impl<const N: usize, T, Iter> Iterator for IterAsArray<Iter, N>
where
    Iter: Iterator<Item = T>,
{
    type Item = [T; N];
    fn next(&mut self) -> Option<Self::Item> {
        let mut array = std::array::from_fn(|_| MaybeUninit::uninit());

        for (i, element) in array.iter_mut().enumerate() {
            if let Some(val) = self.iter.next() {
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
}

impl<const N: usize, T, Iter> DoubleEndedIterator for IterAsArray<Iter, N>
where
    Iter: DoubleEndedIterator<Item = T>,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        let mut array = std::array::from_fn(|_| MaybeUninit::uninit());

        for (i, element) in array.iter_mut().enumerate().rev() {
            if let Some(val) = self.iter.next_back() {
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

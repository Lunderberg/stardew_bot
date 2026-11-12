use crate::Vector;

pub trait TileSet: Sized + Copy {
    fn iter(self) -> impl Iterator<Item = Vector<isize>>;
}
impl TileSet for Vector<isize> {
    fn iter(self) -> impl Iterator<Item = Vector<isize>> {
        std::iter::once(self)
    }
}

mod sealed {
    use super::*;
    pub trait TileIter {
        fn transform_iter(
            iter: impl Iterator<Item = Self>,
        ) -> impl Iterator<Item = Vector<isize>>;
    }

    impl<Item> TileIter for Item
    where
        Item: Into<Vector<isize>>,
    {
        fn transform_iter(
            iter: impl Iterator<Item = Self>,
        ) -> impl Iterator<Item = Vector<isize>> {
            iter.map(Into::into)
        }
    }

    impl TileIter for Option<Vector<isize>> {
        fn transform_iter(
            iter: impl Iterator<Item = Self>,
        ) -> impl Iterator<Item = Vector<isize>> {
            iter.flatten()
        }
    }
}

impl<Iter, Item> TileSet for Iter
where
    Iter: Copy,
    Iter: IntoIterator<Item = Item>,
    Item: sealed::TileIter,
{
    fn iter(self) -> impl Iterator<Item = Vector<isize>> {
        <Item as sealed::TileIter>::transform_iter(self.into_iter())
    }
}

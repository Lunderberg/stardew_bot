use crate::Vector;

pub trait TileSet: Sized + Copy {
    fn iter(self) -> impl Iterator<Item = Vector<isize>>;
}
impl TileSet for Vector<isize> {
    fn iter(self) -> impl Iterator<Item = Vector<isize>> {
        std::iter::once(self)
    }
}
impl<Iter> TileSet for Iter
where
    Iter: Copy,
    Iter: IntoIterator,
    <Iter as IntoIterator>::Item: Into<Vector<isize>>,
{
    fn iter(self) -> impl Iterator<Item = Vector<isize>> {
        self.into_iter().map(Into::into)
    }
}

pub trait ExtendIter: Iterator {
    fn extend<Item>(self, item: Item) -> impl Iterator<Item = Self::Item>
    where
        Self: Sized,
        Item: Into<Self::Item>,
    {
        self.chain(std::iter::once(item.into()))
    }
}

impl<Iter: Iterator> ExtendIter for Iter {}

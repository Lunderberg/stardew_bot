use crate::game_state::{Item, ItemId};

pub trait ItemSet: Sized {
    fn iter_item_set(self) -> impl Iterator<Item = Item>;
}
impl ItemSet for Item {
    fn iter_item_set(self) -> impl Iterator<Item = Item> {
        std::iter::once(self)
    }
}
impl ItemSet for ItemId {
    fn iter_item_set(self) -> impl Iterator<Item = Item> {
        std::iter::once(self.into())
    }
}
impl<Iter, IterItem> ItemSet for Iter
where
    Iter: IntoIterator<Item = IterItem>,
    IterItem: Into<Item>,
{
    fn iter_item_set(self) -> impl Iterator<Item = Item> {
        self.into_iter().map(Into::into)
    }
}

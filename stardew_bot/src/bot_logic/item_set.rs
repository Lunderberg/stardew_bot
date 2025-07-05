use crate::game_state::Item;

pub trait ItemSet: Sized {
    fn iter_item_set(self) -> impl Iterator<Item = Item>;
}
impl ItemSet for Item {
    fn iter_item_set(self) -> impl Iterator<Item = Item> {
        std::iter::once(self)
    }
}
impl<Iter> ItemSet for Iter
where
    Iter: IntoIterator<Item = Item>,
{
    fn iter_item_set(self) -> impl Iterator<Item = Item> {
        self.into_iter()
    }
}

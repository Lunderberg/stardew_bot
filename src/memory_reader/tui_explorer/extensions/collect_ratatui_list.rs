use ratatui::widgets::{List, ListItem};

// TODO: Replace `List::new` with `.collect::<List>()` when
// ratatui 0.26 is released.
pub trait CollectRatatuiList<'a> {
    fn collect_list(self) -> List<'a>;
}

impl<'a, Iter> CollectRatatuiList<'a> for Iter
where
    Iter: IntoIterator,
    Iter::Item: Into<ListItem<'a>>,
{
    fn collect_list(self) -> List<'a> {
        List::new(self)
    }
}

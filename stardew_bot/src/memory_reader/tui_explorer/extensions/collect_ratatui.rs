use ratatui::{
    layout::Constraint,
    widgets::{Cell, List, ListItem, Row, Table},
};

// TODO: Replace `.collect_list` with `.collect::<List>()` when
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

// TODO: Replace `.collect_table` with `.collect::<Table>()` when
// ratatui 0.26 is released.
pub trait CollectRatatuiTable<'a> {
    fn collect_table(self) -> Table<'a>;
}

impl<'a, Iter> CollectRatatuiTable<'a> for Iter
where
    Iter: IntoIterator,
    Iter::Item: Into<Row<'a>>,
{
    fn collect_table(self) -> Table<'a> {
        let widths: [Constraint; 0] = [];
        Table::new(self.into_iter().map(Into::into), widths)
    }
}

// TODO: Replace `.collect_row` with `.collect::<Row>()` when
// ratatui 0.26 is released.
pub trait CollectRatatuiRow<'a> {
    fn collect_row(self) -> Row<'a>;
}

impl<'a, Iter> CollectRatatuiRow<'a> for Iter
where
    Iter: IntoIterator,
    Iter::Item: Into<Cell<'a>>,
{
    fn collect_row(self) -> Row<'a> {
        Row::new(self)
    }
}

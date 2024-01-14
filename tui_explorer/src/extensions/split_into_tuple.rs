use itertools::{traits::HomogeneousTuple, Itertools as _};
use ratatui::layout::{Layout, Rect};

pub trait SplitIntoTuple<Tuple> {
    fn split_tuple(self, area: Rect) -> Tuple;
}

impl<Tuple: HomogeneousTuple<Item = Rect>> SplitIntoTuple<Tuple> for Layout {
    fn split_tuple(self, area: Rect) -> Tuple {
        self.split(area)
            .into_iter()
            .cloned()
            .collect_tuple()
            .expect("Incorrect number of tuple elements")
    }
}

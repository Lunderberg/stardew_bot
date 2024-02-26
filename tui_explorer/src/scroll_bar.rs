use ratatui::{
    layout::{Constraint, Direction, Layout, Rect},
    widgets::{StatefulWidget, Widget},
};

use crate::extensions::*;

use crate::VerticalBar;

pub(crate) struct WithScrollbar<Inner> {
    inner: Inner,
    num_rows: usize,
    num_header_rows: usize,
}

pub(crate) trait Scrollable: StatefulWidget {
    fn selected_row(state: &Self::State) -> usize;

    fn select_row(state: &mut Self::State, row: usize);

    fn make_scrollbar(
        &self,
        num_rows: usize,
        area: Rect,
        state: &Self::State,
    ) -> impl Widget {
        let selected_row = Self::selected_row(state);
        let rows_shown = area.height as usize;
        let (top_ratio, bottom_ratio) = if selected_row < rows_shown {
            (0.0, (rows_shown as f64) / (num_rows as f64))
        } else if selected_row > num_rows - rows_shown {
            (((num_rows - rows_shown) as f64) / (num_rows as f64), 1.0)
        } else {
            (
                ((selected_row - rows_shown / 2) as f64) / (num_rows as f64),
                ((selected_row + rows_shown / 2) as f64) / (num_rows as f64),
            )
        };
        let bar = VerticalBar::default()
            .bar_top_ratio(top_ratio)
            .bar_bottom_ratio(bottom_ratio);
        bar
    }
}

impl<Inner> WithScrollbar<Inner> {
    pub(crate) fn new(inner: Inner, num_rows: usize) -> Self {
        WithScrollbar {
            inner,
            num_rows,
            num_header_rows: 2,
        }
    }

    pub(crate) fn _num_header_rows(self, num_header_rows: usize) -> Self {
        Self {
            num_header_rows,
            ..self
        }
    }

    fn split_scroll_area(&self, area: Rect) -> (Rect, Rect) {
        let (left_column, inner_area) = Layout::default()
            .direction(Direction::Horizontal)
            .margin(0)
            .constraints([Constraint::Min(1), Constraint::Percentage(100)])
            .split_tuple(area);

        let (_, scrollbar_area) = Layout::default()
            .direction(Direction::Vertical)
            .margin(0)
            .constraints([
                Constraint::Min(self.num_header_rows as u16),
                Constraint::Percentage(100),
            ])
            .split_tuple(left_column);

        (scrollbar_area, inner_area)
    }
}

impl<Inner> StatefulWidget for WithScrollbar<Inner>
where
    Inner: Scrollable,
{
    type State = Inner::State;

    fn render(
        self,
        area: Rect,
        buf: &mut ratatui::prelude::Buffer,
        state: &mut Self::State,
    ) {
        let (scrollbar_area, inner_area) = self.split_scroll_area(area);

        self.inner
            .make_scrollbar(self.num_rows, area, state)
            .render(scrollbar_area, buf);
        self.inner.render(inner_area, buf, state);
    }
}

impl Scrollable for ratatui::widgets::Table<'_> {
    fn selected_row(state: &Self::State) -> usize {
        state.selected().unwrap_or(0)
    }

    fn select_row(state: &mut Self::State, row: usize) {
        state.select(Some(row))
    }
}

impl Scrollable for ratatui::widgets::List<'_> {
    fn selected_row(state: &Self::State) -> usize {
        state.selected().unwrap_or(0)
    }

    fn select_row(state: &mut Self::State, row: usize) {
        state.select(Some(row))
    }
}

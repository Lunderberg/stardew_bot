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

pub(crate) trait ScrollableState {
    fn selected_row(&self) -> Option<usize>;

    fn select_row(&mut self, row: Option<usize>);
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

    fn make_scrollbar(&self, area: Rect, state: &Inner::State) -> impl Widget
    where
        Inner: StatefulWidget,
        Inner::State: ScrollableState,
    {
        let selected_row = state.selected_row().unwrap_or(0);
        let rows_shown = area.height as usize;
        let (top_ratio, bottom_ratio) = if selected_row < rows_shown {
            (0.0, (rows_shown as f64) / (self.num_rows as f64))
        } else if selected_row > self.num_rows - rows_shown {
            (
                ((self.num_rows - rows_shown) as f64) / (self.num_rows as f64),
                1.0,
            )
        } else {
            (
                ((selected_row - rows_shown / 2) as f64)
                    / (self.num_rows as f64),
                ((selected_row + rows_shown / 2) as f64)
                    / (self.num_rows as f64),
            )
        };
        let bar = VerticalBar::default()
            .bar_top_ratio(top_ratio)
            .bar_bottom_ratio(bottom_ratio);
        bar
    }
}

impl<Inner> StatefulWidget for WithScrollbar<Inner>
where
    Inner: StatefulWidget,
    Inner::State: ScrollableState,
{
    type State = Inner::State;

    fn render(
        self,
        area: Rect,
        buf: &mut ratatui::prelude::Buffer,
        state: &mut Self::State,
    ) {
        let (scrollbar_area, inner_area) = self.split_scroll_area(area);

        self.make_scrollbar(area, state).render(scrollbar_area, buf);
        self.inner.render(inner_area, buf, state);
    }
}

impl ScrollableState for ratatui::widgets::TableState {
    fn selected_row(&self) -> Option<usize> {
        self.selected()
    }

    fn select_row(&mut self, row: Option<usize>) {
        self.select(row)
    }
}

impl ScrollableState for ratatui::widgets::ListState {
    fn selected_row(&self) -> Option<usize> {
        self.selected()
    }

    fn select_row(&mut self, row: Option<usize>) {
        self.select(row)
    }
}

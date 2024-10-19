use ratatui::{
    layout::Rect,
    style::{Modifier, Style},
    text::Line,
    widgets::{List, ListState, StatefulWidget, Widget},
};

use crate::{
    extensions::SplitRect as _,
    inputs::{KeyBindingMatch, KeySequence},
    widgets::VerticalBar,
};

pub struct WithScrollbar<Inner> {
    inner: Inner,
    num_rows: usize,
    num_header_rows: usize,
    numbered: bool,
}

pub trait ScrollableState {
    fn selected_row(&self) -> Option<usize>;

    fn select_row(&mut self, row: Option<usize>);

    fn current_offset(&self) -> usize;

    fn apply_key_binding(
        &mut self,
        keystrokes: &KeySequence,
        num_rows: usize,
        page_up_down_size: usize,
    ) -> KeyBindingMatch {
        KeyBindingMatch::Mismatch
            .or_try_bindings(["<down>", "C-n"], keystrokes, || {
                self.move_selection_relative(num_rows, 1)
            })
            .or_try_bindings(["<up>", "C-p"], keystrokes, || {
                self.move_selection_relative(num_rows, -1)
            })
            .or_try_bindings(["<pageup>", "C-v"], keystrokes, || {
                self.move_selection_relative(
                    num_rows,
                    -(page_up_down_size as i64),
                )
            })
            .or_try_bindings(["<pagedown>", "M-v"], keystrokes, || {
                self.move_selection_relative(num_rows, page_up_down_size as i64)
            })
            .or_try_bindings(["C-<home>", "M-<"], keystrokes, || {
                self.select_row(Some(0))
            })
            .or_try_bindings(["C-<end>", "M->"], keystrokes, || {
                self.select_row(Some(num_rows - 1))
            })
    }

    fn move_selection_relative(&mut self, num_rows: usize, delta: i64) {
        let row = match (self.selected_row(), delta.signum()) {
            // If no prior selection, moving down a line selects the
            // first element, but still allows a page down.
            (None, 1) => (delta as usize) - 1,

            // Wrapping to the end of the list selects the last
            // element, regardless of step size.
            (None | Some(0), -1) => num_rows - 1,

            // Wrapping to the beginning of the list selects the first
            // element, regardless of step size.
            (Some(i), 1) if i == num_rows - 1 => 0,

            // Otherwise, go in the direction specified, but capped at
            // the endpoint.
            (Some(i), -1) => ((i as i64) + delta).max(0) as usize,
            (Some(i), 1) => (i + (delta as usize)).min(num_rows - 1),
            _ => panic!("This shouldn't happen"),
        };
        self.select_row(Some(row));
    }
}

impl<Inner> WithScrollbar<Inner> {
    pub fn new(inner: Inner, num_rows: usize) -> Self {
        WithScrollbar {
            inner,
            num_rows,
            num_header_rows: 0,
            numbered: false,
        }
    }

    pub fn num_header_rows(self, num_header_rows: usize) -> Self {
        Self {
            num_header_rows,
            ..self
        }
    }

    pub fn number_each_row(self) -> Self {
        Self {
            numbered: true,
            ..self
        }
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
        let (left_column, inner_area) = area.split_from_left(1);
        let (_, scrollbar_area) =
            left_column.split_from_top(self.num_header_rows as u16);

        let (sidebar, inner_area) = if self.numbered {
            let ncols = (self.num_rows + 1).ilog10() as u16;
            let (sidebar, inner_area) = inner_area.split_from_left(ncols);

            (Some(sidebar), inner_area)
        } else {
            (None, inner_area)
        };

        self.make_scrollbar(area, state).render(scrollbar_area, buf);
        self.inner.render(inner_area, buf, state);

        if let Some(sidebar) = sidebar {
            let offset = state.current_offset();
            let lines = (0..sidebar.height as usize)
                .map(|i| Line::raw(format!("{}", i + offset)));
            let widget = List::new(lines)
                .highlight_style(
                    Style::default().add_modifier(Modifier::REVERSED),
                )
                .style(Style::default().fg(ratatui::style::Color::Gray));
            let mut dummy_state = ListState::default()
                .with_offset(0)
                .with_selected(state.selected_row().map(|sel| sel - offset));
            StatefulWidget::render(widget, sidebar, buf, &mut dummy_state);
        }
    }
}

impl ScrollableState for ratatui::widgets::TableState {
    fn selected_row(&self) -> Option<usize> {
        self.selected()
    }

    fn select_row(&mut self, row: Option<usize>) {
        self.select(row)
    }

    fn current_offset(&self) -> usize {
        self.offset()
    }
}

impl ScrollableState for ratatui::widgets::ListState {
    fn selected_row(&self) -> Option<usize> {
        self.selected()
    }

    fn select_row(&mut self, row: Option<usize>) {
        self.select(row)
    }

    fn current_offset(&self) -> usize {
        self.offset()
    }
}

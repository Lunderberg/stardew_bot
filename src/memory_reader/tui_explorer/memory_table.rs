use tui::{
    backend::Backend,
    layout::{Constraint, Rect},
    style::{Color, Modifier, Style},
    widgets::{Block, Borders, Cell, Row, Table, TableState},
    Frame,
};

use crate::memory_reader::{CollectBytes, MemoryRegion, MemoryValue, Pointer};

pub struct MemoryTable {
    state: TableState,
    region: MemoryRegion,
    previous_height: Option<usize>,
}

const POINTER_SIZE: usize = 8;

impl MemoryTable {
    pub fn new(region: MemoryRegion) -> Self {
        Self {
            state: TableState::default(),
            region,
            previous_height: None,
        }
    }

    pub fn selected_value(&self) -> MemoryValue<Pointer> {
        let selected = self.state.selected().unwrap_or(0);
        let byte_offset = selected * POINTER_SIZE;
        self.region
            .bytes_at_offset(byte_offset)
            .map(|bytes| bytes.into())
    }

    fn move_selection(&mut self, delta: i64) {
        let n = self.table_size();
        let i = match (self.state.selected(), delta.signum()) {
            // If no prior selection, moving down a line selects the
            // first element, but still allows a page down.
            (None, 1) => (delta as usize) - 1,

            // Wrapping to the end of the list selects the last
            // element, regardless of step size.
            (None | Some(0), -1) => n - 1,

            // Wrapping to the beginning of the list selects the first
            // element, regardless of step size.
            (Some(i), 1) if i == n - 1 => 0,

            // Otherwise, go in the direction specified, but capped at
            // the endpoint.
            (Some(i), -1) => ((i as i64) + delta).max(0) as usize,
            (Some(i), 1) => (i + (delta as usize)).min(n - 1),
            // (i + delta) % self.table_size(),
            _ => panic!("This shouldn't happen"),
        };
        self.state.select(Some(i));
    }

    pub fn move_selection_down(&mut self) {
        self.move_selection(1);
    }

    pub fn move_selection_up(&mut self) {
        self.move_selection(-1);
    }

    pub fn move_selection_page_down(&mut self) {
        self.move_selection(self.displayed_rows() as i64);
    }

    pub fn move_selection_page_up(&mut self) {
        self.move_selection(-(self.displayed_rows() as i64));
    }

    fn displayed_rows(&self) -> usize {
        let non_data_rows = 5;
        self.previous_height
            .map(|height| height - non_data_rows)
            .unwrap_or(1)
    }

    fn table_size(&self) -> usize {
        self.region.size_bytes() / POINTER_SIZE
    }

    pub fn draw<B: Backend>(&mut self, frame: &mut Frame<B>, area: Rect) {
        self.previous_height = Some(area.height as usize);

        let selected_style = Style::default().add_modifier(Modifier::REVERSED);
        let normal_style = Style::default().bg(Color::Blue);

        let header_cells = ["Address", "Hex"].iter().map(|h| {
            Cell::from(*h).style(
                Style::default()
                    .fg(Color::LightCyan)
                    .add_modifier(Modifier::BOLD),
            )
        });
        let header = Row::new(header_cells)
            .style(normal_style)
            .height(1)
            .bottom_margin(1);
        let rows = self.region.iter_bytes().iter_byte_arr().enumerate().map(
            |(i, arr): (_, MemoryValue<[u8; 8]>)| {
                let selected = self.state.selected().unwrap_or(0);
                let dist_to_selected = if i > selected {
                    i - selected
                } else {
                    selected - i
                };
                let is_near_selected =
                    dist_to_selected < (area.height as usize);

                // TODO: Use abs_diff, stabilized in 1.60
                //
                // let is_near_selected =
                //     self.state.selected().unwrap_or(0).abs_diff(i) < area.height;

                let cells = if is_near_selected {
                    let as_pointer: Pointer = arr.value.into();
                    vec![
                        Cell::from(format!("{}", arr.location)),
                        Cell::from(format!("{}", as_pointer)),
                    ]
                } else {
                    vec![]
                };
                Row::new(cells.into_iter()).height(1).bottom_margin(0)
            },
        );

        let table = Table::new(rows)
            .header(header)
            .block(Block::default().borders(Borders::ALL).title("Memory table"))
            .highlight_style(selected_style)
            .highlight_symbol(">> ")
            .widths(&[
                Constraint::Min(19),
                Constraint::Percentage(50),
                Constraint::Min(5),
            ]);
        frame.render_stateful_widget(table, area, &mut self.state);
    }
}

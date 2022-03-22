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
}

const POINTER_SIZE: usize = 8;

impl MemoryTable {
    pub fn new(region: MemoryRegion) -> Self {
        Self {
            state: TableState::default(),
            region,
        }
    }

    pub fn selected_value(&self) -> MemoryValue<Pointer> {
        let selected = self.state.selected().unwrap_or(0);
        let byte_offset = selected * POINTER_SIZE;
        self.region
            .bytes_at_offset(byte_offset)
            .map(|bytes| bytes.into())
    }

    pub fn move_selection_down(&mut self) {
        let i = match self.state.selected() {
            Some(i) => (i + 1) % self.table_size(),
            None => 0,
        };
        self.state.select(Some(i));
    }

    pub fn move_selection_up(&mut self) {
        let i = match self.state.selected() {
            Some(i) => (i + (self.table_size() - 1)) % self.table_size(),
            None => self.table_size() - 1,
        };
        self.state.select(Some(i));
    }

    fn table_size(&self) -> usize {
        self.region.size_bytes() / POINTER_SIZE
    }

    pub fn draw<B: Backend>(&mut self, frame: &mut Frame<B>, area: Rect) {
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

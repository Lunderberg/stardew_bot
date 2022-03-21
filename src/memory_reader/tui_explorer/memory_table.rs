use tui::{
    backend::Backend,
    layout::{Constraint, Rect},
    style::{Color, Modifier, Style},
    widgets::{Block, Borders, Cell, Row, Table, TableState},
    Frame,
};

use crate::memory_reader::{
    CollectBytes, MemoryReader, MemoryRegion, MemoryValue, Pointer, Result,
};

pub struct MemoryTable {
    state: TableState,
    _reader: MemoryReader,
    region: MemoryRegion,
}

impl MemoryTable {
    pub fn new(pid: u32) -> Result<Self> {
        let reader = MemoryReader::new(pid)?;
        let region = reader.stack()?.read()?;
        Ok(Self {
            state: TableState::default(),
            _reader: reader,
            region,
        })
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
        self.region.size_bytes() / 8
    }

    pub fn draw<B: Backend>(&mut self, frame: &mut Frame<B>, area: Rect) {
        let selected_style = Style::default().add_modifier(Modifier::REVERSED);
        let normal_style = Style::default().bg(Color::Blue);

        let header_cells = ["Address", "Hex", "Temp_i"].iter().map(|h| {
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
                let as_pointer: Pointer =
                    usize::from_ne_bytes(arr.value).into();
                let cells = [
                    Cell::from(format!("{}", arr.location)),
                    Cell::from(format!("{}", as_pointer)),
                    Cell::from(format!("{}", i)),
                ];
                Row::new(cells).height(1).bottom_margin(0)
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

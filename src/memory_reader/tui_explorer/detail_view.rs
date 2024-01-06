use ratatui::{
    layout::{Constraint, Rect},
    widgets::{Block, Borders, Cell, Row, Table, TableState},
    Frame,
};

use crate::{
    memory_reader::{MemoryRegion, Pointer},
    MemoryReader,
};

use super::InfoFormatter;

pub struct DetailView {
    state: TableState,
    values: Vec<(&'static str, String)>,
    formatters: Vec<Box<dyn InfoFormatter>>,
}

impl DetailView {
    pub fn new(formatters: Vec<Box<dyn InfoFormatter>>) -> Self {
        Self {
            state: TableState::default(),
            values: Vec::new(),
            formatters,
        }
    }

    pub fn update_details(
        &mut self,
        reader: &MemoryReader,
        region: &MemoryRegion,
        pointer: Pointer,
    ) {
        self.values = self
            .formatters
            .iter()
            .filter_map(|formatter| {
                formatter
                    .format(reader, region, pointer)
                    .map(|text| (formatter.name(), text))
            })
            .collect()
    }

    pub fn draw(&mut self, frame: &mut Frame, area: Rect) {
        let rows = self.values.iter().map(|(key, value)| {
            let height = value.chars().filter(|c| *c == '\n').count() + 1;
            let cells = [Cell::from(*key), Cell::from(value.as_str())];
            Row::new(cells).height(height as u16)
        });

        let table = Table::new(
            rows,
            [
                Constraint::Percentage(50),
                Constraint::Length(30),
                Constraint::Min(10),
            ],
        )
        .block(Block::default().borders(Borders::ALL).title("Detail View"));
        frame.render_stateful_widget(table, area, &mut self.state);
    }
}

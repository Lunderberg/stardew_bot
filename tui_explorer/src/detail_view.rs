use ratatui::{
    layout::{Constraint, Rect},
    widgets::{Block, Borders, Cell, Row, Table, TableState},
    Frame,
};

use memory_reader::{MemoryReader, MemoryRegion, Pointer};

use crate::{Annotation, InfoFormatter};

pub struct DetailView {
    state: TableState,
    values: Vec<(String, String)>,
    formatters: Vec<Box<dyn InfoFormatter>>,
}

impl DetailView {
    pub(crate) fn new(formatters: Vec<Box<dyn InfoFormatter>>) -> Self {
        Self {
            state: TableState::default(),
            values: Vec::new(),
            formatters,
        }
    }

    pub(crate) fn update_details(
        &mut self,
        reader: &MemoryReader,
        region: &MemoryRegion,
        annotations: &[Annotation],
        pointer: Pointer,
    ) {
        let from_annotations = annotations
            .iter()
            //.filter(|ann| ann.range.contains(&pointer))
            .filter(|ann| {
                ann.range.start < pointer + MemoryRegion::POINTER_SIZE
                    && pointer < ann.range.end
            })
            .map(|ann| (ann.name.clone(), ann.value.clone()));

        let from_formatters = self.formatters.iter().filter_map(|formatter| {
            formatter
                .format(reader, region, pointer)
                .map(|text| (formatter.name().to_string(), text))
        });

        self.values = from_annotations.chain(from_formatters).collect();
    }

    pub(crate) fn draw(&mut self, frame: &mut Frame, area: Rect) {
        let rows = self.values.iter().map(|(key, value)| {
            let height = value.chars().filter(|c| *c == '\n').count() + 1;
            let cells = [Cell::from(key.as_str()), Cell::from(value.as_str())];
            Row::new(cells).height(height as u16)
        });

        let table = Table::new(
            rows,
            [Constraint::Min(15), Constraint::Percentage(100)],
        )
        .block(Block::default().borders(Borders::ALL).title("Detail View"));
        frame.render_stateful_widget(table, area, &mut self.state);
    }
}

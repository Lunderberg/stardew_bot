use tui::{
    backend::Backend,
    layout::{Constraint, Rect},
    widgets::{Block, Borders, Cell, Row, Table, TableState},
    Frame,
};

pub struct DetailView {
    state: TableState,
    values: Vec<(String, String)>,
}

impl DetailView {
    pub fn new() -> Self {
        Self {
            state: TableState::default(),
            values: Vec::new(),
        }
    }

    pub fn load_details<I>(&mut self, details: I)
    where
        I: Iterator<Item = (String, String)>,
    {
        self.values = details.collect();
    }

    pub fn draw<B: Backend>(&mut self, frame: &mut Frame<B>, area: Rect) {
        let rows = self.values.iter().map(|(key, value)| {
            let height = value.chars().filter(|c| *c == '\n').count() + 1;
            let cells = [Cell::from(key.as_str()), Cell::from(value.as_str())];
            Row::new(cells).height(height as u16)
        });

        let table = Table::new(rows)
            .block(Block::default().borders(Borders::ALL).title("Detail View"))
            .widths(&[
                Constraint::Percentage(50),
                Constraint::Length(30),
                Constraint::Min(10),
            ]);
        frame.render_stateful_widget(table, area, &mut self.state);
    }
}

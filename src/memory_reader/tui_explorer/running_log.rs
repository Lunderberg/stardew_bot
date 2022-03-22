use std::collections::VecDeque;

use tui::{
    backend::Backend,
    layout::Rect,
    style::{Color, Modifier, Style},
    widgets::{Block, Borders, List, ListItem, ListState},
    Frame,
};

use chrono::prelude::*;

pub struct RunningLog {
    max_elements: usize,
    state: ListState,
    items: VecDeque<(DateTime<Local>, String)>,
}

impl RunningLog {
    pub fn new(max_elements: usize) -> Self {
        Self {
            max_elements,
            state: ListState::default(),
            items: VecDeque::new(),
        }
    }

    pub fn add_log(&mut self, log: String) {
        self.items.push_back((Local::now(), log));
        while self.items.len() > self.max_elements {
            self.items.pop_front();
        }
    }

    pub fn draw<B: Backend>(&mut self, frame: &mut Frame<B>, area: Rect) {
        let items: Vec<_> = self
            .items
            .iter()
            .rev()
            .map(|(timestamp, entry)| {
                ListItem::new(format!(
                    "{} {}",
                    timestamp.format("%H:%M:%S"),
                    entry
                ))
            })
            .collect();

        let running_log = List::new(items)
            .block(Block::default().title("Running info").borders(Borders::ALL))
            .highlight_style(
                Style::default()
                    .bg(Color::LightGreen)
                    .add_modifier(Modifier::BOLD),
            )
            .highlight_symbol(">> ");

        frame.render_stateful_widget(running_log, area, &mut self.state);
    }
}

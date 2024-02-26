use std::collections::VecDeque;

use ratatui::{
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
    pub(crate) fn new(max_elements: usize) -> Self {
        Self {
            max_elements,
            state: ListState::default(),
            items: VecDeque::new(),
        }
    }

    pub(crate) fn add_log(&mut self, log: impl Into<String>) {
        self.items.push_back((Local::now(), log.into()));
        while self.items.len() > self.max_elements {
            self.items.pop_front();
        }
    }

    pub(crate) fn draw(
        &mut self,
        frame: &mut Frame,
        area: Rect,
        border_style: Style,
    ) {
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
            .block(
                Block::default()
                    .title("Log")
                    .borders(Borders::ALL)
                    .border_style(border_style),
            )
            .highlight_style(
                Style::default()
                    .bg(Color::LightGreen)
                    .add_modifier(Modifier::BOLD),
            )
            .highlight_symbol(">> ");

        frame.render_stateful_widget(running_log, area, &mut self.state);
    }
}

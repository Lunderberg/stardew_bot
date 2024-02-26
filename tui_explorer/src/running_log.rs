use std::collections::VecDeque;

use ratatui::{
    layout::Rect,
    style::{Modifier, Style},
    widgets::{Block, Borders, List, ListItem, ListState},
    Frame,
};

use chrono::prelude::*;

use crate::{KeyBindingMatch, KeySequence, ScrollableState as _};

pub struct RunningLog {
    max_elements: usize,
    state: ListState,
    items: VecDeque<(DateTime<Local>, String)>,
    prev_draw_height: usize,
}

impl RunningLog {
    pub(crate) fn new(max_elements: usize) -> Self {
        Self {
            max_elements,
            state: ListState::default(),
            items: VecDeque::new(),
            prev_draw_height: 1,
        }
    }

    pub(crate) fn add_log(&mut self, log: impl Into<String>) {
        self.items.push_back((Local::now(), log.into()));
        while self.items.len() > self.max_elements {
            self.items.pop_front();
        }
        if let Some(selected) = self.state.selected() {
            let selected = selected + 1;
            let selected = if selected < self.items.len() {
                Some(selected)
            } else {
                None
            };
            self.state.select(selected)
        }
    }

    pub(crate) fn apply_key_binding(
        &mut self,
        keystrokes: &KeySequence,
    ) -> KeyBindingMatch {
        KeyBindingMatch::Mismatch.or_else(|| {
            self.state.apply_key_binding(
                keystrokes,
                self.items.len(),
                self.prev_draw_height - 3,
            )
        })
    }

    pub(crate) fn draw(
        &mut self,
        frame: &mut Frame,
        area: Rect,
        border_style: Style,
    ) {
        self.prev_draw_height = area.height as usize;

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
            .highlight_style(Style::default().add_modifier(Modifier::REVERSED))
            .highlight_symbol(">> ");

        frame.render_stateful_widget(running_log, area, &mut self.state);
    }
}

use ratatui::{
    layout::Rect,
    widgets::{Block, Borders, Paragraph},
    Frame,
};

use crate::{KeyBindingMatch, KeySequence};

pub struct InputWindow {
    title: &'static str,
    chars: Vec<char>,
}

impl InputWindow {
    pub(crate) fn new(title: &'static str) -> Self {
        Self {
            title,
            chars: Vec::new(),
        }
    }

    pub(crate) fn apply_key_binding(
        &mut self,
        keystrokes: &KeySequence,
    ) -> KeyBindingMatch {
        KeyBindingMatch::Mismatch
            .or_try_binding("<backspace>", keystrokes, || {
                self.chars.pop();
            })
            .or_else(|| {
                if let Some(c) = keystrokes.as_char() {
                    self.chars.push(c);
                    KeyBindingMatch::Full
                } else {
                    KeyBindingMatch::Mismatch
                }
            })
    }

    pub(crate) fn text(self) -> String {
        self.chars.into_iter().collect()
    }

    pub(crate) fn draw(&self, frame: &mut Frame, area: Rect) {
        let title = self.title;

        let widget =
            Paragraph::new(self.chars.iter().cloned().collect::<String>())
                .block(Block::default().borders(Borders::ALL).title(title));
        frame.render_widget(widget, area);
    }
}

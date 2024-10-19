use crate::inputs::{KeyBindingMatch, KeySequence};
use ratatui::{
    layout::Rect,
    widgets::{Block, Borders, Paragraph},
};

pub struct InputWindow {
    title: &'static str,
    chars: Vec<char>,
}

impl InputWindow {
    pub fn new(title: &'static str) -> Self {
        Self {
            title,
            chars: Vec::new(),
        }
    }

    pub fn apply_key_binding(
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

    pub fn text(self) -> String {
        self.chars.into_iter().collect()
    }
}

impl<'a> ratatui::widgets::Widget for &'a InputWindow {
    fn render(self, area: Rect, buf: &mut ratatui::prelude::Buffer)
    where
        Self: Sized,
    {
        let widget =
            Paragraph::new(self.chars.iter().cloned().collect::<String>())
                .block(
                    Block::default().borders(Borders::ALL).title(self.title),
                );
        widget.render(area, buf);
    }
}

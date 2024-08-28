use std::str::FromStr;

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use itertools::Itertools as _;

use crate::Error;

#[derive(Debug, PartialEq, Default)]
pub struct KeySequence {
    pub(crate) sequence: Vec<KeyEvent>,
}

pub enum KeyBindingMatch {
    Full,
    Partial,
    Mismatch,
}

impl KeySequence {
    pub fn push(&mut self, event: KeyEvent) {
        self.sequence.push(event)
    }

    pub fn clear(&mut self) {
        self.sequence.clear()
    }

    pub fn match_binding(
        binding: &str,
        keystrokes: &KeySequence,
    ) -> KeyBindingMatch {
        let binding: Self = binding.parse().unwrap();
        binding.matches(keystrokes)
    }

    pub fn matches(&self, keystrokes: &KeySequence) -> KeyBindingMatch {
        let self_seq = &self.sequence;
        let user_seq = &keystrokes.sequence;
        if user_seq.len() == self_seq.len() && self_seq == user_seq {
            KeyBindingMatch::Full
        } else if user_seq.len() < self_seq.len()
            && &self_seq[..user_seq.len()] == user_seq
        {
            KeyBindingMatch::Partial
        } else {
            KeyBindingMatch::Mismatch
        }
    }

    pub fn as_char(&self) -> Option<char> {
        (self.sequence.len() == 1)
            .then(|| {
                if let KeyEvent {
                    code: KeyCode::Char(c),
                    modifiers: KeyModifiers::NONE | KeyModifiers::SHIFT,
                    ..
                } = &self.sequence[0]
                {
                    Some(*c)
                } else {
                    None
                }
            })
            .flatten()
    }
}

impl KeyBindingMatch {
    pub fn then(self, mut callback: impl FnMut()) -> Self {
        if matches!(self, KeyBindingMatch::Full) {
            callback()
        }
        self
    }

    pub fn or_else(
        self,
        mut callback: impl FnMut() -> KeyBindingMatch,
    ) -> Self {
        let next = match self {
            KeyBindingMatch::Full => KeyBindingMatch::Mismatch,
            _ => callback(),
        };
        match (self, next) {
            (KeyBindingMatch::Full, _) | (_, KeyBindingMatch::Full) => {
                KeyBindingMatch::Full
            }
            (KeyBindingMatch::Partial, _) | (_, KeyBindingMatch::Partial) => {
                KeyBindingMatch::Partial
            }

            _ => KeyBindingMatch::Mismatch,
        }
    }

    pub fn or_try_binding(
        self,
        binding: &str,
        keystrokes: &KeySequence,
        mut callback: impl FnMut(),
    ) -> Self {
        self.or_else(|| Self::try_binding(binding, keystrokes, || callback()))
    }

    pub fn try_binding(
        binding: &str,
        keystrokes: &KeySequence,
        mut callback: impl FnMut(),
    ) -> Self {
        let res = KeySequence::match_binding(binding, keystrokes);
        if matches!(res, KeyBindingMatch::Full) {
            callback();
        }
        res
    }

    pub fn or_try_bindings<'a>(
        self,
        bindings: impl IntoIterator<Item = &'a str>,
        keystrokes: &KeySequence,
        mut callback: impl FnMut(),
    ) -> Self {
        bindings.into_iter().fold(self, |before, binding| {
            before.or_try_binding(binding, keystrokes, || callback())
        })
    }
}

impl FromStr for KeySequence {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.split_ascii_whitespace()
            .map(|word| {
                let mut chars = word.chars();

                let mut modifiers = KeyModifiers::NONE;
                let mut first;
                let mut second;

                loop {
                    first = chars.next();
                    second = chars.next();

                    match (first, second) {
                        (None, _) => {
                            return Err(Error::InvalidKeyBinding(s.to_string()))
                        }
                        (Some('C'), Some('-')) => {
                            modifiers |= KeyModifiers::CONTROL;
                        }
                        (Some('M'), Some('-')) => {
                            modifiers |= KeyModifiers::ALT;
                        }
                        _ => break,
                    }
                }

                let mut chars =
                    [first, second].into_iter().filter_map(|c| c).chain(chars);

                chars
                    .next()
                    .and_then(|c| match c {
                        '<' => {
                            let name: String = chars.collect();
                            match name.as_str() {
                                "home>" => Some(KeyCode::Home),
                                "end>" => Some(KeyCode::End),
                                "enter>" => Some(KeyCode::Enter),
                                "prior>" | "pageup>" => Some(KeyCode::PageUp),
                                "next>" | "pagedown>" => {
                                    Some(KeyCode::PageDown)
                                }
                                "up>" => Some(KeyCode::Up),
                                "down>" => Some(KeyCode::Down),
                                "left>" => Some(KeyCode::Left),
                                "right>" => Some(KeyCode::Right),
                                "backspace>" => Some(KeyCode::Backspace),
                                "" => Some(KeyCode::Char('<')),
                                _ => None,
                            }
                        }
                        c => chars.next().is_none().then(|| KeyCode::Char(c)),
                    })
                    .map(|code| KeyEvent::new(code, modifiers))
                    .ok_or_else(|| Error::InvalidKeyBinding(s.to_string()))
            })
            .collect::<Result<Vec<_>, _>>()
            .map(|sequence| KeySequence { sequence })
    }
}

impl std::fmt::Display for KeySequence {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.sequence.iter().with_position().try_for_each(
            |(position, key)| -> std::fmt::Result {
                match position {
                    itertools::Position::Middle | itertools::Position::Last => {
                        write!(f, " ")?;
                    }
                    _ => {}
                }

                if key.modifiers.contains(KeyModifiers::CONTROL) {
                    write!(f, "C-")?;
                }
                if key.modifiers.contains(KeyModifiers::ALT) {
                    write!(f, "M-")?;
                }
                match key.code {
                    KeyCode::Backspace => write!(f, "<backspace>"),
                    KeyCode::Enter => write!(f, "<enter>"),
                    KeyCode::Left => write!(f, "<left>"),
                    KeyCode::Right => write!(f, "<right>"),
                    KeyCode::Up => write!(f, "<up>"),
                    KeyCode::Down => write!(f, "<down>"),
                    KeyCode::Home => write!(f, "<home>"),
                    KeyCode::End => write!(f, "<end>"),
                    KeyCode::PageUp => write!(f, "<pageup>"),
                    KeyCode::PageDown => write!(f, "<pagedown>"),
                    KeyCode::Char(c) => write!(f, "{c}"),

                    other => panic!(
                        "KeyCode {other:?} should not appear in KeySequence"
                    ),
                }
            },
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! test_parse_success {
        ($name:ident: $str:literal => $sequence:expr) => {
            #[test]
            fn $name() -> Result<(), Error> {
                let seq = $str.parse::<KeySequence>()?;
                assert_eq!(
                    seq,
                    KeySequence {
                        sequence: $sequence,
                    }
                );
                Ok(())
            }
        };
    }

    macro_rules! test_parse_error {
        ($name:ident: $str:literal) => {
            #[test]
            fn $name() {
                let seq = $str.parse::<KeySequence>();
                assert!(
                    seq.is_err(),
                    "Expected error, but found {:?}",
                    seq.unwrap()
                );
            }
        };
    }

    test_parse_success!(parse_char: "t" => vec![KeyEvent::new(
        KeyCode::Char('t'),
        KeyModifiers::NONE
    )]);

    test_parse_success!(parse_control: "C-c" => vec![KeyEvent::new(
        KeyCode::Char('c'),
        KeyModifiers::CONTROL
    )]);

    test_parse_success!(parse_meta: "M-x" => vec![KeyEvent::new(
        KeyCode::Char('x'),
        KeyModifiers::ALT
    )]);

    test_parse_success!(parse_control_and_meta: "C-M-x" => vec![KeyEvent::new(
        KeyCode::Char('x'),
        KeyModifiers::CONTROL | KeyModifiers::ALT
    )]);

    test_parse_success!(parse_page_up: "<pageup>" => vec![KeyEvent::new(
        KeyCode::PageUp,
        KeyModifiers::NONE
    )]);

    test_parse_success!(parse_prior_as_page_up: "<prior>" => vec![KeyEvent::new(
        KeyCode::PageUp,
        KeyModifiers::NONE
    )]);

    test_parse_success!(parse_page_down: "<pagedown>" => vec![KeyEvent::new(
        KeyCode::PageDown,
        KeyModifiers::NONE
    )]);

    test_parse_success!(parse_next_as_page_down: "<next>" => vec![KeyEvent::new(
        KeyCode::PageDown,
        KeyModifiers::NONE
    )]);

    test_parse_success!(parse_home: "<home>" => vec![KeyEvent::new(
        KeyCode::Home,
        KeyModifiers::NONE
    )]);

    test_parse_success!(parse_end: "<end>" => vec![KeyEvent::new(
        KeyCode::End,
        KeyModifiers::NONE
    )]);

    test_parse_success!(parse_left: "<left>" => vec![KeyEvent::new(
        KeyCode::Left,
        KeyModifiers::NONE
    )]);

    test_parse_success!(parse_right: "<right>" => vec![KeyEvent::new(
        KeyCode::Right,
        KeyModifiers::NONE
    )]);

    test_parse_success!(parse_up: "<up>" => vec![KeyEvent::new(
        KeyCode::Up,
        KeyModifiers::NONE
    )]);

    test_parse_success!(parse_down: "<down>" => vec![KeyEvent::new(
        KeyCode::Down,
        KeyModifiers::NONE
    )]);

    test_parse_success!(parse_backspace: "<backspace>" => vec![KeyEvent::new(
        KeyCode::Backspace,
        KeyModifiers::NONE
    )]);

    test_parse_success!(parse_sequence_control: "C-x C-c" => vec![KeyEvent::new(
        KeyCode::Char('x'),
        KeyModifiers::CONTROL
    ), KeyEvent::new(
        KeyCode::Char('c'),
        KeyModifiers::CONTROL
    )]);

    test_parse_success!(parse_sequence_control_on_first: "C-x c" => vec![KeyEvent::new(
        KeyCode::Char('x'),
        KeyModifiers::CONTROL
    ), KeyEvent::new(
        KeyCode::Char('c'),
        KeyModifiers::NONE
    )]);

    test_parse_error!(parse_error_multiple_chars: "cd");
    test_parse_error!(parse_error_leading_dash: "-m");
    test_parse_error!(parse_error_multiple_dashes: "C--m");
    test_parse_error!(parse_error_unknown_special_key: "C-<asdf>");
}

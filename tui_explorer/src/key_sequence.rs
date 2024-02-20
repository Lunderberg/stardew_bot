use std::str::FromStr;

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use itertools::Itertools;

use crate::Error;

#[derive(Debug, PartialEq)]
pub struct KeySequence {
    sequence: Vec<KeyEvent>,
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
                            let name: String =
                                chars.take_while_ref(|c| *c != '>').collect();
                            match name.as_str() {
                                "home" => Some(KeyCode::Home),
                                "end" => Some(KeyCode::End),
                                "prior" | "pageup" => Some(KeyCode::PageUp),
                                "next" | "pagedown" => Some(KeyCode::PageDown),
                                "up" => Some(KeyCode::Up),
                                "down" => Some(KeyCode::Down),
                                "left" => Some(KeyCode::Left),
                                "right" => Some(KeyCode::Right),
                                "backspace" => Some(KeyCode::Backspace),
                                _ => None,
                            }
                            .filter(|_| {
                                matches!(
                                    (chars.next(), chars.next()),
                                    (Some('>'), None)
                                )
                            })
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

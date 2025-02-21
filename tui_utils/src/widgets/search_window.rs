use crate::{
    containers::NonEmptyVec,
    extensions::HighlightLine,
    inputs::{KeyBindingMatch, KeySequence},
    widgets::ScrollableState,
};
use itertools::Either;
use ratatui::{
    layout::Rect,
    style::{Color, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Paragraph},
};
use regex::{Regex, RegexBuilder};

pub struct SearchWindow<T> {
    pub stack: NonEmptyVec<SearchItem>,
    pub pre_search_state: T,
}

pub struct SearchItem {
    pub command: SearchCommand,
    pub search_result: Option<usize>,
}

#[derive(Clone, Copy)]
pub enum SearchCommand {
    NextResult(SearchDirection),
    AddChar(char),
}

#[derive(Clone, Copy)]
pub enum SearchDirection {
    Forward,
    Reverse,
}

impl<T> SearchWindow<T> {
    pub fn new(direction: SearchDirection, pre_search_state: T) -> Self
    where
        T: ScrollableState,
    {
        let command = SearchCommand::NextResult(direction);
        let initial_row = pre_search_state.selected_row().unwrap_or(0);
        let initial_item = SearchItem {
            command,
            search_result: Some(initial_row),
        };

        Self {
            stack: NonEmptyVec::new(initial_item),
            pre_search_state,
        }
    }

    // Undo the most recent command, unless it was the initial command
    // that started the search.
    pub fn pop_command(&mut self) {
        if self.stack.len() > 1 {
            self.stack.pop();
        }
    }

    pub fn highlight_search_matches<'a>(
        &self,
        line: impl Into<Line<'a>>,
    ) -> Line<'a> {
        let regex = self.get_search_regex(None);
        line.into()
            .style_regex_ref(&regex, Style::default().bg(Color::Yellow))
    }

    // Return the string being searched for.
    pub fn get_search_string(&self, last_char: Option<char>) -> String {
        self.stack
            .iter()
            .filter_map(|item| match item.command {
                SearchCommand::AddChar(c) => Some(c),
                _ => None,
            })
            .chain(last_char.iter().copied())
            .collect()
    }

    // Return a tuple of 2 strings, where the first string is the
    // portion of the search string that was found, and the second
    // string is the portion of the search string that wasn't found.
    pub fn get_search_string_parts(&self) -> (String, String) {
        let vals: Vec<(char, bool)> = self
            .stack
            .iter()
            .filter_map(|item| match item.command {
                SearchCommand::AddChar(c) => {
                    Some((c, item.search_result.is_some()))
                }
                _ => None,
            })
            .collect();

        let matching_part =
            vals.iter().filter(|(_, p)| *p).map(|(c, _)| c).collect();
        let non_matching_part =
            vals.iter().filter(|(_, p)| !*p).map(|(c, _)| c).collect();
        (matching_part, non_matching_part)
    }

    pub fn apply_key_binding(
        &mut self,
        keystrokes: &KeySequence,
        table_size: usize,
        mut row_generator: impl FnMut(usize) -> Vec<String>,
    ) -> KeyBindingMatch {
        KeyBindingMatch::Mismatch
            .or_try_binding("<backspace>", keystrokes, || self.pop_command())
            .or_try_binding("C-s", keystrokes, || {
                self.apply_command(
                    SearchCommand::NextResult(SearchDirection::Forward),
                    table_size,
                    |i| row_generator(i),
                )
            })
            .or_try_binding("C-r", keystrokes, || {
                self.apply_command(
                    SearchCommand::NextResult(SearchDirection::Reverse),
                    table_size,
                    |i| row_generator(i),
                )
            })
            .or_else(|| {
                if let Some(c) = keystrokes.as_char() {
                    self.apply_command(
                        SearchCommand::AddChar(c),
                        table_size,
                        |i| row_generator(i),
                    );
                    KeyBindingMatch::Full
                } else {
                    KeyBindingMatch::Mismatch
                }
            })
    }

    pub fn apply_command<F>(
        &mut self,
        command: SearchCommand,
        table_size: usize,
        row_generator: F,
    ) where
        F: FnMut(usize) -> Vec<String>,
    {
        use SearchCommand::*;
        use SearchDirection::*;

        let previous_result: Option<usize> = self.stack.last().search_result;

        let previous_direction = self
            .stack
            .iter()
            .rev()
            .find_map(|item| match item.command {
                NextResult(dir) => Some(dir),
                _ => None,
            })
            .expect(
                "If no others, \
                 first item in search stack should have direction",
            );

        let search_range = match (&command, previous_result, previous_direction)
        {
            (NextResult(Forward), None, _) => Some(Either::Left(0..table_size)),
            (NextResult(Forward), Some(prev), _) => {
                Some(Either::Left((prev + 1)..table_size))
            }
            (NextResult(Reverse), None, _) => {
                Some(Either::Right((0..table_size).rev()))
            }
            (NextResult(Reverse), Some(prev), _) => {
                Some(Either::Right((0..prev).rev()))
            }
            (AddChar(_), None, _) => None,
            (AddChar(_), Some(prev), Forward) => {
                Some(Either::Left(prev..table_size))
            }
            (AddChar(_), Some(prev), Reverse) => {
                Some(Either::Right((0..(prev + 1)).rev()))
            }
        };

        let new_char = match command {
            AddChar(c) => Some(c),
            _ => None,
        };

        let search_result: Option<usize> =
            search_range.and_then(|search_range| {
                self.search(search_range, new_char, row_generator)
            });

        self.stack.push(SearchItem {
            command,
            search_result,
        });
    }

    fn get_search_regex(&self, last_char: Option<char>) -> Regex {
        let needle = self.get_search_string(last_char);
        let is_case_sensitive = needle.chars().any(char::is_uppercase);

        let regex = RegexBuilder::new(regex::escape(&needle).as_str())
            .case_insensitive(!is_case_sensitive)
            .build()
            .unwrap();

        regex
    }

    pub fn search<F>(
        &mut self,
        search_range: impl IntoIterator<Item = usize>,
        last_char: Option<char>,
        mut row_generator: F,
    ) -> Option<usize>
    where
        F: FnMut(usize) -> Vec<String>,
    {
        let regex = self.get_search_regex(last_char);

        search_range.into_iter().find(|row| {
            row_generator(*row)
                .iter()
                .any(|cell_text| regex.is_match(cell_text))
        })
    }

    /// Return the index of the row that should be selected in order
    /// to display the search.
    ///
    /// If a match has occurred, select the row containing the most
    /// recent match if a match exists.  Otherwise, select the initial
    /// location of the search.
    pub fn recommended_row_selection(&self) -> usize {
        self.stack
            .iter()
            .rev()
            .find_map(|item| item.search_result)
            .expect(
                "If no others, \
                 first item in search stack should be Some(row)",
            )
    }

    pub fn description(&self) -> String {
        use SearchCommand::*;
        use SearchDirection::*;

        let is_failing_search = self.stack.last().search_result.is_none();

        let is_wrapped_search = self
            .stack
            .iter()
            .skip_while(|item| item.search_result.is_some())
            .skip(1)
            .any(|item| match item.command {
                NextResult(_) => true,
                AddChar(_) => false,
            });

        let direction = self
            .stack
            .iter()
            .rev()
            .find_map(|item| match item.command {
                NextResult(dir) => Some(dir),
                AddChar(_) => None,
            })
            .expect(
                "If no others, \
                 first item in search stack should have direction",
            );

        let desc = match (is_failing_search, is_wrapped_search, direction) {
            (false, false, Forward) => "I-search",
            (true, false, Forward) => "Failing I-search",
            (false, true, Forward) => "Wrapped I-search",
            (true, true, Forward) => "Failing wrapped I-search",
            (false, false, Reverse) => "I-search backward",
            (true, false, Reverse) => "Failing I-search backward",
            (false, true, Reverse) => "Wrapped I-search backward",
            (true, true, Reverse) => "Failing wrapped I-search backward",
        };
        desc.to_string()
    }
}

impl<'a, T> ratatui::widgets::Widget for &'a SearchWindow<T> {
    fn render(self, area: Rect, buf: &mut ratatui::prelude::Buffer)
    where
        Self: Sized,
    {
        let (matching_part, non_matching_part) = self.get_search_string_parts();
        let line: Line = vec![
            Span::raw(matching_part),
            Span::styled(non_matching_part, Style::default().bg(Color::Red)),
        ]
        .into();

        let title = self.description();

        let widget = Paragraph::new(line)
            .block(Block::default().borders(Borders::ALL).title(title));

        widget.render(area, buf);
    }
}

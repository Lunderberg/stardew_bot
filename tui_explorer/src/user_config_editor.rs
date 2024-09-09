use itertools::{Either, Itertools};
use ratatui::style::Stylize as _;
use ratatui::{
    text::{Line, Span},
    widgets::{List, Widget},
};

use crate::extended_tui::WidgetWindow;
use crate::UserConfig;
use crate::{extensions::*, KeyBindingMatch};

pub struct UserConfigEditor {
    user_config: UserConfig,
    point: UserConfigPoint,
    is_modified: bool,
}

enum UserConfigPoint {
    ObjectExplorerSortTop(ListStringPoint),
    ObjectExplorerSortBottom(ListStringPoint),
}

struct ListStringPoint {
    list_index: usize,
    string_index: usize,
}

impl UserConfigEditor {
    pub(crate) fn new(user_config: UserConfig) -> Self {
        Self {
            user_config,
            point: UserConfigPoint::ObjectExplorerSortTop(ListStringPoint {
                list_index: 0,
                string_index: 0,
            }),
            is_modified: false,
        }
    }

    fn get_location(
        &mut self,
        char_offset: isize,
    ) -> (&mut Vec<String>, &mut usize, &mut usize, Option<usize>) {
        let (vec, list_index, string_index) = match &mut self.point {
            UserConfigPoint::ObjectExplorerSortTop(ListStringPoint {
                list_index,
                string_index,
            }) => (
                &mut self.user_config.object_explorer_sort_top,
                list_index,
                string_index,
            ),
            UserConfigPoint::ObjectExplorerSortBottom(ListStringPoint {
                list_index,
                string_index,
            }) => (
                &mut self.user_config.object_explorer_sort_bottom,
                list_index,
                string_index,
            ),
        };

        if vec.is_empty() {
            (vec, list_index, string_index, None)
        } else {
            let char_index = if char_offset < 0 {
                string_index.saturating_sub((-char_offset) as usize)
            } else {
                *string_index + (char_offset as usize)
            };

            let byte_index = vec[*list_index]
                .char_indices()
                .map(|(i, _)| i)
                .nth(char_index);

            (vec, list_index, string_index, byte_index)
        }
    }

    fn insert_char(&mut self, new_char: char) {
        self.is_modified = true;

        let (vec, list_index, string_index) = match &mut self.point {
            UserConfigPoint::ObjectExplorerSortTop(ListStringPoint {
                list_index,
                string_index,
            }) => (
                &mut self.user_config.object_explorer_sort_top,
                list_index,
                string_index,
            ),
            UserConfigPoint::ObjectExplorerSortBottom(ListStringPoint {
                list_index,
                string_index,
            }) => (
                &mut self.user_config.object_explorer_sort_bottom,
                list_index,
                string_index,
            ),
        };

        if vec.is_empty() {
            vec.push(new_char.into());
            *string_index = 1;
        } else {
            let value = &mut vec[*list_index];

            let mut num_chars = 0;
            let insert_loc = value
                .char_indices()
                .map(|(i, _)| i)
                .inspect(|_| {
                    num_chars += 1;
                })
                .nth(*string_index);
            if let Some(byte_index) = insert_loc {
                value.insert(byte_index, new_char);
                *string_index += 1;
            } else {
                value.push(new_char);
                *string_index = num_chars + 1;
            }
        }
    }

    fn new_line(&mut self) {
        self.is_modified = true;

        let (vec, list_index, string_index, opt_byte_index) =
            self.get_location(0);

        if vec.is_empty() {
            vec.push("".into());
            *list_index = 0;
        } else if let Some(byte_index) = opt_byte_index {
            let next_line = vec[*list_index].split_off(byte_index);
            vec.insert(*list_index + 1, next_line);
            *list_index += 1;
            *string_index = 0;
        } else {
            vec.insert(*list_index + 1, "".to_string());
            *list_index += 1;
            *string_index = 0;
        }
    }

    fn go_to_start_of_line(&mut self) {
        match &mut self.point {
            UserConfigPoint::ObjectExplorerSortTop(ListStringPoint {
                string_index,
                ..
            })
            | UserConfigPoint::ObjectExplorerSortBottom(ListStringPoint {
                string_index,
                ..
            }) => {
                *string_index = 0;
            }
        }
    }

    fn go_to_end_of_line(&mut self) {
        match &mut self.point {
            UserConfigPoint::ObjectExplorerSortTop(ListStringPoint {
                list_index,
                string_index,
            }) => {
                *string_index = self.user_config.object_explorer_sort_top
                    [*list_index]
                    .len();
            }
            UserConfigPoint::ObjectExplorerSortBottom(ListStringPoint {
                list_index,
                string_index,
            }) => {
                *string_index = self.user_config.object_explorer_sort_bottom
                    [*list_index]
                    .len();
            }
        }
    }

    fn scroll_down(&mut self) {
        self.point = match self.point {
            UserConfigPoint::ObjectExplorerSortTop(ListStringPoint {
                list_index,
                string_index,
            }) if list_index
                == self
                    .user_config
                    .object_explorer_sort_top
                    .len()
                    .saturating_sub(1) =>
            {
                UserConfigPoint::ObjectExplorerSortBottom(ListStringPoint {
                    list_index: 0,
                    string_index,
                })
            }
            UserConfigPoint::ObjectExplorerSortTop(ListStringPoint {
                list_index,
                string_index,
            }) => UserConfigPoint::ObjectExplorerSortTop(ListStringPoint {
                list_index: list_index + 1,
                string_index,
            }),
            UserConfigPoint::ObjectExplorerSortBottom(ListStringPoint {
                list_index,
                string_index,
            }) => UserConfigPoint::ObjectExplorerSortBottom(ListStringPoint {
                list_index: (list_index + 1).min(
                    self.user_config
                        .object_explorer_sort_bottom
                        .len()
                        .saturating_sub(1),
                ),
                string_index,
            }),
        };
    }

    fn scroll_up(&mut self) {
        self.point = match self.point {
            UserConfigPoint::ObjectExplorerSortTop(ListStringPoint {
                list_index,
                string_index,
            }) => UserConfigPoint::ObjectExplorerSortTop(ListStringPoint {
                list_index: list_index.saturating_sub(1),
                string_index,
            }),
            UserConfigPoint::ObjectExplorerSortBottom(ListStringPoint {
                list_index,
                string_index,
            }) if list_index == 0 => {
                UserConfigPoint::ObjectExplorerSortTop(ListStringPoint {
                    list_index: self
                        .user_config
                        .object_explorer_sort_top
                        .len()
                        .saturating_sub(1),
                    string_index,
                })
            }
            UserConfigPoint::ObjectExplorerSortBottom(ListStringPoint {
                list_index,
                string_index,
            }) => UserConfigPoint::ObjectExplorerSortBottom(ListStringPoint {
                list_index: list_index - 1,
                string_index,
            }),
        };
    }

    fn scroll_left(&mut self) {
        match &mut self.point {
            UserConfigPoint::ObjectExplorerSortTop(ListStringPoint {
                string_index,
                ..
            })
            | UserConfigPoint::ObjectExplorerSortBottom(ListStringPoint {
                string_index,
                ..
            }) => {
                *string_index = string_index.saturating_sub(1);
            }
        }
    }

    fn scroll_right(&mut self) {
        match &mut self.point {
            UserConfigPoint::ObjectExplorerSortTop(ListStringPoint {
                list_index,
                string_index,
            }) => {
                *string_index = (*string_index + 1).min(
                    self.user_config.object_explorer_sort_top[*list_index]
                        .len(),
                );
            }
            UserConfigPoint::ObjectExplorerSortBottom(ListStringPoint {
                list_index,
                string_index,
            }) => {
                *string_index = (*string_index + 1).min(
                    self.user_config.object_explorer_sort_bottom[*list_index]
                        .len(),
                );
            }
        }
    }

    fn delete_character(&mut self) {
        self.is_modified = true;

        let (vec, list_index, _, opt_byte_index) = self.get_location(0);

        if vec.is_empty() {
            return;
        }

        if let Some(byte_index) = opt_byte_index {
            vec[*list_index].remove(byte_index);
        } else {
            // End of the line, remove next line and concatenate.
            let next_line = vec.remove(*list_index + 1);
            vec[*list_index].push_str(&next_line);
        }
    }

    fn backspace_character(&mut self) {
        self.is_modified = true;

        let (vec, list_index, string_index, opt_byte_index) =
            self.get_location(-1);

        if vec.is_empty() {
            return;
        }

        if *string_index == 0 {
            if *list_index > 0 {
                // Beginning of the line, concatenate into previous line
                let this_line = vec.remove(*list_index);
                *list_index -= 1;
                vec[*list_index].push_str(&this_line);
                *string_index = vec[*list_index].len();
            }
        } else if let Some(byte_index) = opt_byte_index {
            vec[*list_index].remove(byte_index);
            *string_index -= 1;
        }
    }

    fn kill_to_end_of_line(&mut self) {
        self.is_modified = true;

        let (vec, list_index, _, opt_byte_index) = self.get_location(0);

        if let Some(byte_index) = opt_byte_index {
            // Middle of the line, remove everything after point
            vec[*list_index].truncate(byte_index);
        } else if *list_index + 1 < vec.len() {
            // End of the line, remove next line and concatenate.
            let next_line = vec.remove(*list_index + 1);
            vec[*list_index].push_str(&next_line);
        }
    }

    fn iter_lines_list<'a>(
        &'a self,
        desc: &'a str,
        name: &'a str,
        values: &'a [String],
        point: Option<&'a ListStringPoint>,
    ) -> impl Iterator<Item = Line<'a>> + 'a {
        if values.is_empty() {
            let empty_list = if point.is_some() {
                [format!("{name} = [").into(), Span::raw("]").reversed()]
                    .into_iter()
                    .collect()
            } else {
                Line::raw(format!("{name} = []"))
            };
            return Either::Left([desc.into(), empty_list].into_iter());
        }

        let iter = [desc.into(), format!("{name} = [").into()]
            .into_iter()
            .chain(values.iter().enumerate().map(
                move |(i_line, value)| -> Line {
                    let is_selected_line =
                        point.map(|p| p.list_index == i_line).unwrap_or(false);
                    let indent = "    ";
                    if !is_selected_line {
                        return format!("{indent}\"{value}\",").into();
                    }

                    let selected_char = point.unwrap().string_index;

                    let iter_chars = value
                        .chars()
                        .extend('"')
                        .with_position()
                        .enumerate()
                        .map(|(i_char, (position, c))| {
                            let is_selected_char = match position {
                                itertools::Position::Last
                                | itertools::Position::Only => {
                                    selected_char >= i_char
                                }
                                _ => selected_char == i_char,
                            };

                            let span = Span::raw(c.to_string());
                            if is_selected_char {
                                span.reversed()
                            } else {
                                span
                            }
                        });

                    std::iter::once(Span::raw(indent))
                        .extend("\"")
                        .chain(iter_chars)
                        .extend(",")
                        .collect()
                },
            ))
            .chain(std::iter::once("]".into()));
        Either::Right(iter)
    }

    fn iter_lines(&self) -> impl Iterator<Item = Line> + '_ {
        std::iter::empty::<Line>()
            .chain(self.iter_lines_list(
                "// Items to sort to the top of ObjectExplorer",
                "object_explorer_sort_top",
                &self.user_config.object_explorer_sort_top,
                match &self.point {
                    UserConfigPoint::ObjectExplorerSortTop(p) => Some(p),
                    _ => None,
                },
            ))
            .extend("")
            .chain(self.iter_lines_list(
                "// Items to sort to the bottom of ObjectExplorer",
                "object_explorer_sort_botttom",
                &self.user_config.object_explorer_sort_bottom,
                match &self.point {
                    UserConfigPoint::ObjectExplorerSortBottom(p) => Some(p),
                    _ => None,
                },
            ))
    }
}

impl WidgetWindow for UserConfigEditor {
    fn title(&self) -> std::borrow::Cow<str> {
        if self.is_modified {
            "User Config Editor (unsaved modifications)".into()
        } else {
            "User Config Editor".into()
        }
    }

    fn apply_key_binding<'a>(
        &'a mut self,
        keystrokes: &'a crate::KeySequence,
        _globals: crate::extended_tui::WidgetGlobals<'a>,
        side_effects: &'a mut crate::extended_tui::WidgetSideEffects,
    ) -> KeyBindingMatch {
        KeyBindingMatch::Mismatch
            .or_try_binding("C-x C-s", keystrokes, || {
                self.user_config.save_to_default_location().unwrap_or_else(
                    |err| side_effects.add_log(format!("Error: {err}")),
                );
                self.is_modified = false;
            })
            .or_try_bindings(["C-n", "<down>"], keystrokes, || {
                self.scroll_down()
            })
            .or_try_bindings(["C-p", "<up>"], keystrokes, || self.scroll_up())
            .or_try_bindings(["C-f", "<right>"], keystrokes, || {
                self.scroll_right()
            })
            .or_try_bindings(["C-b", "<left>"], keystrokes, || {
                self.scroll_left()
            })
            .or_try_bindings(["C-b", "<left>"], keystrokes, || {
                self.scroll_left()
            })
            .or_try_bindings(["C-a", "<home>"], keystrokes, || {
                self.go_to_start_of_line()
            })
            .or_try_bindings(["C-e", "<end>"], keystrokes, || {
                self.go_to_end_of_line()
            })
            .or_try_binding("C-d", keystrokes, || self.delete_character())
            .or_try_binding("<backspace>", keystrokes, || {
                self.backspace_character()
            })
            .or_try_bindings(["C-j", "<enter>"], keystrokes, || self.new_line())
            .or_try_binding("C-k", keystrokes, || self.kill_to_end_of_line())
            .or_else(|| {
                if let Some(c) = keystrokes.as_char() {
                    self.insert_char(c);
                    KeyBindingMatch::Full
                } else {
                    KeyBindingMatch::Mismatch
                }
            })
    }

    fn draw<'a>(
        &'a mut self,
        _globals: crate::extended_tui::WidgetGlobals<'a>,
        area: ratatui::layout::Rect,
        buf: &mut ratatui::prelude::Buffer,
    ) {
        let widget: List = self.iter_lines().collect();
        widget.render(area, buf)
    }
}

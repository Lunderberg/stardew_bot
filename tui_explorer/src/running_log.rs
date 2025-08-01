use std::collections::VecDeque;

use chrono::prelude::*;
use ratatui::widgets::StatefulWidget;
use ratatui::{
    layout::Rect,
    style::{Color, Modifier, Style},
    text::Line,
    widgets::{List, ListState, Widget},
};
use regex::Regex;

use memory_reader::Pointer;
use tui_utils::{
    extensions::HighlightLine as _,
    inputs::{KeyBindingMatch, KeySequence},
    widgets::{ScrollableState as _, SearchDirection, SearchWindow},
    LogMessage, TuiGlobals, WidgetSideEffects, WidgetWindow,
};

use crate::{ChangeAddress, Error};

pub struct RunningLog {
    max_elements: usize,
    state: ListState,
    items: VecDeque<(DateTime<Local>, String)>,
    search: Option<SearchWindow<ListState>>,
    prev_draw_height: usize,
}

impl RunningLog {
    pub(crate) fn new(max_elements: usize) -> Self {
        Self {
            max_elements,
            state: ListState::default(),
            items: VecDeque::new(),
            search: None,
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

    fn start_search(&mut self, direction: SearchDirection) {
        self.search = Some(SearchWindow::new(direction, self.state.clone()));
    }

    fn cancel_search(&mut self) {
        if let Some(search) = self.search.take() {
            self.state = search.pre_search_state;
        }
    }

    fn finalize_search(&mut self) {
        self.search = None;
    }

    fn highlighted_address(&self) -> Option<Pointer> {
        let line_num = self.state.selected()?;

        let (_, line) = self
            .items
            .get(self.items.len() - line_num - 1)
            .expect("Invalid line num selected");

        Regex::new("0x[0-9A-Fa-f]+")
            .expect("Invalid regex")
            .find(line)
            .map(|addr_match| addr_match.as_str())
            .map(|addr_str| {
                usize::from_str_radix(&addr_str[2..], 16)
                    .expect("Conversion should succeed from prev regex")
            })
            .map(Pointer::new)
    }

    fn jump_to_highlighted_address(
        &mut self,
        side_effects: &mut WidgetSideEffects,
    ) {
        if let Some(address) = self.highlighted_address() {
            side_effects.broadcast(ChangeAddress(address));
        } else {
            side_effects.add_log("Line doesn't contain an address");
        }
    }

    fn get_row_generator<'a>(
        items: &'a VecDeque<(DateTime<Local>, String)>,
    ) -> impl Fn(usize) -> Vec<String> + 'a {
        move |row: usize| -> Vec<String> {
            let (timestamp, entry) = &items[items.len() - row - 1];
            vec![format!("{} {}", timestamp.format("%H:%M:%S"), entry)]
        }
    }
}

impl Widget for &mut RunningLog {
    fn render(self, area: Rect, buf: &mut ratatui::prelude::Buffer)
    where
        Self: Sized,
    {
        self.prev_draw_height = area.height as usize;

        let row_generator = RunningLog::get_row_generator(&self.items);

        let items = (0..self.items.len())
            .map(|i| row_generator(i).pop().unwrap())
            .map(|line| -> Line { line.into() })
            .map(|line| {
                line.style_regex(
                    "0x[0-9a-fA-F]+",
                    Style::default().fg(Color::LightRed),
                )
            })
            .map(|line| {
                if let Some(search) = self.search.as_ref() {
                    search.highlight_search_matches(line)
                } else {
                    line
                }
            });

        let search_area_height = if self.search.is_some() {
            area.height.min(3)
        } else {
            0
        };

        let search_area = Rect::new(
            area.x,
            area.bottom() - search_area_height,
            area.width,
            search_area_height,
        );

        let log_area = Rect::new(
            area.x,
            area.y,
            area.width,
            area.height - search_area_height,
        );

        if let Some(search) = self.search.as_ref() {
            search.render(search_area, buf);
        }

        let running_log = List::new(items)
            .highlight_style(Style::default().add_modifier(Modifier::REVERSED))
            .highlight_symbol(">> ");

        StatefulWidget::render(running_log, log_area, buf, &mut self.state);
    }
}

impl WidgetWindow<Error> for RunningLog {
    fn title(&self) -> std::borrow::Cow<str> {
        "Log".into()
    }

    fn apply_key_binding(
        &mut self,
        keystrokes: &KeySequence,
        _globals: &TuiGlobals,
        side_effects: &mut WidgetSideEffects,
    ) -> KeyBindingMatch {
        KeyBindingMatch::Mismatch
            .or_else(|| {
                self.state
                    .apply_key_binding(
                        keystrokes,
                        self.items.len(),
                        self.prev_draw_height - 3,
                    )
                    .then(|| self.finalize_search())
                    .or_else(|| {
                        if let Some(search) = self.search.as_mut() {
                            search
                                .apply_key_binding(
                                    keystrokes,
                                    self.items.len(),
                                    Self::get_row_generator(&self.items),
                                )
                                .then(|| {
                                    self.state.select(Some(
                                        search.recommended_row_selection(),
                                    ))
                                })
                                .or_try_binding("C-g", keystrokes, || {
                                    self.cancel_search()
                                })
                        } else {
                            KeyBindingMatch::Mismatch
                        }
                    })
            })
            .or_try_binding("C-s", keystrokes, || {
                self.start_search(SearchDirection::Forward)
            })
            .or_try_binding("C-r", keystrokes, || {
                self.start_search(SearchDirection::Reverse)
            })
            .or_try_binding("<enter>", keystrokes, || {
                self.jump_to_highlighted_address(side_effects)
            })
    }

    fn draw<'a>(
        &'a mut self,
        _: &'a TuiGlobals,
        area: ratatui::layout::Rect,
        buf: &mut ratatui::prelude::Buffer,
    ) {
        self.render(area, buf)
    }

    fn apply_side_effects<'a>(
        &'a mut self,
        _globals: &'a TuiGlobals,
        side_effects: &'a mut WidgetSideEffects,
    ) -> Result<(), Error> {
        side_effects.into_iter::<LogMessage>().for_each(|log| {
            self.add_log(log.0);
        });
        Ok(())
    }
}

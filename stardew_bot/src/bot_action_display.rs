use ratatui::{
    style::{Modifier, Style},
    text::Line,
    widgets::{List, ListState, StatefulWidget, Widget},
};
use tui_utils::{
    extensions::{SplitRect as _, WidgetWithScrollbar as _},
    inputs::{KeyBindingMatch, KeySequence},
    widgets::{ScrollableState as _, SearchDirection, SearchWindow},
    TuiGlobals, WidgetSideEffects, WidgetWindow,
};

use crate::{BotLogic, Error};

pub struct BotActionDisplay {
    state: ListState,
    search: Option<SearchWindow<ListState>>,
    prev_draw_height: usize,
}

impl BotActionDisplay {
    pub(crate) fn new() -> Self {
        Self {
            state: ListState::default(),
            search: None,
            prev_draw_height: 1,
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

    fn get_row_generator<'a>(
        bot_logic: &'a BotLogic,
    ) -> impl Fn(usize) -> Vec<String> + 'a {
        move |row: usize| -> Vec<String> {
            bot_logic
                .iter_recent_actions()
                .rev()
                .skip(row)
                .next()
                .map(|action| format!("{action}"))
                .into_iter()
                .collect()
        }
    }
}

impl WidgetWindow<Error> for BotActionDisplay {
    fn title(&self) -> std::borrow::Cow<str> {
        "Bot Actions".into()
    }

    fn apply_key_binding(
        &mut self,
        keystrokes: &KeySequence,
        globals: &TuiGlobals,
        _side_effects: &mut WidgetSideEffects,
    ) -> KeyBindingMatch {
        let bot_logic = globals
            .get::<BotLogic>()
            .expect("Generated/updated in top-level GUI update");

        KeyBindingMatch::Mismatch
            .or_else(|| {
                self.state
                    .apply_key_binding(
                        keystrokes,
                        bot_logic.iter_recent_actions().len(),
                        self.prev_draw_height - 3,
                    )
                    .then(|| self.finalize_search())
                    .or_else(|| {
                        if let Some(search) = self.search.as_mut() {
                            search
                                .apply_key_binding(
                                    keystrokes,
                                    bot_logic.iter_recent_actions().len(),
                                    Self::get_row_generator(bot_logic),
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
    }

    fn draw<'a>(
        &'a mut self,
        globals: &'a tui_utils::TuiGlobals,
        area: ratatui::layout::Rect,
        buf: &mut ratatui::prelude::Buffer,
    ) {
        self.prev_draw_height = area.height as usize;

        let bot_logic = globals
            .get::<BotLogic>()
            .expect("Generated/updated in top-level GUI update");

        let search_area_height = if self.search.is_some() {
            area.height.min(3)
        } else {
            0
        };

        let (main_area, search_area) =
            area.split_from_bottom(search_area_height);

        if let Some(search) = self.search.as_ref() {
            search.render(search_area, buf);
        }

        let items = bot_logic
            .iter_recent_actions()
            .rev()
            .map(|verbose_action| format!("{verbose_action}"))
            .map(Line::raw)
            .map(|line| {
                if let Some(search) = self.search.as_ref() {
                    search.highlight_search_matches(line)
                } else {
                    line
                }
            });

        let goal_list = List::new(items)
            .highlight_style(Style::default().add_modifier(Modifier::REVERSED))
            .highlight_symbol(">> ")
            .with_scrollbar(bot_logic.iter_recent_actions().len());

        StatefulWidget::render(goal_list, main_area, buf, &mut self.state);
    }
}

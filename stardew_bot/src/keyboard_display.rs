use ratatui::widgets::{List, Widget};
use tui_utils::WidgetWindow;

use crate::{Error, GameState};

pub struct KeyboardDisplay;

impl WidgetWindow<Error> for KeyboardDisplay {
    fn title(&self) -> std::borrow::Cow<str> {
        "Keyboard".into()
    }

    fn draw<'a>(
        &'a mut self,
        globals: &'a tui_utils::TuiGlobals,
        area: ratatui::layout::Rect,
        buf: &mut ratatui::prelude::Buffer,
    ) {
        let game_state = globals
            .get::<GameState>()
            .expect("Generated/updated in top-level GUI update");

        let key_list = List::new(
            game_state.keyboard.keys.iter().map(|key| format!("{key}")),
        );

        Widget::render(key_list, area, buf);
    }
}

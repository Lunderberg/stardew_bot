use ratatui::widgets::{List, Widget};
use tui_utils::WidgetWindow;

use crate::{Error, GameState};

pub struct InputDisplay;

impl WidgetWindow<Error> for InputDisplay {
    fn title(&self) -> std::borrow::Cow<str> {
        "Mouse/Keyboard".into()
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

        let mouse_pos =
            format!("Mouse: {}", game_state.inputs.mouse_pixel_location);
        let mouse_buttons = format!("M: {}", game_state.inputs.mouse_buttons);

        let iter_keys = game_state
            .inputs
            .keys_pressed
            .iter()
            .map(|key| format!("{key}"));

        let iter_rows = [mouse_pos, mouse_buttons].into_iter().chain(iter_keys);

        let input_list = List::new(iter_rows);

        Widget::render(input_list, area, buf);
    }
}

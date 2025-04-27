use ratatui::widgets::{List, Widget};
use tui_utils::WidgetWindow;

use crate::{BotLogic, Error};

pub struct BotGoalDisplay;

impl WidgetWindow<Error> for BotGoalDisplay {
    fn title(&self) -> std::borrow::Cow<str> {
        "Bot Goals".into()
    }

    fn draw<'a>(
        &'a mut self,
        globals: &'a tui_utils::TuiGlobals,
        area: ratatui::layout::Rect,
        buf: &mut ratatui::prelude::Buffer,
    ) {
        let bot_logic = globals
            .get::<BotLogic>()
            .expect("Generated/updated in top-level GUI update");

        let goal_list = List::new(
            bot_logic.iter_goals().rev().map(|goal| goal.description()),
        );

        Widget::render(goal_list, area, buf);
    }
}

use ratatui::{
    layout::Constraint,
    style::{Color, Style},
    text::Text,
    widgets::{Row, Table, Widget},
};
use tui_utils::WidgetWindow;

use bot_logic::predict_daily_luck;

use crate::{Error, GameState};

pub struct PredictedLuckDisplay;

impl WidgetWindow<Error> for PredictedLuckDisplay {
    fn title(&self) -> std::borrow::Cow<str> {
        "Predicted Daily Luck".into()
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

        let iter_rows = (0..20).map(|additional_steps| {
            let luck = predict_daily_luck(game_state, additional_steps);
            let style = if luck < -80 {
                Style::default().fg(Color::Red)
            } else if luck > 80 {
                Style::default().fg(Color::Green)
            } else {
                Style::default()
            };
            Row::new([
                Text::raw(format!("{additional_steps}")),
                Text::styled(format!("{luck}"), style),
            ])
        });

        let table = Table::new(
            iter_rows,
            [Constraint::Min(15), Constraint::Percentage(100)],
        );

        Widget::render(table, area, buf);
    }
}

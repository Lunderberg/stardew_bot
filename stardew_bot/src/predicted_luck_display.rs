use ratatui::{
    layout::Constraint,
    style::{Color, Modifier, Style},
    text::Text,
    widgets::{Row, Table, Widget},
};
use tui_utils::WidgetWindow;

use bot_logic::predictors::StepCountPredictor;

use crate::{Error, GameState};

pub struct PredictedLuckDisplay;

impl WidgetWindow<Error> for PredictedLuckDisplay {
    fn title(&self) -> std::borrow::Cow<str> {
        "Step Count RNG".into()
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

        let friendships = &game_state.player.friendships;

        let predictor = StepCountPredictor::new(game_state);
        let iter_rows = (0..20).map(|additional_steps| {
            let prediction = predictor.predict(additional_steps);

            let luck_column = {
                let luck = prediction.daily_luck;
                let style = if luck < -80 {
                    Style::default().fg(Color::Red)
                } else if luck > 80 {
                    Style::default().fg(Color::Green)
                } else {
                    Style::default()
                };
                Text::styled(format!("{luck}"), style)
            };

            let friendship_column = if let Some(friendship) =
                friendships.get(prediction.friendship_index)
            {
                let name = &friendship.name;
                let required_hearts = prediction.min_hearts_for_gift;
                let current_hearts = friendship.points / 250;

                let style = if required_hearts <= current_hearts {
                    Style::default().fg(Color::Green)
                } else {
                    Style::default()
                };

                Text::styled(format!("{name} ({required_hearts} ♥)"), style)
            } else {
                Text::raw("")
            };

            Row::new([
                Text::raw(format!("{additional_steps}")),
                luck_column,
                friendship_column,
            ])
        });

        let table = Table::new(
            iter_rows,
            [
                Constraint::Min(15),
                Constraint::Min(5),
                Constraint::Percentage(100),
            ],
        )
        .header(
            Row::new(["Steps", "Luck", "Gifter"]).style(
                Style::default()
                    .bg(Color::Blue)
                    .fg(Color::LightCyan)
                    .add_modifier(Modifier::BOLD),
            ),
        );

        Widget::render(table, area, buf);
    }
}

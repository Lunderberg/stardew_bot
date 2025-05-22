use ratatui::{
    layout::Constraint,
    style::{Color, Style},
    text::Text,
    widgets::{Row, Table, Widget},
};
use tui_utils::WidgetWindow;

use crate::{Error, GameState};

pub struct RngDisplay;

impl WidgetWindow<Error> for RngDisplay {
    fn title(&self) -> std::borrow::Cow<str> {
        "Global RNG".into()
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

        let average_calls = game_state
            .rng_state
            .iter_prev_calls()
            .fold(Some((0, 0)), |state, calls_this_frame: Option<usize>| {
                let (calls, ticks) = state?;
                Some((calls + calls_this_frame?, ticks + 1))
            })
            .map(|(num_calls, num_frames)| {
                (num_calls as f32) / (num_frames as f32)
            })
            .unwrap_or(f32::NAN);

        let iter_rows = [
            Row::new([
                "Game tick:".into(),
                format!("{}", game_state.global_game_state.game_tick),
            ]),
            Row::new([
                "Game mode tick:".into(),
                format!("{}", game_state.global_game_state.game_mode_tick),
            ]),
            Row::new(["Avg calls/tick:".into(), format!("{average_calls:.1}")]),
        ]
        .into_iter()
        .chain([Row::new([Text::default()])])
        .chain(game_state.rng_state.iter_float().take(20).map(|r| {
            let style = if r < 0.1 {
                Style::default().fg(Color::Red)
            } else if r > 0.9 {
                Style::default().fg(Color::Green)
            } else {
                Style::default()
            };
            let text = Text::styled(format!("{r:.3}"), style);
            Row::new([Text::default(), text])
        }));
        let table = Table::new(
            iter_rows,
            [Constraint::Min(15), Constraint::Percentage(100)],
        );

        Widget::render(table, area, buf);
    }
}

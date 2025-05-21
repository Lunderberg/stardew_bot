use ratatui::{
    layout::Constraint,
    style::{Color, Style},
    text::Text,
    widgets::{Row, Table, Widget},
};
use tui_utils::{TuiGlobals, WidgetSideEffects, WidgetWindow};

use crate::{game_state::RngState, Error, GameState};

pub struct RngDisplay {
    prev_state: RngState,
    current_state: RngState,
}

impl RngDisplay {
    pub fn new() -> Self {
        Self {
            prev_state: RngState::from_seed(0),
            current_state: RngState::from_seed(0),
        }
    }
}

impl WidgetWindow<Error> for RngDisplay {
    fn title(&self) -> std::borrow::Cow<str> {
        "Global RNG".into()
    }

    fn periodic_update<'a>(
        &mut self,
        globals: &'a TuiGlobals,
        _side_effects: &'a mut WidgetSideEffects,
    ) -> Result<(), Error> {
        let game_state = globals
            .get::<GameState>()
            .expect("Generated/updated in top-level GUI update");

        if game_state.rng_state != self.current_state {
            self.prev_state = self.current_state.clone();
            self.current_state = game_state.rng_state.clone();
        }

        Ok(())
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

        let mut lookahead = self.current_state.clone();

        let iter_rows = [
            Row::new([
                "Game tick:".into(),
                format!("{}", game_state.global_game_state.game_tick),
            ]),
            Row::new([
                "Current index:".into(),
                format!("{}", self.current_state.inext),
            ]),
            Row::new([
                "Prev index:".into(),
                format!("{}", self.prev_state.inext),
            ]),
            Row::new([
                "Delta index:".into(),
                format!(
                    "{}",
                    (self.current_state.inext - self.prev_state.inext)
                        .rem_euclid(56)
                ),
            ]),
        ]
        .into_iter()
        .chain([Row::new([Text::default()])])
        .chain((0..20).map(|_| lookahead.rand_float()).map(|r| {
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

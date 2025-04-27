use ratatui::{
    layout::Constraint,
    widgets::{Row, StatefulWidget, Table, TableState},
};
use tui_utils::WidgetWindow;

use crate::{Error, GameState};

pub struct PlayerStats {
    table_state: TableState,
}

impl PlayerStats {
    pub fn new() -> Self {
        Self {
            table_state: TableState::new(),
        }
    }
}

impl WidgetWindow<Error> for PlayerStats {
    fn title(&self) -> std::borrow::Cow<str> {
        "Player Stats".into()
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
        let player_state = &game_state.player;
        let daily_state = &game_state.daily;

        let table = Table::new(
            [
                Row::new([
                    "Moving".into(),
                    player_state
                        .movement
                        .map(|dir| format!("{dir}"))
                        .unwrap_or("".into()),
                ]),
                Row::default(),
                Row::new([
                    "Farming XP".into(),
                    format!("{}", player_state.skills.farming_xp),
                ]),
                Row::new([
                    "Mining XP".into(),
                    format!("{}", player_state.skills.mining_xp),
                ]),
                Row::new([
                    "Foraging XP".into(),
                    format!("{}", player_state.skills.foraging_xp),
                ]),
                Row::new([
                    "Fishing XP".into(),
                    format!("{}", player_state.skills.fishing_xp),
                ]),
                Row::new([
                    "Combat XP".into(),
                    format!("{}", player_state.skills.combat_xp),
                ]),
                Row::default(),
                Row::new([
                    "Daily luck".into(),
                    format!("{}", daily_state.daily_luck),
                ]),
                Row::new([
                    "Weather forecast".into(),
                    daily_state.tomorrow_weather.clone(),
                ]),
            ],
            [Constraint::Min(20), Constraint::Percentage(100)],
        );

        StatefulWidget::render(table, area, buf, &mut self.table_state);
    }
}

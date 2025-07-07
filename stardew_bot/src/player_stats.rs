use ratatui::{
    layout::Constraint,
    widgets::{Row, StatefulWidget, Table, TableState},
};
use tui_utils::WidgetWindow;

use crate::{
    game_state::{ItemCategory, ItemId},
    Error, GameState,
};

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

        let fungible_gold = player_state.current_money
            + player_state
                .inventory
                .iter_items()
                .filter(|item| -> bool {
                    matches!(item.category, Some(ItemCategory::Fish))
                        || item.id == ItemId::CLAY
                })
                .map(|item| item.stack_price())
                .sum::<i32>();

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
                Row::new(["Fungible GP".into(), format!("{fungible_gold}")]),
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

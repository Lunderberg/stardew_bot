use dotnet_debugger::{SymbolicGraph, VMResults, ValueToken};
use ratatui::{
    layout::Constraint,
    text::Text,
    widgets::{Row, StatefulWidget, Table, TableState},
};
use tui_utils::WidgetWindow;

use crate::Error;

pub struct PlayerStats {
    table_state: TableState,

    farming_xp: ValueToken,
    mining_xp: ValueToken,
    foraging_xp: ValueToken,
    fishing_xp: ValueToken,
    combat_xp: ValueToken,

    daily_luck: ValueToken,
    tomorrow_weather: ValueToken,
}

impl PlayerStats {
    pub fn new(per_frame_reader: &mut SymbolicGraph) -> Result<Self, Error> {
        let mut register = |value: &str| -> Result<ValueToken, Error> {
            let expr = per_frame_reader.parse(value)?;
            let token = per_frame_reader.mark_output(expr);
            Ok(token)
        };

        let player = "StardewValley.Game1._player";
        let skill_list = format!("{player}.experiencePoints.elements._items");
        let farming_xp = register(&format!("{skill_list}[0].value"))?;
        let fishing_xp = register(&format!("{skill_list}[1].value"))?;
        let foraging_xp = register(&format!("{skill_list}[2].value"))?;
        let mining_xp = register(&format!("{skill_list}[3].value"))?;
        let combat_xp = register(&format!("{skill_list}[4].value"))?;

        let daily_luck = register(&format!(
            "{player}.teamRoot.value.sharedDailyLuck.value"
        ))?;
        let tomorrow_weather =
            register("StardewValley.Game1.weatherForTomorrow")?;
        Ok(Self {
            table_state: TableState::new(),
            farming_xp,
            mining_xp,
            foraging_xp,
            fishing_xp,
            combat_xp,
            daily_luck,
            tomorrow_weather,
        })
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
        let values = globals
            .get::<VMResults>()
            .expect("Generated for each frame");

        let get_value = |token: ValueToken| match &values[token] {
            Some(value) => Text::raw(format!("{value}")),
            None => Text::raw(""),
        };

        let tomorrow_weather = match values[self.tomorrow_weather]
            .as_ref()
            .and_then(|val| val.as_prim())
        {
            Some(ptr) => match ptr.read_string_ptr(globals.reader()) {
                Ok(weather) => Text::raw(weather),
                Err(err) => Text::raw(format!("{err}")),
            },
            None => Text::raw(""),
        };

        let table = Table::new(
            [
                Row::new(["Farming XP".into(), get_value(self.farming_xp)]),
                Row::new(["Mining XP".into(), get_value(self.mining_xp)]),
                Row::new(["Foraging XP".into(), get_value(self.foraging_xp)]),
                Row::new(["Fishing XP".into(), get_value(self.fishing_xp)]),
                Row::new(["Combat XP".into(), get_value(self.combat_xp)]),
                Row::default(),
                Row::new(["Daily luck".into(), get_value(self.daily_luck)]),
                Row::new(["Weather forecast".into(), tomorrow_weather]),
            ],
            [Constraint::Min(20), Constraint::Percentage(100)],
        );

        StatefulWidget::render(table, area, buf, &mut self.table_state);
    }
}

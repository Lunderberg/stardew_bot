use dotnet_debugger::{CachedReader, PhysicalAccessChain};
use ratatui::{
    layout::Constraint,
    text::Text,
    widgets::{Row, Table, TableState},
};
use tui_utils::WidgetWindow;

use crate::Error;

pub struct FishingUI {
    table_state: TableState,
    time_until_fishing_bite: PhysicalAccessChain,
}

impl FishingUI {
    pub fn new(reader: CachedReader<'_>) -> Result<Self, Error> {
        let time_until_fishing_bite = reader.parse_access_chain(
            "StardewValley.Game1​._player​\
             .netItems​.value​.Items​.array​.value​.elements​._items\
             [5]​.value​.as<StardewValley.Tools.FishingRod>()\
             .timeUntilFishingBite",
        )?;
        Ok(Self {
            table_state: TableState::new(),
            time_until_fishing_bite,
        })
    }
}

impl WidgetWindow for FishingUI {
    fn title(&self) -> std::borrow::Cow<str> {
        "Fishing".into()
    }

    fn draw<'a>(
        &'a mut self,
        globals: &'a tui_utils::TuiGlobals,
        area: ratatui::layout::Rect,
        buf: &mut ratatui::prelude::Buffer,
    ) {
        let reader = globals.cached_reader();
        let value = match self.time_until_fishing_bite.read(reader) {
            Ok(Some(value)) => format!("{value}"),
            Ok(None) => "".to_string(),
            Err(err) => format!("Error: {err}"),
        };
        let table = Table::new(
            [Row::new([Text::raw("Time until fish:"), Text::raw(value)])],
            [Constraint::Min(15), Constraint::Percentage(100)],
        );

        ratatui::widgets::StatefulWidget::render(
            table,
            area,
            buf,
            &mut self.table_state,
        );
    }
}

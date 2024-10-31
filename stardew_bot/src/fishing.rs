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

    /// The time at which a fish will bite
    time_until_fishing_bite: PhysicalAccessChain,

    /// The time spent waiting for a fish to bite.
    fishing_bite_accumulator: PhysicalAccessChain,

    /// The time at which a fish will no longer be hooked.
    time_until_fishing_nibble_done: PhysicalAccessChain,

    /// The time since a fish was hooked, incremented for each frame.
    /// When this value reaches `time_until_fishing_nibble_done`, the
    /// fish can no longer be caught.
    fishing_nibble_accumulator: PhysicalAccessChain,

    /// The location of the fish within the fishing minigame.
    fish_position: PhysicalAccessChain,

    /// The location of the fish within the fishing minigame.
    bar_position: PhysicalAccessChain,

    /// The height of the fish within the fishing minigame.
    bar_height: PhysicalAccessChain,
}

impl FishingUI {
    pub fn new(reader: CachedReader<'_>) -> Result<Self, Error> {
        let fishing_rod = "StardewValley.Game1​\
                           ._player​\
                           .netItems​.value​.Items​\
                           .array​.value​.elements​._items\
                           [5]​.value​\
                           .as<StardewValley.Tools.FishingRod>()";

        let time_until_fishing_bite = reader.parse_access_chain(&format!(
            "{fishing_rod}.timeUntilFishingBite"
        ))?;
        let fishing_bite_accumulator = reader.parse_access_chain(&format!(
            "{fishing_rod}.fishingBiteAccumulator"
        ))?;
        let time_until_fishing_nibble_done = reader.parse_access_chain(
            &format!("{fishing_rod}.timeUntilFishingNibbleDone"),
        )?;
        let fishing_nibble_accumulator = reader.parse_access_chain(
            &format!("{fishing_rod}.fishingNibbleAccumulator"),
        )?;

        let minigame = "StardewValley.Game1\
                        ._activeClickableMenu\
                        .as<StardewValley.Menus.BobberBar>()";
        let fish_position = reader
            .parse_access_chain(&format!("{minigame}.bobberTargetPosition"))?;
        let bar_position =
            reader.parse_access_chain(&format!("{minigame}.bobberBarPos"))?;
        let bar_height = reader
            .parse_access_chain(&format!("{minigame}.bobberBarHeight"))?;

        Ok(Self {
            table_state: TableState::new(),
            time_until_fishing_bite,
            fishing_bite_accumulator,
            time_until_fishing_nibble_done,
            fishing_nibble_accumulator,
            fish_position,
            bar_position,
            bar_height,
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
        let read_value = |chain: &PhysicalAccessChain| {
            let value = match chain.read(reader) {
                Ok(Some(value)) => format!("{value}"),
                Ok(None) => "".to_string(),
                Err(err) => format!("Error: {err}"),
            };
            Text::raw(value)
        };

        let table = Table::new(
            [
                Row::new([
                    Text::raw("Fish bites at:"),
                    read_value(&self.time_until_fishing_bite),
                ]),
                Row::new([
                    Text::raw("Time waiting for bite:"),
                    read_value(&self.fishing_bite_accumulator),
                ]),
                Row::new([
                    Text::raw("Fish leaves at:"),
                    read_value(&self.time_until_fishing_nibble_done),
                ]),
                Row::new([
                    Text::raw("Time waiting with hooked fish:"),
                    read_value(&self.fishing_nibble_accumulator),
                ]),
                Row::new([
                    Text::raw("Fish position:"),
                    read_value(&self.fish_position),
                ]),
                Row::new([
                    Text::raw("Fishing bar position:"),
                    read_value(&self.bar_position),
                ]),
                Row::new([
                    Text::raw("Fishing bar height:"),
                    read_value(&self.bar_height),
                ]),
            ],
            [Constraint::Min(33), Constraint::Percentage(100)],
        );

        ratatui::widgets::StatefulWidget::render(
            table,
            area,
            buf,
            &mut self.table_state,
        );
    }
}

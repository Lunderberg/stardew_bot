use dotnet_debugger::{CachedReader, PhysicalAccessChain};
use ratatui::{
    layout::Constraint,
    style::{Color, Modifier, Style},
    text::Text,
    widgets::{Row, Table, TableState},
};
use tui_utils::WidgetWindow;

use crate::Error;

pub struct FishingUI {
    table_state: TableState,

    /// True while the power of the cast is being selected.
    is_timing_cast: PhysicalAccessChain,

    /// A float, ranging from 0.0 to 1.0, indicating how far the
    /// bobber will travel.
    casting_power: PhysicalAccessChain,

    /// True during the casting animation, until the bobber starts
    /// traveling through the air.
    is_casting: PhysicalAccessChain,

    /// While while the bobber is in the air.
    bobber_in_air: PhysicalAccessChain,

    /// True if the bobber has landed in the water, and the player is
    /// waiting for a fish.  But it doesn't get reset until after the
    /// entire minigame.
    is_fishing: PhysicalAccessChain,

    /// The time at which a fish will bite
    time_until_fishing_bite: PhysicalAccessChain,

    /// The time spent waiting for a fish to bite.
    fishing_bite_accumulator: PhysicalAccessChain,

    /// True if a fish is nibbling on the hook.  But it doesn't get
    /// reset until after the entire minigame.
    is_nibbling: PhysicalAccessChain,

    /// The time at which a fish will no longer be hooked.
    time_until_fishing_nibble_done: PhysicalAccessChain,

    /// The time since a fish was hooked, incremented for each frame.
    /// When this value reaches `time_until_fishing_nibble_done`, the
    /// fish can no longer be caught.
    fishing_nibble_accumulator: PhysicalAccessChain,

    /// True if a fish is currently being caught
    minigame_in_progress: PhysicalAccessChain,

    /// The location of the fish within the fishing minigame.
    fish_position: PhysicalAccessChain,

    /// The location of the fish within the fishing minigame.
    bar_position: PhysicalAccessChain,

    /// The height of the fish within the fishing minigame.
    bar_height: PhysicalAccessChain,

    /// A boolean, indicating whether the fish is currently within the
    /// fishing bar.
    bobber_in_bar: PhysicalAccessChain,

    /// A float, indicating how close the fish is to escaping or being
    /// caught.  At 0.0, the fish escapes.  At 1.0, the fish is
    /// caught.
    catch_progress: PhysicalAccessChain,

    /// True if the minigame has completed, and the bobber is
    /// currently being pulled from the water.
    pulling_out_of_water: PhysicalAccessChain,
}

impl FishingUI {
    pub fn new(reader: CachedReader<'_>) -> Result<Self, Error> {
        let fishing_rod = "StardewValley.Game1​\
                           ._player​\
                           .netItems​.value​.Items​\
                           .array​.value​.elements​._items\
                           [5]​.value​\
                           .as<StardewValley.Tools.FishingRod>()";

        let is_timing_cast = reader
            .parse_access_chain(&format!("{fishing_rod}.isTimingCast"))?;
        let casting_power = reader
            .parse_access_chain(&format!("{fishing_rod}.castingPower"))?;

        let is_casting =
            reader.parse_access_chain(&format!("{fishing_rod}.isCasting"))?;

        let bobber_in_air = reader.parse_access_chain(&format!(
            "{fishing_rod}.castedButBobberStillInAir"
        ))?;

        let is_fishing =
            reader.parse_access_chain(&format!("{fishing_rod}.isFishing"))?;

        let time_until_fishing_bite = reader.parse_access_chain(&format!(
            "{fishing_rod}.timeUntilFishingBite"
        ))?;
        let fishing_bite_accumulator = reader.parse_access_chain(&format!(
            "{fishing_rod}.fishingBiteAccumulator"
        ))?;

        let is_nibbling =
            reader.parse_access_chain(&format!("{fishing_rod}.isNibbling"))?;
        let time_until_fishing_nibble_done = reader.parse_access_chain(
            &format!("{fishing_rod}.timeUntilFishingNibbleDone"),
        )?;
        let fishing_nibble_accumulator = reader.parse_access_chain(
            &format!("{fishing_rod}.fishingNibbleAccumulator"),
        )?;

        let minigame_in_progress =
            reader.parse_access_chain(&format!("{fishing_rod}.isReeling"))?;

        let minigame = "StardewValley.Game1\
                        ._activeClickableMenu\
                        .as<StardewValley.Menus.BobberBar>()";
        let fish_position = reader
            .parse_access_chain(&format!("{minigame}.bobberTargetPosition"))?;
        let bar_position =
            reader.parse_access_chain(&format!("{minigame}.bobberBarPos"))?;
        let bar_height = reader
            .parse_access_chain(&format!("{minigame}.bobberBarHeight"))?;
        let bobber_in_bar =
            reader.parse_access_chain(&format!("{minigame}.bobberInBar"))?;
        let catch_progress = reader
            .parse_access_chain(&format!("{minigame}.distanceFromCatching"))?;

        let pulling_out_of_water = reader
            .parse_access_chain(&format!("{fishing_rod}.pullingOutOfWater"))?;

        Ok(Self {
            table_state: TableState::new(),
            is_timing_cast,
            casting_power,
            is_casting,
            bobber_in_air,
            is_fishing,
            time_until_fishing_bite,
            fishing_bite_accumulator,
            is_nibbling,
            time_until_fishing_nibble_done,
            fishing_nibble_accumulator,
            minigame_in_progress,
            fish_position,
            bar_position,
            bar_height,
            bobber_in_bar,
            catch_progress,
            pulling_out_of_water,
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

        let get_bool = |chain: &PhysicalAccessChain| {
            chain
                .read(reader)
                .ok()
                .flatten()
                .map(|value| match value {
                    dotnet_debugger::RuntimePrimValue::Bool(b) => b,
                    _ => false,
                })
                .unwrap_or(false)
        };

        let pulling_out_of_water = get_bool(&self.pulling_out_of_water);
        let minigame_in_progress = get_bool(&self.minigame_in_progress);
        let is_nibbling = get_bool(&self.is_nibbling) && !minigame_in_progress;
        let is_fishing =
            get_bool(&self.is_fishing) && !is_nibbling && !minigame_in_progress;
        let bobber_in_air = get_bool(&self.bobber_in_air);
        let is_casting = get_bool(&self.is_casting);
        let is_timing_cast = get_bool(&self.is_timing_cast);

        let get_style = |is_active: bool| {
            if is_active {
                Style::new().fg(Color::White).add_modifier(Modifier::BOLD)
            } else {
                Style::new().fg(Color::DarkGray)
            }
        };

        let table = Table::new(
            [
                Row::new([
                    "Selecting cast distance:".into(),
                    read_value(&self.is_timing_cast),
                ])
                .style(get_style(is_timing_cast)),
                Row::new([
                    "Cast distance:".into(),
                    read_value(&self.casting_power),
                ])
                .style(get_style(is_timing_cast)),
                Row::new([
                    "Currently casting:".into(),
                    read_value(&self.is_casting),
                ])
                .style(get_style(is_casting)),
                Row::new([
                    "Bobber in air:".into(),
                    read_value(&self.is_casting),
                ])
                .style(get_style(bobber_in_air)),
                Row::new([
                    "Currently fishing:".into(),
                    read_value(&self.is_fishing),
                ])
                .style(get_style(is_fishing && !is_nibbling)),
                Row::new([
                    "Fish bites at:".into(),
                    read_value(&self.time_until_fishing_bite),
                ])
                .style(get_style(is_fishing && !is_nibbling)),
                Row::new([
                    "Time waiting for bite:".into(),
                    read_value(&self.fishing_bite_accumulator),
                ])
                .style(get_style(is_fishing && !is_nibbling)),
                Row::new([
                    "Fish nibbling hook:".into(),
                    read_value(&self.is_nibbling),
                ])
                .style(get_style(is_nibbling)),
                Row::new([
                    "Fish leaves at:".into(),
                    read_value(&self.time_until_fishing_nibble_done),
                ])
                .style(get_style(is_nibbling)),
                Row::new([
                    "Time waiting with hooked fish:".into(),
                    read_value(&self.fishing_nibble_accumulator),
                ])
                .style(get_style(is_nibbling)),
                Row::new([
                    "Minigame in progress:".into(),
                    read_value(&self.minigame_in_progress),
                ])
                .style(get_style(minigame_in_progress)),
                Row::new([
                    "Fish position:".into(),
                    read_value(&self.fish_position),
                ])
                .style(get_style(minigame_in_progress)),
                Row::new([
                    "Fishing bar position:".into(),
                    read_value(&self.bar_position),
                ])
                .style(get_style(minigame_in_progress)),
                Row::new([
                    "Fishing bar height:".into(),
                    read_value(&self.bar_height),
                ])
                .style(get_style(minigame_in_progress)),
                Row::new([
                    "Bobber in bar:".into(),
                    read_value(&self.bobber_in_bar),
                ])
                .style(get_style(minigame_in_progress)),
                Row::new([
                    "Catch progress:".into(),
                    read_value(&self.catch_progress),
                ])
                .style(get_style(minigame_in_progress)),
                Row::new([
                    "Pulling out of water:".into(),
                    read_value(&self.pulling_out_of_water),
                ])
                .style(get_style(pulling_out_of_water)),
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

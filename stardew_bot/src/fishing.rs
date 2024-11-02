use std::ops::Range;

use dotnet_debugger::{CachedReader, PhysicalAccessChain};
use itertools::Itertools as _;
use ratatui::{
    layout::Constraint,
    style::{Color, Modifier, Style, Stylize as _},
    symbols::Marker,
    text::Text,
    widgets::{
        Axis, Block, Chart, Dataset, GraphType, Row, StatefulWidget, Table,
        TableState, Widget,
    },
};
use tui_utils::{extensions::SplitRect as _, WidgetWindow};

use crate::Error;

pub struct FishingUI {
    table_state: TableState,

    history: Vec<FishingState>,

    /// The number of game ticks that have elapsed
    game_tick: PhysicalAccessChain,

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

    /// The difficulty of the fish.  Typically on a scale from 0-100
    fish_difficulty: PhysicalAccessChain,

    /// The location of the fish within the fishing minigame.
    ///
    /// Initially set to 508 (out of 568)
    ///
    /// Updated by fish_velocity on each frame.
    fish_position: PhysicalAccessChain,

    /// The velocity of the fish
    ///
    /// Initially set to zero
    ///
    /// Updated each frame based on distance between fish and its target.
    ///
    ///    accel = (target_pos - pos) / ( 20*rand() + 10 + max(0,100-difficulty))
    ///    vel = 0.8*vel + 0.2*accel
    fish_velocity: PhysicalAccessChain,

    /// The location that the fish is trying to reach within the
    /// fishing minigame.
    ///
    /// Initial position: (100-difficulty)/100 * 548
    ///
    ///    Higher difficulties make the fish jump upward at the start,
    ///    by setting the target position near the top.
    ///
    ///
    fish_target_position: PhysicalAccessChain,

    /// The location of the fish within the fishing minigame.
    bar_position: PhysicalAccessChain,

    /// The location of the fish within the fishing minigame.
    bar_velocity: PhysicalAccessChain,

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

struct FishingState {
    tick: i32,
    fishing_bar: Range<f32>,
    fish_in_bar: bool,
    fish_position: f32,
    fish_target_position: f32,
}

impl FishingUI {
    pub fn new(reader: CachedReader<'_>) -> Result<Self, Error> {
        let game_tick =
            reader.parse_access_chain("StardewValley.Game1​.ticks")?;

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

        let fish_difficulty =
            reader.parse_access_chain(&format!("{minigame}.difficulty"))?;
        let fish_position =
            reader.parse_access_chain(&format!("{minigame}.bobberPosition"))?;
        let fish_velocity =
            reader.parse_access_chain(&format!("{minigame}.bobberSpeed"))?;
        let fish_target_position = reader
            .parse_access_chain(&format!("{minigame}.bobberTargetPosition"))?;
        let bar_position =
            reader.parse_access_chain(&format!("{minigame}.bobberBarPos"))?;
        let bar_velocity =
            reader.parse_access_chain(&format!("{minigame}.bobberBarSpeed"))?;
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
            history: Vec::new(),
            game_tick,
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
            fish_difficulty,
            fish_position,
            fish_velocity,
            fish_target_position,
            bar_position,
            bar_velocity,
            bar_height,
            bobber_in_bar,
            catch_progress,
            pulling_out_of_water,
        })
    }

    fn current_state(
        &self,
        reader: CachedReader<'_>,
    ) -> Result<Option<FishingState>, dotnet_debugger::Error> {
        let minigame_in_progress = self
            .minigame_in_progress
            .read_as::<bool>(reader)?
            .unwrap_or(false);

        if !minigame_in_progress {
            return Ok(None);
        }

        let tick = self.game_tick.read_as::<i32>(reader)?.unwrap_or(-1);
        if self
            .history
            .last()
            .map(|prev| prev.tick == tick)
            .unwrap_or(false)
        {
            return Ok(None);
        }

        let fishing_bar_bottom =
            self.bar_position.read_as::<f32>(reader)?.unwrap_or(-1.0);
        let fishing_bar_height =
            self.bar_height.read_as::<i32>(reader)?.unwrap_or(-1);
        let fishing_bar = fishing_bar_bottom
            ..fishing_bar_bottom + (fishing_bar_height as f32);

        let fish_in_bar =
            self.bobber_in_bar.read_as::<bool>(reader)?.unwrap_or(false);
        let fish_position =
            self.fish_position.read_as::<f32>(reader)?.unwrap_or(-1.0);
        let fish_target_position = self
            .fish_target_position
            .read_as::<f32>(reader)?
            .unwrap_or(-1.0);

        Ok(Some(FishingState {
            tick,
            fishing_bar,
            fish_in_bar,
            fish_position,
            fish_target_position,
        }))
    }
}

impl WidgetWindow for FishingUI {
    fn title(&self) -> std::borrow::Cow<str> {
        "Fishing".into()
    }

    fn periodic_update<'a>(
        &mut self,
        globals: &'a tui_utils::TuiGlobals,
        _side_effects: &'a mut tui_utils::WidgetSideEffects,
    ) -> Result<(), tui_utils::Error> {
        let reader = globals.cached_reader();

        let is_casting =
            self.is_casting.read_as::<bool>(reader)?.unwrap_or(false);

        if is_casting {
            self.history.clear();
        }

        if let Some(state) = self.current_state(reader)? {
            self.history.push(state);
        }

        Ok(())
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
                .read_as::<bool>(reader)
                .ok()
                .flatten()
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
                Row::new(["Game tick:".into(), read_value(&self.game_tick)]),
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
                    "Fish difficulty:".into(),
                    read_value(&self.fish_difficulty),
                ])
                .style(get_style(minigame_in_progress)),
                Row::new([
                    "Fish position:".into(),
                    read_value(&self.fish_position),
                ])
                .style(get_style(minigame_in_progress)),
                Row::new([
                    "Fish velocity:".into(),
                    read_value(&self.fish_velocity),
                ])
                .style(get_style(minigame_in_progress)),
                Row::new([
                    "Fish target position:".into(),
                    read_value(&self.fish_target_position),
                ])
                .style(get_style(minigame_in_progress)),
                Row::new([
                    "Fishing bar position:".into(),
                    read_value(&self.bar_position),
                ])
                .style(get_style(minigame_in_progress)),
                Row::new([
                    "Fishing bar velocity:".into(),
                    read_value(&self.bar_velocity),
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

        let graph_data: (Vec<_>, Vec<_>, Vec<_>) = self
            .history
            .iter()
            .map(|state| {
                (
                    (state.tick as f64, state.fish_position as f64),
                    (state.tick as f64, state.fishing_bar.start as f64),
                    (state.tick as f64, state.fishing_bar.end as f64),
                )
            })
            .multiunzip();
        let datasets = vec![
            Dataset::default()
                .data(&graph_data.1)
                .graph_type(GraphType::Line)
                .name("Bar (top)")
                .marker(Marker::Block)
                .style(Style::default().green()),
            Dataset::default()
                .data(&graph_data.2)
                .graph_type(GraphType::Line)
                .name("Bar (bottom)")
                .marker(Marker::Dot)
                .style(Style::default().bg(Color::Green)),
            Dataset::default()
                .data(&graph_data.0)
                .graph_type(GraphType::Line)
                .name("Fish")
                .marker(Marker::Braille)
                .style(Style::default().cyan()),
        ];

        let x_axis = Axis::default()
            .title("Game tick")
            .style(Style::default().white())
            .bounds(
                match self
                    .history
                    .iter()
                    .map(|state| state.tick as f64)
                    .minmax()
                {
                    itertools::MinMaxResult::NoElements => [0.0, 0.0],
                    itertools::MinMaxResult::OneElement(value) => {
                        [value, value + 1.0]
                    }
                    itertools::MinMaxResult::MinMax(a, b) => [a, b],
                },
            );

        let y_axis = Axis::default()
            .title("Height")
            .style(Style::default().white())
            .bounds([0.0, 568.0]);

        let graph = Chart::new(datasets)
            .block(Block::new().title(format!(
                "Graph with {} data points",
                self.history.len()
            )))
            .x_axis(x_axis)
            .y_axis(y_axis);

        let (table_area, graph_area) = area.split_from_left(50);

        StatefulWidget::render(table, table_area, buf, &mut self.table_state);

        graph.render(graph_area, buf);
    }
}

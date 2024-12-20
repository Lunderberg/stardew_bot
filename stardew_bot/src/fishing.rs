use std::ops::Range;

use dotnet_debugger::{CachedReader, VirtualMachine};
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

use crate::{Error, GameAction};

pub struct FishingUI {
    table_state: TableState,

    history: Vec<FishingState>,

    /// The number of game ticks that have elapsed
    game_tick: VirtualMachine,

    /// True while the power of the cast is being selected.
    is_timing_cast: VirtualMachine,

    /// A float, ranging from 0.0 to 1.0, indicating how far the
    /// bobber will travel.
    casting_power: VirtualMachine,

    /// True during the casting animation, until the bobber starts
    /// traveling through the air.
    is_casting: VirtualMachine,

    /// While while the bobber is in the air.
    bobber_in_air: VirtualMachine,

    /// True if the bobber has landed in the water, and the player is
    /// waiting for a fish.  But it doesn't get reset until after the
    /// entire minigame.
    is_fishing: VirtualMachine,

    /// The time at which a fish will bite
    time_until_fishing_bite: VirtualMachine,

    /// The time spent waiting for a fish to bite.
    fishing_bite_accumulator: VirtualMachine,

    /// True if a fish is nibbling on the hook.  But it doesn't get
    /// reset until after the entire minigame.
    is_nibbling: VirtualMachine,

    /// The time at which a fish will no longer be hooked.
    time_until_fishing_nibble_done: VirtualMachine,

    /// The time since a fish was hooked, incremented for each frame.
    /// When this value reaches `time_until_fishing_nibble_done`, the
    /// fish can no longer be caught.
    fishing_nibble_accumulator: VirtualMachine,

    /// True if a fish is currently being caught
    minigame_in_progress: VirtualMachine,

    /// The difficulty of the fish.  Typically on a scale from 0-100
    fish_difficulty: VirtualMachine,

    /// The location of the fish within the fishing minigame.
    ///
    /// Initially set to 508 (out of 568)
    ///
    /// Updated by fish_velocity on each frame.
    fish_position: VirtualMachine,

    /// The velocity of the fish
    ///
    /// Initially set to zero
    ///
    /// Updated each frame based on distance between fish and its target.
    ///
    ///    accel = (target_pos - pos) / ( 20*rand() + 10 + max(0,100-difficulty))
    ///    vel = 0.8*vel + 0.2*accel
    fish_velocity: VirtualMachine,

    /// The location that the fish is trying to reach within the
    /// fishing minigame.
    ///
    /// Initial position: (100-difficulty)/100 * 548
    ///
    ///    Higher difficulties make the fish jump upward at the start,
    ///    by setting the target position near the top.
    ///
    ///
    fish_target_position: VirtualMachine,

    /// The location of the fish within the fishing minigame.
    bar_position: VirtualMachine,

    /// The location of the fish within the fishing minigame.
    bar_velocity: VirtualMachine,

    /// The height of the fish within the fishing minigame.
    bar_height: VirtualMachine,

    /// A boolean, indicating whether the fish is currently within the
    /// fishing bar.
    bobber_in_bar: VirtualMachine,

    /// A float, indicating how close the fish is to escaping or being
    /// caught.  At 0.0, the fish escapes.  At 1.0, the fish is
    /// caught.
    catch_progress: VirtualMachine,

    /// True if the minigame has completed, and the bobber is
    /// currently being pulled from the water.
    pulling_out_of_water: VirtualMachine,

    /// True if the minigame has completed, and the fish is currently
    /// being displayed above the player's head.
    showing_fish: VirtualMachine,

    /// True if the minigame has completed, and the player is
    /// currently being shown the treasure that has been caught.
    showing_treasure: VirtualMachine,
}

struct FishingState {
    tick: i32,
    bar_position: Range<f32>,
    bar_velocity: f32,
    fish_in_bar: bool,
    fish_position: f32,
    fish_velocity: f32,
    fish_target_position: f32,
    fish_difficulty: f32,
}

impl FishingUI {
    pub fn new(reader: CachedReader<'_>) -> Result<Self, Error> {
        let game_tick = reader.parse_expr("StardewValley.Game1.ticks")?;

        let fishing_rod = "StardewValley.Game1\
                           ._player\
                           .as<StardewValley.Farmer>()\
                           .netItems.value.Items\
                           .array.value.elements._items\
                           [StardewValley.Game1._player.currentToolIndex.value].value\
                           .as<StardewValley.Tools.FishingRod>()";

        let is_timing_cast =
            reader.parse_expr(&format!("{fishing_rod}.isTimingCast"))?;
        let casting_power =
            reader.parse_expr(&format!("{fishing_rod}.castingPower"))?;

        let is_casting =
            reader.parse_expr(&format!("{fishing_rod}.isCasting"))?;

        let bobber_in_air = reader
            .parse_expr(&format!("{fishing_rod}.castedButBobberStillInAir"))?;

        let is_fishing =
            reader.parse_expr(&format!("{fishing_rod}.isFishing"))?;

        let time_until_fishing_bite = reader
            .parse_expr(&format!("{fishing_rod}.timeUntilFishingBite"))?;
        let fishing_bite_accumulator = reader
            .parse_expr(&format!("{fishing_rod}.fishingBiteAccumulator"))?;

        let is_nibbling =
            reader.parse_expr(&format!("{fishing_rod}.isNibbling"))?;
        let time_until_fishing_nibble_done = reader
            .parse_expr(&format!("{fishing_rod}.timeUntilFishingNibbleDone"))?;
        let fishing_nibble_accumulator = reader
            .parse_expr(&format!("{fishing_rod}.fishingNibbleAccumulator"))?;

        let minigame_in_progress =
            reader.parse_expr(&format!("{fishing_rod}.isReeling"))?;

        let minigame = "StardewValley.Game1\
                        ._activeClickableMenu\
                        .as<StardewValley.Menus.BobberBar>()";

        let fish_difficulty =
            reader.parse_expr(&format!("{minigame}.difficulty"))?;
        let fish_position =
            reader.parse_expr(&format!("{minigame}.bobberPosition"))?;
        let fish_velocity =
            reader.parse_expr(&format!("{minigame}.bobberSpeed"))?;
        let fish_target_position =
            reader.parse_expr(&format!("{minigame}.bobberTargetPosition"))?;
        let bar_position =
            reader.parse_expr(&format!("{minigame}.bobberBarPos"))?;
        let bar_velocity =
            reader.parse_expr(&format!("{minigame}.bobberBarSpeed"))?;
        let bar_height =
            reader.parse_expr(&format!("{minigame}.bobberBarHeight"))?;
        let bobber_in_bar =
            reader.parse_expr(&format!("{minigame}.bobberInBar"))?;
        let catch_progress =
            reader.parse_expr(&format!("{minigame}.distanceFromCatching"))?;

        let pulling_out_of_water =
            reader.parse_expr(&format!("{fishing_rod}.pullingOutOfWater"))?;

        let showing_fish =
            reader.parse_expr(&format!("{fishing_rod}.fishCaught"))?;

        let showing_treasure =
            reader.parse_expr(&format!("{fishing_rod}.showingTreasure"))?;

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
            showing_fish,
            showing_treasure,
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

        let fishing_bar_bottom = self
            .bar_position
            .read_as::<f32>(reader)?
            .unwrap_or(f32::NAN);
        let fishing_bar_height =
            self.bar_height.read_as::<i32>(reader)?.unwrap_or(-1);
        let bar_position = fishing_bar_bottom
            ..fishing_bar_bottom + (fishing_bar_height as f32);
        let bar_velocity = self
            .bar_velocity
            .read_as::<f32>(reader)?
            .unwrap_or(f32::NAN);

        let fish_in_bar =
            self.bobber_in_bar.read_as::<bool>(reader)?.unwrap_or(false);
        let fish_position = self
            .fish_position
            .read_as::<f32>(reader)?
            .unwrap_or(f32::NAN);
        let fish_velocity = self
            .fish_velocity
            .read_as::<f32>(reader)?
            .unwrap_or(f32::NAN);
        let fish_target_position = self
            .fish_target_position
            .read_as::<f32>(reader)?
            .unwrap_or(-1.0);
        let fish_difficulty =
            self.fish_difficulty.read_as::<f32>(reader)?.unwrap_or(-1.0);

        Ok(Some(FishingState {
            tick,
            bar_position,
            bar_velocity,
            fish_in_bar,
            fish_position,
            fish_velocity,
            fish_target_position,
            fish_difficulty,
        }))
    }
}

impl FishingState {
    fn predicted_positions(&self) -> impl Fn(f32) -> f32 {
        // m*accel + b*velocity + k*position = 0
        // position = (A*cos(lambda*t) + B*sin(lambda*t))*exp(-lambda*t)
        //
        // The position here is not `fish_position`, but is
        // relative to the `fish_target_position`, since that
        // makes the equation simpler.
        let k = 1.0 / (5.0 * (120.0 - self.fish_difficulty));

        let b = 1.0 / 5.0;
        let lambda = b / 2.0;
        let tau = (lambda * lambda - k).sqrt();

        let initial_offset = self.fish_position - self.fish_target_position;
        let initial_velocity = self.fish_velocity;

        let coefficient_of_pos =
            ((tau - lambda) * initial_offset - initial_velocity) / (2.0 * tau);
        let coefficient_of_neg =
            ((tau + lambda) * initial_offset + initial_velocity) / (2.0 * tau);

        let fish_target_position = self.fish_target_position;

        move |t: f32| -> f32 {
            let offset = coefficient_of_pos * (-(lambda + tau) * t).exp()
                + coefficient_of_neg * (-(lambda - tau) * t).exp();
            let predicted = fish_target_position + offset;

            predicted
        }
    }

    fn should_move_upward(&self) -> bool {
        const LOOKAHEAD_TIME: f32 = 30.0;

        let fish_position = self.predicted_positions()(LOOKAHEAD_TIME);

        let bar_position =
            (self.bar_position.start + self.bar_position.end - 28.0) / 2.0
                - 16.0;

        let bar_velocity = self.bar_velocity;

        let bar_velocity_if_click = bar_velocity - 0.25;
        let bar_velocity_if_release = bar_velocity + 0.25;

        let bar_position_if_click =
            bar_velocity_if_click * LOOKAHEAD_TIME + bar_position;
        let bar_position_if_release =
            bar_velocity_if_release * LOOKAHEAD_TIME + bar_position;

        let distance_if_click = (fish_position - bar_position_if_click).abs();
        let distance_if_release =
            (fish_position - bar_position_if_release).abs();

        distance_if_click < distance_if_release
    }
}

impl WidgetWindow<Error> for FishingUI {
    fn title(&self) -> std::borrow::Cow<str> {
        "Fishing".into()
    }

    fn periodic_update<'a>(
        &mut self,
        globals: &'a tui_utils::TuiGlobals,
        side_effects: &'a mut tui_utils::WidgetSideEffects,
    ) -> Result<(), Error> {
        let reader = globals.cached_reader();

        self.game_tick.evaluate(reader)?;
        self.is_timing_cast.evaluate(reader)?;
        self.casting_power.evaluate(reader)?;
        self.is_casting.evaluate(reader)?;
        self.bobber_in_air.evaluate(reader)?;
        self.is_fishing.evaluate(reader)?;
        self.time_until_fishing_bite.evaluate(reader)?;
        self.fishing_bite_accumulator.evaluate(reader)?;
        self.is_nibbling.evaluate(reader)?;
        self.time_until_fishing_nibble_done.evaluate(reader)?;
        self.fishing_nibble_accumulator.evaluate(reader)?;
        self.minigame_in_progress.evaluate(reader)?;
        self.fish_difficulty.evaluate(reader)?;
        self.fish_position.evaluate(reader)?;
        self.fish_velocity.evaluate(reader)?;
        self.fish_target_position.evaluate(reader)?;
        self.bar_position.evaluate(reader)?;
        self.bar_velocity.evaluate(reader)?;
        self.bar_height.evaluate(reader)?;
        self.bobber_in_bar.evaluate(reader)?;
        self.catch_progress.evaluate(reader)?;
        self.pulling_out_of_water.evaluate(reader)?;
        self.showing_fish.evaluate(reader)?;
        self.showing_treasure.evaluate(reader)?;

        let get_bool = |chain: &VirtualMachine| {
            chain
                .read_as::<bool>(reader)
                .ok()
                .flatten()
                .unwrap_or(false)
        };

        let get_float = |chain: &VirtualMachine| -> Result<_, Error> {
            Ok(chain
                .read_as::<f32>(reader)?
                .ok_or(Error::ExpectedNoneEmptyValue)?)
        };

        let showing_fish = get_bool(&self.showing_fish);
        let minigame_in_progress = get_bool(&self.minigame_in_progress);
        let is_fishing = get_bool(&self.is_fishing);
        let is_nibbling = !showing_fish
            && !minigame_in_progress
            && get_bool(&self.is_nibbling);
        let is_casting = get_bool(&self.is_casting);
        let is_timing_cast = get_bool(&self.is_timing_cast);

        if is_casting {
            self.history.clear();
        }

        if let Some(state) = self.current_state(reader)? {
            self.history.push(state);
        }

        if is_timing_cast {
            let casting_power = get_float(&self.casting_power)?;
            let action = if casting_power > 0.9 {
                GameAction::ReleaseTool
            } else {
                GameAction::HoldTool
            };
            side_effects.broadcast(action);
        } else if minigame_in_progress {
            if let Some(state) = self.history.last() {
                let action = if state.should_move_upward() {
                    GameAction::HoldTool
                } else {
                    GameAction::ReleaseTool
                };
                side_effects.broadcast(action);
            }
        } else if is_nibbling {
            side_effects.broadcast(GameAction::ReleaseTool);
        } else if is_fishing {
            side_effects.broadcast(GameAction::HoldTool);
        } else if showing_fish {
            side_effects.broadcast(GameAction::HoldTool);
        } else {
            side_effects.broadcast(GameAction::HoldTool);
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
        let read_value = |chain: &VirtualMachine| {
            let value = match chain.evaluate(reader).map(|values| values[0]) {
                Ok(Some(value)) => format!("{value}"),
                Ok(None) => "".to_string(),
                Err(err) => format!("Error: {err}"),
            };
            Text::raw(value)
        };

        let get_bool = |chain: &VirtualMachine| {
            chain
                .evaluate(reader)
                .ok()
                .and_then(|values| values[0])
                .and_then(|value| value.try_into().ok())
                .unwrap_or(false)
        };

        let showing_treasure = get_bool(&self.showing_treasure);
        let showing_fish = get_bool(&self.showing_fish);
        let pulling_out_of_water = get_bool(&self.pulling_out_of_water);
        let minigame_in_progress = get_bool(&self.minigame_in_progress);
        let is_nibbling = !showing_fish
            && !minigame_in_progress
            && get_bool(&self.is_nibbling);
        let is_fishing =
            !is_nibbling && !minigame_in_progress && get_bool(&self.is_fishing);
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
                Row::new([
                    "Showing fish:".into(),
                    read_value(&self.showing_fish),
                ])
                .style(get_style(showing_fish)),
                Row::new([
                    "Showing treasure:".into(),
                    read_value(&self.showing_treasure),
                ])
                .style(get_style(showing_treasure)),
            ],
            [Constraint::Min(33), Constraint::Percentage(100)],
        );

        const MAX_SIZE: f64 = 568.0;

        let fish_points_in_bar: Vec<_> = self
            .history
            .iter()
            .filter(|state| state.fish_in_bar)
            .map(|state| {
                (state.tick as f64, MAX_SIZE - state.fish_position as f64)
            })
            .collect();
        let fish_points_outside_bar: Vec<_> = self
            .history
            .iter()
            .filter(|state| !state.fish_in_bar)
            .map(|state| {
                (state.tick as f64, MAX_SIZE - state.fish_position as f64)
            })
            .collect();
        let fishing_bar_data: Vec<_> = self
            .history
            .iter()
            .flat_map(|state| {
                let top = state.bar_position.end as f64;
                let bottom = state.bar_position.start as f64;

                // The computation of `bobberInBar` requires that the
                // following conditions hold:
                //
                //    bobberPosition + 12f <= bobberBarPos - 32f + bobberBarHeight
                //    bobberPosition - 16f >= bobberBarPos - 32f
                //
                // Presumably, these are to account for the finite
                // size of the fish sprite, which must be entirely
                // within the bar.  Practically, if we want to apply
                // `bar <= fish <= bar+height`, it means that both the
                // position is offset, and that the bar is smaller.
                //
                //    bobberBarPos <= bobberPosition + 16.0 <= bobberBarPos + bobberBarHeight - 28.0
                //    (bobberBarPos-16.0) <= bobberPosition <= (bobberBarPos-16.0) + bobberBarHeight - 28.0
                let top = top - 44.0;
                let bottom = bottom - 16.0;

                let top = MAX_SIZE - top;
                let bottom = MAX_SIZE - bottom;

                let mid = (top + bottom) / 2.0;
                let tick = state.tick as f64;
                [(tick, mid), (tick, bottom), (tick, top), (tick, mid)]
            })
            .collect();

        let fish_target_points: Vec<_> = self
            .history
            .iter()
            .tuples()
            .filter(|(a, b)| a.fish_target_position != b.fish_target_position)
            .map(|(_, b)| b)
            .map(|state| {
                (
                    state.tick as f64,
                    MAX_SIZE - state.fish_target_position as f64,
                )
            })
            .collect();

        let prediction_size = 120;
        let predicted_fish_points: Vec<_> = self
            .history
            .last()
            .map(|state| {
                let predictor = state.predicted_positions();

                (0..prediction_size).map(move |i| {
                    let pos = predictor(i as f32) as f64;
                    let tick = state.tick as f64;
                    let i = i as f64;
                    (tick + i, MAX_SIZE - pos)
                })
            })
            .into_iter()
            .flatten()
            .collect();

        let datasets = vec![
            Dataset::default()
                .data(&fishing_bar_data)
                .graph_type(GraphType::Line)
                .name("Fishing Bar")
                .marker(Marker::Block)
                .style(Color::Rgb(10, 50, 10)),
            Dataset::default()
                .data(&fish_points_outside_bar)
                .graph_type(GraphType::Scatter)
                .name("Fish")
                .marker(Marker::Braille)
                .style(Style::default().blue()),
            Dataset::default()
                .data(&fish_points_in_bar)
                .graph_type(GraphType::Scatter)
                .marker(Marker::Braille)
                .style(Style::default().red()),
            Dataset::default()
                .data(&predicted_fish_points)
                .graph_type(GraphType::Scatter)
                .name("Fish (pred.)")
                .marker(Marker::Braille)
                .style(Style::default().yellow()),
            Dataset::default()
                .data(&fish_target_points)
                .graph_type(GraphType::Scatter)
                .name("Fish's Target")
                .marker(Marker::Dot)
                .style(Style::default().yellow()),
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
                        [value, value + 1.0 + (prediction_size as f64)]
                    }
                    itertools::MinMaxResult::MinMax(a, b) => {
                        [a, b + (prediction_size as f64)]
                    }
                },
            );

        let y_axis = Axis::default()
            .title("Height")
            .style(Style::default().white())
            .bounds([0.0, MAX_SIZE]);

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

    fn apply_key_binding<'a>(
        &'a mut self,
        _keystrokes: &'a tui_utils::inputs::KeySequence,
        _globals: &'a tui_utils::TuiGlobals,
        _side_effects: &'a mut tui_utils::WidgetSideEffects,
    ) -> tui_utils::inputs::KeyBindingMatch {
        tui_utils::inputs::KeyBindingMatch::Mismatch
    }

    fn apply_side_effects<'a>(
        &'a mut self,
        _globals: &'a tui_utils::TuiGlobals,
        _side_effects: &'a mut tui_utils::WidgetSideEffects,
    ) -> Result<(), Error> {
        Ok(())
    }
}

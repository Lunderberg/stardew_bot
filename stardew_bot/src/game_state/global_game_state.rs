use std::collections::HashMap;

use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};

use crate::Error;

#[derive(RustNativeObject, Debug, Clone)]
pub struct GlobalGameState {
    /// The unique id of the game, used for seeding RNG
    pub unique_id: u64,

    /// The number of game ticks that have elapsed
    pub game_tick: i32,

    /// The number of game ticks that have elapsed
    pub game_mode_tick: i32,

    /// The current in-game time.
    ///
    /// This is a kind of weird field, because while it is an integer,
    /// it shows a 24-hour clock.  It is set to 600 at the start of
    /// each day, and is incremented by 10 within
    /// `Game1.performTenMinuteClockUpdate`, advancing to the next
    /// multiple of 100 whenever the tens digit is 6 or higher.
    /// Effectively, thiis number is human-readable as a 24-hour
    /// clock.
    ///
    /// At some point, if more precise timings are required (e.g. to
    /// arrive at a shop just before it closes), may need to also
    /// check `Game1.gameTimeInterval`,
    /// `Game1.realMilliSecondsPerGameTenMinutes`, and the current
    /// location's `ExtraMillisecondsPerInGameMinute` field.
    pub in_game_time: i32,

    /// If the screen is currently fading to black.
    pub currently_fading_to_black: bool,

    pub stats: HashMap<String, u32>,
}

#[derive(RustNativeObject, Default)]
struct Stats(HashMap<String, u32>);

impl GlobalGameState {
    pub(crate) fn def_read_global_game_state(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function(
            "new_global_game_state",
            |unique_id: u64,
             game_tick: i32,
             game_mode_tick: i32,
             in_game_time: i32,
             stats: &Stats,
             currently_fading_to_black: bool| GlobalGameState {
                game_tick,
                unique_id,
                game_mode_tick,
                in_game_time,
                stats: stats.0.clone(),
                currently_fading_to_black,
            },
        )?;

        graph.named_native_function("new_game_stats", || Stats::default())?;
        graph.named_native_function(
            "define_game_stat",
            |stats: &mut Stats, name: &str, value: u32| {
                stats.0.insert(name.to_string(), value);
            },
        )?;

        let game_mode_tick = graph.static_field(
            "StardewValley.Game1",
            "<gameModeTicks>k__BackingField",
        );
        graph.name(game_mode_tick, "game_mode_tick")?;

        let func = graph.parse(stringify! {
            fn read_global_game_state() {
                let game_tick = StardewValley.Game1.ticks;
                let unique_id = StardewValley.Game1.uniqueIDForThisGame;

                let in_game_time = StardewValley.Game1.timeOfDay;

                let currently_fading_to_black = StardewValley.Game1
                    .screenFade
                    .fadeToBlack;

                let stats = StardewValley.Game1._player.stats.Values;
                let num_stats = stats._count.prim_cast::<usize>();
                let stat_dict= (0..num_stats)
                    .reduce(
                        new_game_stats(),
                        |stat_dict, i| {
                            let entry = stats._entries[i];
                            let stat_name = entry.key.read_string();
                            let stat_value = entry.value;
                            define_game_stat(stat_dict, stat_name, stat_value)
                        });

                new_global_game_state(
                    unique_id,
                    game_tick,
                    game_mode_tick,
                    in_game_time,
                    stat_dict,
                    currently_fading_to_black,
                )
            }
        })?;

        Ok(func)
    }

    pub(crate) fn get_stat(&self, stat_name: &str) -> Result<u32, Error> {
        self.stats
            .get(stat_name)
            .cloned()
            .ok_or_else(|| Error::MissingStat(stat_name.to_string()))
    }
}

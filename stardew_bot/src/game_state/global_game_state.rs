use std::collections::{HashMap, HashSet};

use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};

use crate::Error;

#[derive(RustNativeObject, Debug, Clone)]
pub struct GlobalGameState {
    /// The unique id of the game, used for seeding RNG.  Generated
    /// when starting a new game, and never changes.
    pub game_id: u64,

    /// The unique multiplayer ID, used for communication and
    /// sometimes for seeding RNG.  Generated each time the game is
    /// loaded.
    pub multiplayer_id: i64,

    /// The number of game ticks that have elapsed
    pub game_tick: i32,

    /// The number of game ticks that have elapsed while in the
    /// present game mode.
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

    /// If an event is currently playing.
    pub event_up: bool,

    pub stats: HashMap<String, u32>,

    /// The lowest level that has been reached in the mines.  Values
    /// above 120 indicate reaching level (value-120) within
    /// SkullCavern.
    pub lowest_mine_level_reached: i32,

    /// Events that have been triggered within the game.
    pub events_triggered: HashSet<String>,

    /// Events that have been queued to occur at the start of the next
    /// day.
    pub queued_events: HashSet<String>,

    /// Flags indicatating which items have been given to a bundle.
    /// The lookup key is the `bundle_index`, and each boolean flag
    /// corresponds to an ingredient within that bundle.
    pub bundles: HashMap<i32, Vec<bool>>,
}

#[derive(RustNativeObject, Default)]
struct Stats(HashMap<String, u32>);

#[derive(RustNativeObject, Debug, Clone)]
struct BundleFlags {
    bundle_index: i32,
    flags: Vec<bool>,
}

impl GlobalGameState {
    pub(crate) fn def_read_global_game_state(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function(
            "new_bundle_flags",
            |bundle_index: i32, flags: &Vec<bool>| BundleFlags {
                bundle_index,
                flags: flags.clone(),
            },
        )?;

        graph.named_native_function(
            "new_global_game_state",
            |game_id: u64,
             multiplayer_id: i64,
             game_tick: i32,
             game_mode_tick: i32,
             in_game_time: i32,
             stats: &Stats,
             currently_fading_to_black: bool,
             event_up: bool,
             lowest_mine_level_reached: i32,
             events_triggered: &Vec<String>,
             queued_events: &Vec<String>,
             bundle_flags: &Vec<BundleFlags>| {
                let bundles = bundle_flags
                    .iter()
                    .map(|bundle| (bundle.bundle_index, bundle.flags.clone()))
                    .collect();
                let events_triggered =
                    events_triggered.iter().cloned().collect();

                let queued_events = queued_events.iter().cloned().collect();
                GlobalGameState {
                    game_tick,
                    multiplayer_id,
                    game_id,
                    game_mode_tick,
                    in_game_time,
                    stats: stats.0.clone(),
                    currently_fading_to_black,
                    event_up,
                    lowest_mine_level_reached,
                    events_triggered,
                    queued_events,
                    bundles,
                }
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
                let game_id = StardewValley.Game1.uniqueIDForThisGame;
                let multiplayer_id = StardewValley.Game1
                    ._player
                    .uniqueMultiplayerID
                    .value;

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

                let event_up = StardewValley.Game1.eventUp;

                let lowest_mine_level_reached = StardewValley.Game1
                    .netWorldState
                    .value
                    .lowestMineLevel
                    .value;

                let events_triggered = {
                    let mail = StardewValley.Game1._player.mailReceived;
                    let num = mail.Set._count.prim_cast::<usize>();
                    (0..num)
                        .map(|i| mail.Set._entries[i].Value.read_string())
                        .collect()
                };

                let queued_events = {
                    let mail = StardewValley.Game1._player.mailForTomorrow;
                    let num = mail.Set._count.prim_cast::<usize>();
                    (0..num)
                        .map(|i| mail.Set._entries[i].Value.read_string())
                        .filter(|opt| opt.is_some())
                        .collect()
                };

                let bundle_dict = StardewValley.Game1
                    .netWorldState
                    .value
                    .bundles
                    .dict;
                let num_bundles = bundle_dict
                    ._count
                    .prim_cast::<usize>();

                let bundle_flags = (0..num_bundles)
                    .map(|i| bundle_dict._entries[i])
                    .map(|entry| {
                        let bundle_index = entry.key;
                        let flag_list = entry.value.elements;
                        let num_flags = flag_list
                            ._size
                            .prim_cast::<usize>();

                        let flags = (0..num_flags)
                            .map(|i| flag_list._items[i])
                            .map(|flag| flag.value.prim_cast::<bool>())
                            .collect();

                        new_bundle_flags(
                            bundle_index,
                            flags,
                        )
                    })
                    .collect();

                new_global_game_state(
                    game_id,
                    multiplayer_id,
                    game_tick,
                    game_mode_tick,
                    in_game_time,
                    stat_dict,
                    currently_fading_to_black,
                    event_up,
                    lowest_mine_level_reached,
                    events_triggered,
                    queued_events,
                    bundle_flags,
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

    pub(crate) fn days_played(&self) -> u32 {
        self.get_stat("daysPlayed").unwrap_or(1)
    }
}

use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};

use crate::Error;

#[derive(RustNativeObject, Debug, Clone)]
pub struct FishingState {
    /// The number of game ticks that have elapsed
    pub game_tick: i32,

    pub is_holding_rod: bool,

    /// True while the power of the cast is being selected.
    pub is_timing_cast: bool,

    /// A float, ranging from 0.0 to 1.0, indicating how far the
    /// bobber will travel.
    pub casting_power: f32,

    /// True during the casting animation, until the bobber starts
    /// traveling through the air.
    pub is_casting: bool,

    /// While while the bobber is in the air.
    pub bobber_in_air: bool,

    /// True if the bobber has landed in the water, and the player is
    /// waiting for a fish.  But it doesn't get reset until after the
    /// entire minigame.
    pub is_fishing: bool,

    /// The time at which a fish will bite
    pub time_until_fishing_bite: f32,

    /// The time spent waiting for a fish to bite.
    pub fishing_bite_accumulator: f32,

    /// True if a fish is nibbling on the hook.  But it doesn't get
    /// reset until after the entire minigame.
    pub is_nibbling: bool,

    /// The time at which a fish will no longer be hooked.
    pub time_until_fishing_nibble_done: f32,

    /// The time since a fish was hooked, incremented for each frame.
    /// When this value reaches `time_until_fishing_nibble_done`, the
    /// fish can no longer be caught.
    pub fishing_nibble_accumulator: f32,

    /// True if a fish is currently being caught
    pub minigame_in_progress: bool,

    /// The difficulty of the fish.  Typically on a scale from 0-100
    pub fish_difficulty: f32,

    /// The location of the fish within the fishing minigame.
    ///
    /// Initially set to 508 (out of 568)
    ///
    /// Updated by fish_velocity on each frame.
    pub fish_position: f32,

    /// The velocity of the fish
    ///
    /// Initially set to zero
    ///
    /// Updated each frame based on distance between fish and its target.
    ///
    ///    accel = (target_pos - pos) / ( 20*rand() + 10 + max(0,100-difficulty))
    ///    vel = 0.8*vel + 0.2*accel
    pub fish_velocity: f32,

    /// The location that the fish is trying to reach within the
    /// fishing minigame.
    ///
    /// Initial position: (100-difficulty)/100 * 548
    ///
    ///    Higher difficulties make the fish jump upward at the start,
    ///    by setting the target position near the top.
    ///
    ///
    pub fish_target_position: f32,

    /// The location of the fish within the fishing minigame.
    pub bar_position: f32,

    /// The location of the fish within the fishing minigame.
    pub bar_velocity: f32,

    /// The height of the fish within the fishing minigame.
    pub bar_height: i32,

    /// A boolean, indicating whether the fish is currently within the
    /// fishing bar.
    pub bobber_in_bar: bool,

    /// A float, indicating how close the fish is to escaping or being
    /// caught.  At 0.0, the fish escapes.  At 1.0, the fish is
    /// caught.
    pub catch_progress: f32,

    /// True if the minigame has completed, and the bobber is
    /// currently being pulled from the water.
    pub pulling_out_of_water: bool,

    /// True if the minigame has completed, and the fish is currently
    /// being displayed above the player's head.
    pub showing_fish: bool,

    /// True if the minigame has completed, and the player is
    /// currently being shown the treasure that has been caught.
    pub showing_treasure: bool,
}

impl FishingState {
    pub(crate) fn read_all(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function(
            "new_fishing_state",
            |game_tick: i32,
             is_holding_rod: bool,
             is_timing_cast: bool,
             casting_power: f32,
             is_casting: bool,
             bobber_in_air: bool,
             is_fishing: bool,
             time_until_fishing_bite: f32,
             fishing_bite_accumulator: f32,
             is_nibbling: bool,
             time_until_fishing_nibble_done: f32,
             fishing_nibble_accumulator: f32,
             minigame_in_progress: bool,
             fish_difficulty: f32,
             fish_position: f32,
             fish_velocity: f32,
             fish_target_position: f32,
             bar_position: f32,
             bar_velocity: f32,
             bar_height: i32,
             bobber_in_bar: bool,
             catch_progress: f32,
             pulling_out_of_water: bool,
             showing_fish: bool,
             showing_treasure: bool| FishingState {
                game_tick,
                is_holding_rod,
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
            },
        )?;

        // TODO: Allow the parser to generate nan.
        graph.named_native_function("make_nan", || f32::NAN)?;

        let func = graph.parse(stringify! {
            fn read_fishing() {
                let nan = make_nan();

                let game_tick = StardewValley.Game1.ticks;

                let player = StardewValley
                    .Game1
                    ._player
                    .as::<StardewValley.Farmer>();

                let selected_index = player.currentToolIndex.value;
                let selected_item = player
                    .netItems.value.Items
                    .array.value.elements._items
                    [selected_index]
                    .value;
                let fishing_rod = selected_item
                    .as::<StardewValley.Tools.FishingRod>();

                let minigame = StardewValley
                    .Game1
                    ._activeClickableMenu
                    .as::<StardewValley.Menus.BobberBar>();

                let is_timing_cast = fishing_rod.is_some() && fishing_rod.isTimingCast;
                let casting_power = if fishing_rod.is_some() {
                    fishing_rod.castingPower
                } else {
                    nan
                };
                let is_casting = fishing_rod.is_some() && fishing_rod.isCasting;
                let bobber_in_air = fishing_rod.is_some() && fishing_rod.castedButBobberStillInAir;

                let is_fishing = fishing_rod.is_some() && fishing_rod.isFishing;

                let time_until_fishing_bite = if fishing_rod.is_some() {
                    fishing_rod.timeUntilFishingBite
                } else {
                    nan
                };
                let fishing_bite_accumulator = if fishing_rod.is_some() {
                    fishing_rod.fishingBiteAccumulator
                } else {
                    nan
                };

                let is_nibbling = fishing_rod.is_some() && fishing_rod.isNibbling;
                let time_until_fishing_nibble_done = if fishing_rod.is_some() {
                    fishing_rod.timeUntilFishingNibbleDone
                } else {
                    nan
                };
                let fishing_nibble_accumulator = if fishing_rod.is_some() {
                    fishing_rod.fishingNibbleAccumulator
                } else {
                    nan
                };

                let minigame_in_progress =
                    fishing_rod.is_some() && fishing_rod.isReeling;

                let fish_difficulty = if minigame.is_some() {
                    minigame.difficulty
                } else {
                    nan
                };
                let fish_position = if minigame.is_some() {
                    minigame.bobberPosition
                } else {
                    nan
                };
                let fish_velocity = if minigame.is_some() {
                    minigame.bobberSpeed
                } else {
                    nan
                };
                let fish_target_position = if minigame.is_some() {
                    minigame.bobberTargetPosition
                } else {
                    nan
                };
                let bar_position = if minigame.is_some() {
                    minigame.bobberBarPos
                } else {
                    nan
                };
                let bar_velocity = if minigame.is_some() {
                    minigame.bobberBarSpeed
                } else {
                    nan
                };
                let bar_height = if minigame.is_some() {
                    minigame.bobberBarHeight
                } else {
                    (0).prim_cast::<i32>()
                };
                let bobber_in_bar = minigame.is_some() && minigame.bobberInBar;
                let catch_progress = if minigame.is_some() {
                    minigame.distanceFromCatching
                } else {
                    nan
                };

                let pulling_out_of_water =
                    fishing_rod.is_some() && fishing_rod.pullingOutOfWater;

                let showing_fish = fishing_rod.is_some() && fishing_rod.fishCaught;

                let showing_treasure =
                    fishing_rod.is_some() && fishing_rod.showingTreasure;

                new_fishing_state(
                    game_tick,
                    fishing_rod.is_some(),
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
                )
            }
        })?;

        Ok(func)
    }
}

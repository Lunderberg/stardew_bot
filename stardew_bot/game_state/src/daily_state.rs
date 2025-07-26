use std::collections::HashSet;

use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};

use crate::Error;

#[derive(RustNativeObject, Debug, Clone)]
pub struct DailyState {
    pub daily_luck: f64,
    pub is_raining: bool,
    pub tomorrow_weather: String,
    pub professions: HashSet<i32>,
    pub tool_ready_for_pickup: bool,
}

impl DailyState {
    pub(crate) fn def_read_daily(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function(
            "new_daily_state",
            |daily_luck: f64,
             is_raining: bool,
             tomorrow_weather: &str,
             professions: &Vec<i32>,
             tool_ready_for_pickup: bool| {
                DailyState {
                    daily_luck,
                    is_raining,
                    tomorrow_weather: tomorrow_weather.into(),
                    professions: professions.iter().cloned().collect(),
                    tool_ready_for_pickup,
                }
            },
        )?;

        let func = graph.parse(stringify! {
            fn read_daily() {
                let daily_luck = StardewValley
                    .Game1
                    ._player
                    .teamRoot
                    .value
                    .sharedDailyLuck
                    .value;

                let is_raining = StardewValley
                    .Game1
                    .isRaining;

                let tomorrow_weather = StardewValley
                    .Game1
                    .weatherForTomorrow
                    .read_string();

                let professions = {
                    let set = StardewValley.Game1
                        ._player
                        .professions
                        .Set;
                    let num = set
                        ._count
                        .prim_cast::<usize>();
                    (0..num)
                        .map(|i| set._entries[i].Value)
                        .collect()
                };

                let tool_ready_for_pickup = {
                    let player = StardewValley.Game1._player;

                    let upgrade_in_progress = player
                        .toolBeingUpgraded
                        .value
                        .is_some();

                    let days_remaining = player
                        .daysLeftForToolUpgrade
                        .value;

                    upgrade_in_progress && (days_remaining == 0i32)
                };

                new_daily_state(
                    daily_luck,
                    is_raining,
                    tomorrow_weather,
                    professions,
                    tool_ready_for_pickup,
                )
            }
        })?;

        Ok(func)
    }

    pub fn fish_price_multiplier(&self) -> f32 {
        if self.professions.contains(&8) {
            1.5
        } else if self.professions.contains(&6) {
            1.25
        } else {
            1.0
        }
    }
}

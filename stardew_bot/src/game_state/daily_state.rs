use std::collections::HashSet;

use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};

use crate::Error;

#[derive(RustNativeObject, Debug, Clone)]
pub struct DailyState {
    pub daily_luck: f64,
    pub is_raining: bool,
    pub tomorrow_weather: String,
    pub professions: HashSet<i32>,
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
             professions: &Vec<i32>| {
                DailyState {
                    daily_luck,
                    is_raining,
                    tomorrow_weather: tomorrow_weather.into(),
                    professions: professions.iter().cloned().collect(),
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

                new_daily_state(
                    daily_luck,
                    is_raining,
                    tomorrow_weather,
                    professions,
                )
            }
        })?;

        Ok(func)
    }
}

use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};

use crate::Error;

#[derive(RustNativeObject, Debug, Clone)]
pub struct DailyState {
    pub daily_luck: f64,
    pub is_raining: bool,
    pub tomorrow_weather: String,
}

impl DailyState {
    pub(crate) fn def_read_daily(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function(
            "new_daily_state",
            |daily_luck: f64, is_raining: bool, tomorrow_weather: &str| {
                DailyState {
                    daily_luck,
                    is_raining,
                    tomorrow_weather: tomorrow_weather.into(),
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

                new_daily_state(
                    daily_luck,
                    is_raining,
                    tomorrow_weather,
                )
            }
        })?;

        Ok(func)
    }
}

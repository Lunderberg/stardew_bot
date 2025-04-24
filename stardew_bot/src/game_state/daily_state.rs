use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};

use crate::Error;

#[derive(RustNativeObject, Debug, Clone)]
pub struct DailyState {
    pub daily_luck: f64,
    pub tomorrow_weather: String,
}

impl DailyState {
    pub(crate) fn read_all(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function(
            "new_daily_state",
            |daily_luck: f64, tomorrow_weather: &str| DailyState {
                daily_luck,
                tomorrow_weather: tomorrow_weather.into(),
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

                let tomorrow_weather = StardewValley
                    .Game1
                    .weatherForTomorrow
                    .read_string();

                new_daily_state(daily_luck, tomorrow_weather)
            }
        })?;

        Ok(func)
    }
}

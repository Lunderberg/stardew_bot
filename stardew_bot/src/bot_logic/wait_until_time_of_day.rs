use crate::{Error, GameAction, GameState};

use super::bot_logic::{BotGoal, BotGoalResult};

pub struct WaitUntilTimeOfDay {
    /// Uses the same interpretation as
    /// `GlobalGameState.in_game_time`.
    time_of_day: i32,
}

impl WaitUntilTimeOfDay {
    pub fn new(time_of_day: i32) -> Self {
        Self { time_of_day }
    }
}

impl BotGoal for WaitUntilTimeOfDay {
    fn description(&self) -> std::borrow::Cow<str> {
        format!(
            "Wait until {:02}:{:02}",
            self.time_of_day / 100,
            self.time_of_day % 100
        )
        .into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _: &mut dyn FnMut(GameAction),
    ) -> Result<BotGoalResult, Error> {
        Ok(if game_state.globals.in_game_time < self.time_of_day {
            BotGoalResult::InProgress
        } else {
            BotGoalResult::Completed
        })
    }
}

use game_state::{GameState, Timestamp};

use crate::Error;

use super::bot_logic::{ActionCollector, BotGoal, BotGoalResult};

pub struct WaitUntilTimeOfDay {
    /// Uses the same interpretation as
    /// `GlobalGameState.in_game_time`.
    time_of_day: Timestamp,
}

impl WaitUntilTimeOfDay {
    pub fn new(time_of_day: Timestamp) -> Self {
        Self { time_of_day }
    }
}

impl BotGoal for WaitUntilTimeOfDay {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        format!("Wait until {}", self.time_of_day).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        Ok(if game_state.globals.in_game_time < self.time_of_day {
            BotGoalResult::InProgress
        } else {
            BotGoalResult::Completed
        })
    }
}

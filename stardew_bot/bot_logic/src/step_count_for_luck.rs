use geometry::Direction;

use crate::{
    predictors::StepCountPredictor, Error, GameAction, StopMovingGoal,
};
use game_state::GameState;

use super::{
    bot_logic::{
        ActionCollector, BotGoal, BotGoalResult, BotInterrupt, LogicStack,
        LogicStackItem,
    },
    LocationExt as _,
};

#[derive(Clone)]
pub struct StepCountForLuck {
    /// The time in the day when the steps should start being checked.
    /// Defaults to 2500 (1:00 AM).
    start_time: i32,

    /// The maximum number of steps that may be taken to manipulate
    /// the step counter.
    ///
    /// While in principle, we could always take enough steps that the
    /// day will be guaranteed to have the best possible luck (+100 on
    /// an RNG roll from -100 to +100), taking that many steps may
    /// require taking more steps than there is time to do.
    /// Therefore, select the best possible luck that can be achieved
    /// within the next `max_lookahead` steps.
    max_lookahead: usize,

    /// The target number of steps when the day ends.
    ///
    /// When the time reaches `start_time`, this value is decided
    /// based on the current number of steps taken.
    target_steps: Option<u32>,
}

impl StepCountForLuck {
    pub fn new() -> Self {
        Self {
            start_time: 2500,
            max_lookahead: 30,
            target_steps: None,
        }
    }

    #[allow(dead_code)]
    pub fn start_time(self, start_time: i32) -> Self {
        Self { start_time, ..self }
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        predict_daily_luck(game_state, 0) > 90
    }

    fn plan_steps(&self, game_state: &GameState) -> Result<u32, Error> {
        let additional_steps = (0..(self.max_lookahead as u32))
            .max_by_key(|additional_steps| {
                predict_daily_luck(game_state, *additional_steps)
            })
            .unwrap();

        let steps_taken: u32 =
            game_state.globals.get_stat("stepsTaken").unwrap_or(0);

        Ok(steps_taken + additional_steps)
    }

    fn reached_lucky_step_count(
        &mut self,
        game_state: &GameState,
    ) -> Result<bool, Error> {
        if self.target_steps.is_none() {
            self.target_steps = Some(self.plan_steps(game_state)?);
        }

        let target_steps =
            self.target_steps.expect("Just populated self.target_steps");

        let steps_taken: u32 =
            game_state.globals.get_stat("stepsTaken").unwrap_or(0);

        assert!(
            steps_taken <= target_steps,
            "TODO: Re-plan target steps after overshooting.  \
             Better yet, avoid taking too many steps \
             in the first place."
        );

        Ok(steps_taken == target_steps)
    }
}

impl BotInterrupt for StepCountForLuck {
    fn description(&self) -> std::borrow::Cow<str> {
        if let Some(steps) = self.target_steps {
            format!("Stop at {steps} steps").into()
        } else {
            format!("Wait until {}, then step-count", self.start_time).into()
        }
    }

    fn check(
        &mut self,
        game_state: &GameState,
    ) -> Result<Option<LogicStack>, Error> {
        if game_state.globals.in_game_time < self.start_time {
            return Ok(None);
        }

        if !self.reached_lucky_step_count(game_state)? {
            if game_state.globals.in_game_time >= 2530 {
                // Running out of time, so the bot may be doing an
                // activity that doesn't produce many steps.  Switch
                // to a more active step-counting, to ensure that the
                // day doesn't end on the wrong step count.
                return Ok(Some(self.clone().into()));
            }
            // Still have more steps to take before reaching target
            // step count.
            return Ok(None);
        }

        if game_state.player.movement.is_none() {
            // Not currently moving, no need to change anything.
            return Ok(None);
        }

        // The player is currently moving, and has alreaady reached
        // the step count.
        Ok(Some(
            [LogicStackItem::PreventInterrupt, StopMovingGoal.into()]
                .into_iter()
                .collect(),
        ))
    }
}

impl BotGoal for StepCountForLuck {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Actively walk for luck".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        if self.reached_lucky_step_count(game_state)? {
            return Ok(BotGoalResult::Completed);
        }

        let walkable = game_state
            .current_room()?
            .pathfinding(&game_state.statics)
            .walkable();

        let player_tile = game_state.player.tile();
        let blocked_dir = Direction::iter_cardinal()
            .min_by_key(|dir| {
                let offset = dir.offset();
                (0..)
                    .map(|dist| player_tile + offset * dist)
                    .take_while(|tile| walkable.is_set(*tile))
                    .count()
            })
            .expect("At least one direction will be minimum");

        actions.do_action(GameAction::Move(blocked_dir.into()));
        Ok(BotGoalResult::InProgress)
    }
}

/// Predict the daily luck, based on the current game state.
///
/// Returns the luck as `i8`, which will always be on the range
/// [-100,100], inclusive.
pub fn predict_daily_luck(game_state: &GameState, additional_steps: u32) -> i8 {
    StepCountPredictor::new(game_state)
        .predict(additional_steps)
        .daily_luck
}

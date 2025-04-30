use std::borrow::Cow;

use crate::{
    game_state::{FacingDirection, Vector},
    Error, GameAction, GameState,
};

use super::{
    bot_logic::{BotGoal, BotGoalResult, SubGoals},
    movement_goal::{FaceDirectionGoal, MovementGoal},
};

pub struct FishingGoal;

pub struct FishOnceGoal;

impl BotGoal for FishingGoal {
    fn apply(&mut self, _: &GameState) -> Result<BotGoalResult, Error> {
        let goals = SubGoals::new()
            .then(MovementGoal::new("Forest".into(), Vector::new(70.0, 50.4)))
            .then(FaceDirectionGoal(FacingDirection::South))
            .then(FishOnceGoal);
        Ok(goals.into())
    }

    fn description(&self) -> Cow<str> {
        "River fishing".into()
    }
}

impl BotGoal for FishOnceGoal {
    fn apply(
        &mut self,
        game_state: &GameState,
    ) -> Result<BotGoalResult, Error> {
        let fishing = &game_state.fishing;

        let action = if fishing.is_timing_cast {
            if fishing.casting_power > 0.95 {
                GameAction::ReleaseTool
            } else {
                GameAction::HoldTool
            }
        } else if fishing.minigame_in_progress {
            if fishing.should_move_upward() {
                GameAction::HoldTool
            } else {
                GameAction::ReleaseTool
            }
        } else if fishing.is_nibbling {
            GameAction::ReleaseTool
        } else if fishing.is_fishing {
            GameAction::HoldTool
        } else if fishing.showing_fish {
            return Ok(BotGoalResult::Completed);
        } else if fishing.is_holding_rod {
            GameAction::HoldTool
        } else {
            GameAction::Wait
        };

        Ok(BotGoalResult::Action(action))
    }

    fn description(&self) -> Cow<str> {
        "Fish once".into()
    }
}

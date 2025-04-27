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
            .then(MovementGoal::new("Forest", Vector::new(70.0, 50.4)))
            .then(FaceDirectionGoal(FacingDirection::South))
            .then(FishOnceGoal);
        Ok(goals.into())
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
                Some(GameAction::ReleaseTool)
            } else {
                Some(GameAction::HoldTool)
            }
        } else if fishing.minigame_in_progress {
            if fishing.should_move_upward() {
                Some(GameAction::HoldTool)
            } else {
                Some(GameAction::ReleaseTool)
            }
        } else if fishing.is_nibbling {
            Some(GameAction::ReleaseTool)
        } else if fishing.is_fishing {
            Some(GameAction::HoldTool)
        } else if fishing.showing_fish {
            return Ok(BotGoalResult::Completed);
        } else if fishing.is_holding_rod {
            Some(GameAction::HoldTool)
        } else {
            None
        };

        Ok(BotGoalResult::Action(action))
    }
}

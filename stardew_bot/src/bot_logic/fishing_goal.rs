use std::borrow::Cow;

use crate::{
    game_state::{FacingDirection, Item, Vector},
    Error, GameAction, GameState,
};

use super::{
    bot_logic::{BotGoal, BotGoalResult, LogicStack},
    movement_goal::{FaceDirectionGoal, MovementGoal},
};

pub struct FishingGoal;

pub struct FishOnceGoal;

impl BotGoal for FishingGoal {
    fn apply(
        &mut self,
        _: &GameState,
        _: &mut dyn FnMut(GameAction),
    ) -> Result<BotGoalResult, Error> {
        let goals = LogicStack::new()
            // .then(super::InventoryGoal::new(Item::new("(T)BambooPole")))
            .then(MovementGoal::new("Forest", Vector::new(70.0, 50.4)))
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
        do_action: &mut dyn FnMut(GameAction),
    ) -> Result<BotGoalResult, Error> {
        let fishing = &game_state.fishing;

        let mut state = BotGoalResult::InProgress;

        if fishing.is_timing_cast {
            if fishing.casting_power > 0.95 {
                do_action(GameAction::ReleaseTool)
            } else {
                do_action(GameAction::HoldTool)
            }
        } else if fishing.minigame_in_progress {
            if fishing.should_move_upward() {
                do_action(GameAction::HoldTool)
            } else {
                do_action(GameAction::ReleaseTool)
            }
        } else if fishing.is_nibbling {
            do_action(GameAction::ReleaseTool)
        } else if fishing.is_fishing {
            do_action(GameAction::HoldTool)
        } else if fishing.showing_fish {
            state = BotGoalResult::Completed;
        } else if fishing.is_holding_rod {
            do_action(GameAction::HoldTool)
        }

        Ok(state)
    }

    fn description(&self) -> Cow<str> {
        "Fish once".into()
    }
}

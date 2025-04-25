use crate::{
    game_state::{FacingDirection, Vector},
    Error, GameState,
};

use super::{
    bot_logic::{BotGoal, BotGoalResult, SubGoals},
    movement_goal::{FaceDirectionGoal, MoveToLocationGoal},
};

pub struct FishingGoal;

pub struct FishOnceGoal;

impl BotGoal for FishingGoal {
    fn apply(&mut self, _: &GameState) -> Result<BotGoalResult, Error> {
        let goals = SubGoals::new()
            .then(MoveToLocationGoal::new("Forest", Vector::new(70.2, 50.5)))
            .then(FaceDirectionGoal(FacingDirection::South))
            .then(FishOnceGoal);
        Ok(goals.into())
    }
}

impl BotGoal for FishOnceGoal {
    fn apply(&mut self, _: &GameState) -> Result<BotGoalResult, Error> {
        todo!()
    }
}

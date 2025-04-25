use std::any::Any;

use crate::{Error, GameAction, GameState};

use super::fishing_goal::FishingGoal;

pub struct BotLogic {
    goals: Vec<Box<dyn BotGoal>>,
}

pub trait BotGoal: Any {
    fn apply(&mut self, game_state: &GameState)
        -> Result<BotGoalResult, Error>;
}

pub enum BotGoalResult {
    /// This goal has completed.  It can be popped from the stack, to
    /// resume the previous goal.
    Completed,

    /// To accomplish this goal, the subgoal should first be
    /// completed.
    SubGoals(SubGoals),

    /// This goal has a direct action to accomplish the goal.
    Action(Option<GameAction>),
}

pub struct SubGoals(Vec<Box<dyn BotGoal>>);

impl BotLogic {
    pub fn new() -> Self {
        Self {
            goals: vec![Box::new(FishingGoal)],
        }
    }

    pub fn update(
        &mut self,
        game_state: &GameState,
    ) -> Result<Option<GameAction>, Error> {
        loop {
            let current_goal =
                self.goals.last_mut().ok_or(Error::NoRemainingGoals)?;

            let goal_result = current_goal.apply(game_state)?;

            match goal_result {
                BotGoalResult::Completed => {
                    self.goals.pop();
                }
                BotGoalResult::SubGoals(sub_goals) => {
                    sub_goals
                        .0
                        .into_iter()
                        .rev()
                        .for_each(|sub_goal| self.goals.push(sub_goal));
                }
                BotGoalResult::Action(game_action) => {
                    return Ok(game_action);
                }
            }
        }
    }

    pub fn current_goal(&self) -> Option<&dyn BotGoal> {
        self.goals.last().map(|v| &**v)
    }
}

impl SubGoals {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn then(mut self, goal: impl BotGoal + 'static) -> Self {
        self.0.push(Box::new(goal));
        self
    }
}

impl From<SubGoals> for BotGoalResult {
    fn from(subgoals: SubGoals) -> Self {
        Self::SubGoals(subgoals)
    }
}

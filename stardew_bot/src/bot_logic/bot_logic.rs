use std::{any::Any, borrow::Cow};

use crate::{game_state::Item, Error, GameAction, GameState};

pub struct BotLogic {
    goals: Vec<Box<dyn BotGoal>>,
    verbose: bool,
}

pub trait BotGoal: Any {
    fn description(&self) -> Cow<str>;

    fn apply(
        &mut self,
        game_state: &GameState,
        do_action: &mut dyn FnMut(GameAction),
    ) -> Result<BotGoalResult, Error>;
}

pub enum BotGoalResult {
    /// This goal has completed.  It can be popped from the stack, to
    /// resume the previous goal.
    Completed,

    /// To accomplish this goal, the subgoal should first be
    /// completed.
    SubGoals(SubGoals),

    /// This goal is in-progress, and should remain at the top of the
    /// stack of goals.
    InProgress,
}

pub struct SubGoals(Vec<Box<dyn BotGoal>>);

impl BotLogic {
    pub fn new(verbose: bool) -> Self {
        Self {
            goals: vec![
                // Box::new(super::ClearFarmGoal),
                // Box::new(super::ClayFarmingGoal::new()),
                // Box::new(super::GoToActionTile::new("Carpenter")),
                Box::new(super::FishingGoal),
                Box::new(super::FirstDay),
            ],
            verbose,
        }
    }

    pub fn update(
        &mut self,
        game_state: &GameState,
    ) -> Result<Vec<GameAction>, Error> {
        let mut actions = Vec::new();
        loop {
            let current_goal =
                self.goals.last_mut().ok_or(Error::NoRemainingGoals)?;

            if self.verbose {
                println!("Running top goal '{}'", current_goal.description());
            }
            let goal_result = current_goal
                .apply(game_state, &mut |action| actions.push(action))?;

            match goal_result {
                BotGoalResult::Completed => {
                    if self.verbose {
                        println!("\tGoal completed, removing from stack",);
                    }

                    if let Some(prev) = previously_produced_subgoals {
                        assert!(
                            self.goals.len() > prev + 2,
                            "Infinite loop detected.  \
                             Current goal '{0}' has completed, \
                             but this returns control to the preceding goal '{1}'.  \
                             Since this preceding goal '{1}' has already been run, \
                             and chose to delegate to '{0}', \
                             executing it again would enter an infinite loop.",
                            self.goals.last().unwrap().description(),
                            self.goals[prev].description(),
                        );
                    }

                    self.goals.pop();
                }
                BotGoalResult::SubGoals(sub_goals) => {
                    previously_produced_subgoals = Some(self.goals.len() - 1);
                    sub_goals.0.into_iter().rev().for_each(|sub_goal| {
                        if self.verbose {
                            println!(
                                "\tGoal requires sub-goal '{}', \
                                 pushing sub-goal to top of stack.",
                                sub_goal.description(),
                            );
                        }
                        self.goals.push(sub_goal);
                    });
                }
                BotGoalResult::InProgress => {
                    if self.verbose {
                        println!(
                            "\tGoal is in progress, \
                             waiting until next frame"
                        );
                    }
                    break;
                }
            }
        }

        Ok(actions)
    }

    pub fn iter_goals(
        &self,
    ) -> impl DoubleEndedIterator<Item = &dyn BotGoal> + '_ {
        self.goals.iter().map(AsRef::as_ref)
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

impl<Goal> FromIterator<Goal> for SubGoals
where
    Goal: BotGoal + 'static,
{
    fn from_iter<T: IntoIterator<Item = Goal>>(iter: T) -> Self {
        let mut goals = SubGoals::new();
        for goal in iter {
            goals = goals.then(goal);
        }
        goals
    }
}

impl From<SubGoals> for BotGoalResult {
    fn from(subgoals: SubGoals) -> Self {
        Self::SubGoals(subgoals)
    }
}

impl<T> From<T> for SubGoals
where
    T: BotGoal,
{
    fn from(value: T) -> Self {
        SubGoals::new().then(value)
    }
}

impl<T> From<T> for BotGoalResult
where
    T: BotGoal,
{
    fn from(goal: T) -> Self {
        BotGoalResult::SubGoals(goal.into())
    }
}

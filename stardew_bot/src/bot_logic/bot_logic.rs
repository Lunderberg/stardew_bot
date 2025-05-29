use derive_more::From;
use std::{any::Any, borrow::Cow};

use crate::{
    game_state::{Item, Key},
    Error, GameAction, GameState,
};

pub struct BotLogic {
    stack: Vec<LogicStackItem>,
    verbose: bool,
}

#[derive(From)]
enum LogicStackItem {
    Goal(Box<dyn BotGoal>),
    Interrupt(Box<dyn Fn(&GameState) -> bool>),
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

pub struct SubGoals {
    goals: Vec<Box<dyn BotGoal>>,
    interrupt: Option<Box<dyn Fn(&GameState) -> bool>>,
}

impl BotLogic {
    pub fn new(verbose: bool) -> Self {
        Self {
            stack: vec![
                // Box::new(super::ClearFarmGoal),
                // Box::new(super::ClayFarmingGoal::new()),
                // Box::new(super::GoToActionTile::new("Carpenter")),
                LogicStackItem::Goal(Box::new(super::FishingGoal)),
                LogicStackItem::Goal(Box::new(super::FirstDay)),
            ],
            verbose,
        }
    }

    pub fn update(
        &mut self,
        game_state: &GameState,
    ) -> Result<Vec<GameAction>, Error> {
        let mut actions = Vec::new();

        let is_activating_tile =
            game_state.inputs.keys_pressed.contains(&Key::X);
        let is_confirming_menu =
            game_state.inputs.keys_pressed.contains(&Key::Y);
        let is_animation_cancelling =
            game_state.inputs.keys_pressed.iter().any(|key| {
                matches!(key, Key::Delete | Key::RightShift | Key::R)
            });
        let is_exiting_menu =
            game_state.inputs.keys_pressed.contains(&Key::Escape);

        let mut on_goal_action = |action| {
            // Some actions, such as moving the character around, are
            // continuous, while others are instantaneous.  For an
            // instantaneous action, Stardew checks whether the
            // button/key are in a different state than they were on
            // the previous frame.  Therefore, even if we were to send
            // the input, it wouldn't cause the game action to be
            // repeated.
            //
            // TL;DR: Don't press the mouse down unless the game will
            // recognize it as pressing the mouse down.
            let should_perform = match action {
                GameAction::LeftClick => !game_state.inputs.left_mouse_down(),
                GameAction::RightClick => !game_state.inputs.right_mouse_down(),
                GameAction::AnimationCancel => !is_animation_cancelling,
                GameAction::ActivateTile => !is_activating_tile,
                GameAction::ConfirmMenu => !is_confirming_menu,
                GameAction::ExitMenu => !is_exiting_menu,
                _ => true,
            };
            if should_perform {
                actions.push(action);
            }
        };

        let mut previously_produced_subgoals: Option<usize> = None;

        loop {
            // Check if any interrupts should be triggered.  If so,
            // everything above that position in the stack gets
            // removed.
            let opt_interrupt = self
                .stack
                .iter()
                .enumerate()
                .find(|(_, item)| match item {
                    LogicStackItem::Interrupt(cond) => cond(game_state),
                    LogicStackItem::Goal(_) => false,
                })
                .map(|(i, _)| i);
            if let Some(interrupt) = opt_interrupt {
                if self.verbose {
                    println!(
                        "Interrupt {interrupt} triggered, \
                         removing all goals above it."
                    )
                }
                self.stack.shrink_to(interrupt);
            }

            // Any interrupts on the top of the stack no longer have
            // anything remaining that they can interrupt.
            while self
                .stack
                .last()
                .map(|item| matches!(item, LogicStackItem::Interrupt(_)))
                .unwrap_or(false)
            {
                self.stack.pop();
            }

            let current_goal: &mut dyn BotGoal = self
                .stack
                .iter_mut()
                .filter_map(|item| match item {
                    LogicStackItem::Goal(bot_goal) => Some(bot_goal.as_mut()),
                    LogicStackItem::Interrupt(_) => None,
                })
                .last()
                .ok_or(Error::NoRemainingGoals)?;

            if self.verbose {
                println!("Running top goal '{}'", current_goal.description());
            }

            let goal_result =
                current_goal.apply(game_state, &mut on_goal_action)?;

            match goal_result {
                BotGoalResult::Completed => {
                    if self.verbose {
                        println!("\tGoal completed, removing from stack",);
                    }

                    if let Some(prev) = previously_produced_subgoals {
                        assert!(
                            self.stack.len() > prev + 2,
                            "Infinite loop detected.  \
                             Current goal '{0}' has completed, \
                             but this returns control to the preceding goal '{1}'.  \
                             Since this preceding goal '{1}' has already been run, \
                             and chose to delegate to '{0}', \
                             executing it again would enter an infinite loop.",
                            self.current_goal().unwrap().description(),
                            match &self.stack[prev] {
                                LogicStackItem::Goal(prev_goal) => prev_goal.description(),
                                _ => unreachable!("Contained a BotGoal first time around")
                            }
                        );
                    }

                    self.stack.pop();
                }
                BotGoalResult::SubGoals(sub_goals) => {
                    previously_produced_subgoals = Some(self.stack.len() - 1);

                    if let Some(interrupt) = sub_goals.interrupt {
                        self.stack.push(interrupt.into());
                    }

                    sub_goals.goals.into_iter().rev().for_each(|sub_goal| {
                        if self.verbose {
                            println!(
                                "\tGoal requires sub-goal '{}', \
                                 pushing sub-goal to top of stack.",
                                sub_goal.description(),
                            );
                        }
                        self.stack.push(sub_goal.into());
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

        // Reset mouse/keyboard state back to their unpressed state.
        //
        // For buttons/keys that have an instant effect, such as
        // left-clicking, the button/key is released as soon as it is
        // recognized as being pressed down, so that it can be ready
        // to be pressed down again.
        //
        // For buttons/keys that have a continuous effect, such as
        // moving, the button/key remains pressed down as long as the
        // `BotGoal` keeps producing `GameAction::Move` commands.
        if game_state.inputs.left_mouse_down() {
            actions.push(GameAction::ReleaseLeftClick.into());
        }
        if game_state.inputs.right_mouse_down() {
            actions.push(GameAction::ReleaseRightClick.into());
        }
        if is_animation_cancelling {
            actions.push(GameAction::StopAnimationCanceling);
        }
        if is_activating_tile {
            actions.push(GameAction::StopActivatingTile);
        }
        if is_confirming_menu {
            actions.push(GameAction::StopConfirmingMenu);
        }
        if is_exiting_menu {
            actions.push(GameAction::StopExitingMenu);
        }
        if game_state.player.movement.is_some()
            && actions
                .iter()
                .all(|action| !matches!(action, GameAction::Move(_)))
        {
            actions.push(GameAction::StopMoving);
        }

        Ok(actions)
    }

    pub fn iter_goals(
        &self,
    ) -> impl DoubleEndedIterator<Item = &dyn BotGoal> + '_ {
        self.stack.iter().filter_map(|item| match item {
            LogicStackItem::Goal(bot_goal) => Some(bot_goal.as_ref()),
            LogicStackItem::Interrupt(_) => None,
        })
    }

    pub fn current_goal(&self) -> Option<&dyn BotGoal> {
        self.iter_goals().last()
    }
}

impl SubGoals {
    pub fn new() -> Self {
        Self {
            goals: Vec::new(),
            interrupt: None,
        }
    }

    pub fn then(mut self, goal: impl BotGoal + 'static) -> Self {
        self.goals.push(Box::new(goal));
        self
    }

    pub fn with_interrupt(
        mut self,
        condition: impl Fn(&GameState) -> bool + 'static,
    ) -> Self {
        self.interrupt = Some(Box::new(condition));
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

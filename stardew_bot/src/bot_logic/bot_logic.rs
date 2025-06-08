use derive_more::From;
use std::{any::Any, borrow::Cow, collections::VecDeque};

use crate::{
    game_state::{Item, Key},
    Error, GameAction, GameState,
};

pub struct BotLogic {
    stack: Vec<LogicStackItem>,
    verbose: bool,
}

pub struct LogicStack(VecDeque<LogicStackItem>);

#[derive(From)]
pub enum LogicStackItem {
    /// A goal for the bot to achieve.  Only the top-most goal is ever
    /// active.
    Goal(Box<dyn BotGoal>),

    /// A condition required to continue running subgoals
    ///
    /// This condition is checked for each update, regardless of
    /// position on the stack.  If the condition returns true, all
    /// items above it on the stack are immediately canceled.
    CancelIf(Box<dyn Fn(&GameState) -> bool>),

    /// An interrupt that may push a new goal to the top of the stack,
    /// pre-empting an in-progress goal.
    ///
    /// An interrupt may only have a single goal on the stack at a
    /// time.  Until the interrupt's goal has been completed, the
    /// interrupt may not fire again.
    Interrupt {
        interrupt: Box<dyn BotInterrupt>,
        active_goal: Option<usize>,
    },
}

pub trait BotGoal: Any {
    fn description(&self) -> Cow<str>;

    fn apply(
        &mut self,
        game_state: &GameState,
        do_action: &mut dyn FnMut(GameAction),
    ) -> Result<BotGoalResult, Error>;

    fn cancel_if(
        self,
        condition: impl Fn(&GameState) -> bool + 'static,
    ) -> LogicStack
    where
        Self: Sized,
    {
        let cancellation = LogicStackItem::CancelIf(Box::new(condition));
        [cancellation, self.into()].into_iter().collect()
    }
}

pub enum BotGoalResult {
    /// This goal has completed.  It can be popped from the stack, to
    /// resume the previous goal.
    Completed,

    /// To accomplish this goal, the subgoal should first be
    /// completed.
    SubGoals(LogicStack),

    /// This goal is in-progress, and should remain at the top of the
    /// stack of goals.
    InProgress,
}

pub trait BotInterrupt: Any {
    fn description(&self) -> Cow<str>;

    fn check(
        &mut self,
        game_state: &GameState,
    ) -> Result<Option<Box<dyn BotGoal>>, Error>;
}

impl BotLogic {
    pub fn new(verbose: bool) -> Self {
        Self {
            stack: vec![
                // LogicStackItem::Goal(Box::new(super::ClearFarmGoal)),
                // LogicStackItem::Goal(Box::new(super::ClayFarmingGoal::new())),
                // LogicStackItem::Goal(Box::new(super::GoToActionTile::new(
                //     "Carpenter",
                // ))),
                LogicStackItem::Goal(Box::new(super::FishingGoal)),
                LogicStackItem::Interrupt {
                    interrupt: Box::new(super::StepCountForLuck::new()),
                    active_goal: None,
                },
                LogicStackItem::Goal(Box::new(super::GenericDay)),
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
            // Check if any CancelIf conditions should be triggered.
            // If so, everything above that position in the stack gets
            // removed.
            let opt_cancellation = self
                .stack
                .iter()
                .enumerate()
                .find(|(_, item)| match item {
                    LogicStackItem::CancelIf(cond) => cond(game_state),
                    LogicStackItem::Goal(_)
                    | LogicStackItem::Interrupt { .. } => false,
                })
                .map(|(i, _)| i);
            if let Some(interrupt) = opt_cancellation {
                if self.verbose {
                    println!(
                        "Interrupt {interrupt} triggered, \
                         removing all goals above it."
                    )
                }
                self.stack.truncate(interrupt);
            }

            // Any cancellations or interrupts on the top of the stack no longer
            // have anything remaining that they can cancel/interrupt.
            while self
                .stack
                .last()
                .map(|item| {
                    matches!(
                        item,
                        LogicStackItem::CancelIf(_)
                            | LogicStackItem::Interrupt { .. }
                    )
                })
                .unwrap_or(false)
            {
                self.stack.pop();
            }

            // If anything was removed, mark interrupts that may be
            // triggered again.
            self.reset_completed_interrupts();

            // Check if any interrupts should push a new goal onto the
            // top of the stack.
            for i_item in (0..self.stack.len()).rev() {
                let current_stack_size = self.stack.len();
                let LogicStackItem::Interrupt {
                    interrupt,
                    active_goal,
                } = &mut self.stack[i_item]
                else {
                    continue;
                };
                if active_goal.is_some() {
                    continue;
                }
                let Some(new_goal) = interrupt.check(game_state)? else {
                    continue;
                };

                *active_goal = Some(current_stack_size);
                self.stack.push(new_goal.into());
            }

            let current_goal: &mut dyn BotGoal = self
                .stack
                .iter_mut()
                .filter_map(|item| match item {
                    LogicStackItem::Goal(bot_goal) => Some(bot_goal.as_mut()),
                    LogicStackItem::CancelIf(_)
                    | LogicStackItem::Interrupt { .. } => None,
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
                    self.reset_completed_interrupts();
                }
                BotGoalResult::SubGoals(sub_goals) => {
                    previously_produced_subgoals = Some(self.stack.len() - 1);

                    sub_goals
                        .0
                        .into_iter()
                        .inspect(|item| {
                            if self.verbose {
                                println!(
                                    "\tGoal requires sub-goal '{}', \
                                     pushing to top of stack.",
                                    item.description()
                                );
                            }
                        })
                        .for_each(|item| self.stack.push(item));
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

    fn reset_completed_interrupts(&mut self) {
        let stack_size = self.stack.len();
        for item in &mut self.stack {
            if let LogicStackItem::Interrupt { active_goal, .. } = item {
                let should_reset = active_goal
                    .map(|index| index >= stack_size)
                    .unwrap_or(false);
                if should_reset {
                    *active_goal = None;
                }
            }
        }
    }

    pub fn iter(
        &self,
    ) -> impl DoubleEndedIterator<Item = &LogicStackItem> + '_ {
        self.stack.iter()
    }

    pub fn current_goal(&self) -> Option<&dyn BotGoal> {
        self.stack.iter().rev().find_map(|item| match item {
            LogicStackItem::Goal(bot_goal) => Some(bot_goal.as_ref()),
            _ => None,
        })
    }
}

impl LogicStack {
    pub fn new() -> Self {
        Self(Default::default())
    }

    pub fn then(mut self, goal: impl BotGoal + 'static) -> Self {
        // Push onto the beginning of the stack, such that the new
        // goal will be the last item executed.
        self.0.push_front(goal.into());
        self
    }

    pub fn cancel_if(
        mut self,
        condition: impl Fn(&GameState) -> bool + 'static,
    ) -> Self {
        self.0
            .push_front(LogicStackItem::CancelIf(Box::new(condition)));
        self
    }
}

impl LogicStackItem {
    pub fn description(&self) -> Cow<str> {
        match self {
            LogicStackItem::Goal(goal) => goal.description(),
            LogicStackItem::CancelIf(_) => "CancelIf".into(),
            LogicStackItem::Interrupt { interrupt, .. } => {
                interrupt.description()
            }
        }
    }
}

impl<Item> FromIterator<Item> for LogicStack
where
    Item: Into<LogicStackItem>,
{
    fn from_iter<Iter: IntoIterator<Item = Item>>(iter: Iter) -> Self {
        let stack = iter.into_iter().map(Into::into).collect();
        Self(stack)
    }
}

impl From<LogicStack> for BotGoalResult {
    fn from(subgoals: LogicStack) -> Self {
        Self::SubGoals(subgoals)
    }
}

impl<T> From<T> for LogicStack
where
    T: BotGoal,
{
    fn from(value: T) -> Self {
        LogicStack::new().then(value)
    }
}

impl<T> From<T> for LogicStackItem
where
    T: BotGoal,
{
    fn from(goal: T) -> Self {
        LogicStackItem::Goal(Box::new(goal))
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

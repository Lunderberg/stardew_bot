use derive_more::From;
use itertools::Itertools as _;
use std::{
    any::Any,
    borrow::Cow,
    collections::{HashSet, VecDeque},
    fmt::Display,
};

use crate::{Error, GameAction};
use game_state::{GameState, Key, ScrollWheel};

const MAX_RECENT_GOALS: usize = 30;
const MAX_RECENT_ACTIONS: usize = 1000;

pub struct BotLogic {
    stack: Vec<LogicStackItem>,
    action_dead_time: ActionForbiddenUntil,
    recently_finished: VecDeque<LogicStackItem>,
    recent_actions: VecDeque<VerboseAction>,
    verbose: bool,
    recursed_during_current_update: HashSet<usize>,
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

    /// Prevent any interrupts from lower on the stack from firing.
    /// This should be used very sparingly, and only in cases where
    /// the goals will be completed within a few frames.
    ///
    /// For example, the `StepCountForLuck` interrupt stops the
    /// player's motion, preventing additional steps from being taken.
    /// This interrupt should not itself be interrupted, as that could
    /// cause additional steps to be taken, breaking the RNG
    /// manipulation.  Therefore, the `StepCountForLuck` interrupt
    /// pushes a `PreventInterrupt` item as well as a
    /// `StopMovingGoal`.
    PreventInterrupt,
}

pub struct ActionCollector {
    actions: Vec<VerboseAction>,
    refresh_current_location: bool,

    forbidden: ActionForbiddenUntil,

    game_mode_tick: i32,

    is_activating_tile: bool,
    is_confirming_menu: bool,
    is_animation_cancelling: bool,
    is_exiting_menu: bool,
    is_holding_tool: bool,
    is_moving: bool,
    is_scrolling_up: bool,
    is_scrolling_down: bool,
    is_left_clicking: bool,
    is_right_clicking: bool,
    is_holding_left_shift: bool,
}

#[derive(Debug, Clone)]
struct ActionForbiddenUntil {
    activate_tile: Option<i32>,
    confirm_menu: Option<i32>,
    animation_cancel: Option<i32>,
    exit_menu: Option<i32>,
    scroll_up: Option<i32>,
    scroll_down: Option<i32>,
    left_click: Option<i32>,
    right_click: Option<i32>,
}

pub struct VerboseAction {
    action: GameAction,
    game_mode_tick: i32,
    goal: Option<Cow<'static, str>>,
    detail: Option<Cow<'static, str>>,

    /// If true, this action will be sent to the remote process.  If
    /// false, this action occurred too recently after the same input,
    /// and should be suppressed.
    allowed: bool,
}

pub struct GameActionAnnotator<'a> {
    detail: Option<&'a mut Option<Cow<'static, str>>>,
}

pub trait BotGoal: Any {
    fn description(&self) -> Cow<'static, str>;

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
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

    fn with_interrupt(self, interrupt: impl BotInterrupt) -> LogicStack
    where
        Self: Sized,
    {
        [LogicStackItem::interrupt(interrupt), self.into()]
            .into_iter()
            .collect()
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
    ) -> Result<Option<LogicStack>, Error>;
}

impl ActionForbiddenUntil {
    fn initial() -> Self {
        Self {
            activate_tile: None,
            confirm_menu: None,
            animation_cancel: None,
            exit_menu: None,
            scroll_up: None,
            scroll_down: None,
            left_click: None,
            right_click: None,
        }
    }

    fn from_game_state(game_state: &GameState) -> Self {
        // If an action is currently taking place, then any attempts
        // to repeat it may not occur until at least the next game
        // tick.
        let tick = game_state.globals.game_mode_tick + 1;

        let activate_tile = game_state
            .inputs
            .keys_pressed
            .contains(&Key::X)
            .then_some(tick);
        let confirm_menu = game_state
            .inputs
            .keys_pressed
            .contains(&Key::Y)
            .then_some(tick);
        let animation_cancel = game_state
            .inputs
            .keys_pressed
            .iter()
            .any(|key| matches!(key, Key::Delete | Key::RightShift | Key::R))
            .then_some(tick);
        let exit_menu = game_state
            .inputs
            .keys_pressed
            .contains(&Key::Escape)
            .then_some(tick);

        let scroll_up = (game_state.inputs.scroll_wheel
            == Some(ScrollWheel::ScrollingUp))
        .then_some(tick);
        let scroll_down = (game_state.inputs.scroll_wheel
            == Some(ScrollWheel::ScrollingDown))
        .then_some(tick);

        let left_click = game_state.inputs.left_mouse_down().then_some(tick);
        let right_click = game_state.inputs.right_mouse_down().then_some(tick);

        Self {
            activate_tile,
            confirm_menu,
            animation_cancel,
            exit_menu,
            scroll_up,
            scroll_down,
            left_click,
            right_click,
        }
    }

    fn from_actions(
        current_tick: i32,
        actions: impl IntoIterator<Item = GameAction>,
    ) -> Self {
        let mut out = Self::initial();

        // An input may not be sent again until two game ticks have
        // passed.  One game tick to allow the mouse/keyboard to be
        // recognized as pressed down, and another to be recognized as
        // pressed up.

        actions.into_iter().for_each(|action| match action {
            GameAction::ExitMenu => {
                out.exit_menu = Some(current_tick + 4);
                out.left_click = Some(current_tick + 4);
                out.right_click = Some(current_tick + 4);
            }
            GameAction::ActivateTile => {
                out.activate_tile = Some(current_tick + 2);
            }
            GameAction::ConfirmMenu => {
                out.confirm_menu = Some(current_tick + 2);
                out.activate_tile = Some(current_tick + 2);
            }
            GameAction::LeftClick => {
                out.left_click = Some(current_tick + 2);
                out.right_click = Some(current_tick + 2);
            }
            GameAction::RightClick => {
                out.left_click = Some(current_tick + 2);
                out.right_click = Some(current_tick + 2);
            }
            GameAction::ScrollUp => {
                out.scroll_up = Some(current_tick + 2);
            }
            GameAction::ScrollDown => {
                out.scroll_down = Some(current_tick + 2);
            }
            GameAction::AnimationCancel => {
                out.animation_cancel = Some(current_tick + 2);
            }

            // TODO: GameAction::SelectHotbar(_) => todo!(),
            _ => {}
        });

        out
    }

    fn merge(self, other: Self) -> Self {
        let merge = |a: Option<i32>, b: Option<i32>| -> Option<i32> {
            match (a, b) {
                (Some(a), Some(b)) => Some(a.max(b)),
                (None, other) | (other, None) => other,
            }
        };

        Self {
            activate_tile: merge(self.activate_tile, other.activate_tile),
            confirm_menu: merge(self.confirm_menu, other.confirm_menu),
            animation_cancel: merge(
                self.animation_cancel,
                other.animation_cancel,
            ),
            exit_menu: merge(self.exit_menu, other.exit_menu),
            scroll_up: merge(self.scroll_up, other.scroll_up),
            scroll_down: merge(self.scroll_down, other.scroll_down),
            left_click: merge(self.left_click, other.left_click),
            right_click: merge(self.right_click, other.right_click),
        }
    }

    fn at_game_mode_tick(self, game_mode_tick: i32) -> Self {
        let filter =
            |expires_at: &i32| -> bool { game_mode_tick < *expires_at };

        Self {
            activate_tile: self.activate_tile.filter(filter),
            confirm_menu: self.confirm_menu.filter(filter),
            animation_cancel: self.animation_cancel.filter(filter),
            exit_menu: self.exit_menu.filter(filter),
            scroll_up: self.scroll_up.filter(filter),
            scroll_down: self.scroll_down.filter(filter),
            left_click: self.left_click.filter(filter),
            right_click: self.right_click.filter(filter),
        }
    }
}

impl ActionCollector {
    fn new(game_state: &GameState, forbidden: ActionForbiddenUntil) -> Self {
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
        let is_holding_tool = game_state.inputs.keys_pressed.contains(&Key::C);
        let is_moving = [Key::W, Key::S, Key::A, Key::D]
            .into_iter()
            .any(|key| game_state.inputs.keys_pressed.contains(&key));

        let is_holding_left_shift = game_state.inputs.holding_left_shift();

        let is_scrolling_up =
            game_state.inputs.scroll_wheel == Some(ScrollWheel::ScrollingUp);
        let is_scrolling_down =
            game_state.inputs.scroll_wheel == Some(ScrollWheel::ScrollingDown);

        let is_left_clicking = game_state.inputs.left_mouse_down();
        let is_right_clicking = game_state.inputs.right_mouse_down();

        Self {
            actions: Vec::new(),
            refresh_current_location: false,
            forbidden,
            game_mode_tick: game_state.globals.game_mode_tick,
            is_activating_tile,
            is_confirming_menu,
            is_animation_cancelling,
            is_exiting_menu,
            is_holding_tool,
            is_moving,
            is_scrolling_up,
            is_scrolling_down,
            is_left_clicking,
            is_right_clicking,
            is_holding_left_shift,
        }
    }

    pub fn do_action(&mut self, action: GameAction) -> GameActionAnnotator<'_> {
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

        // let should_perform = match action {
        //     GameAction::LeftClick => !self.is_left_clicking,
        //     GameAction::RightClick => !self.is_right_clicking,
        //     GameAction::AnimationCancel => !self.is_animation_cancelling,
        //     GameAction::ActivateTile => !self.is_activating_tile,
        //     GameAction::ConfirmMenu => !self.is_confirming_menu,
        //     GameAction::ExitMenu => !self.is_exiting_menu,
        //     _ => true,
        // };

        let waited_dead_time = |wait_until: Option<i32>| -> bool {
            wait_until
                .map(|tick| self.game_mode_tick >= tick)
                .unwrap_or(true)
        };

        let allowed = match action {
            GameAction::LeftClick => {
                waited_dead_time(self.forbidden.left_click)
            }
            GameAction::RightClick => {
                waited_dead_time(self.forbidden.right_click)
            }
            GameAction::AnimationCancel => {
                waited_dead_time(self.forbidden.animation_cancel)
            }
            GameAction::ActivateTile => {
                waited_dead_time(self.forbidden.activate_tile)
            }
            GameAction::ConfirmMenu => {
                waited_dead_time(self.forbidden.confirm_menu)
            }
            GameAction::ExitMenu => waited_dead_time(self.forbidden.exit_menu),
            GameAction::ScrollDown => {
                waited_dead_time(self.forbidden.scroll_down)
            }
            GameAction::ScrollUp => waited_dead_time(self.forbidden.scroll_up),
            _ => true,
        };

        self.actions.push(VerboseAction {
            action,
            game_mode_tick: self.game_mode_tick,
            goal: None,
            detail: None,
            allowed,
        });
        GameActionAnnotator {
            detail: self
                .actions
                .last_mut()
                .map(|verbose_action| &mut verbose_action.detail),
        }
    }

    pub fn refresh_current_location(&mut self) {
        self.refresh_current_location = true;
    }

    fn annotate_new(&mut self, goal: &dyn BotGoal, num_actions_before: usize) {
        let goal = goal.description();
        self.actions[num_actions_before..].iter_mut().for_each(
            |verbose_action| {
                verbose_action.goal = Some(goal.clone());
            },
        );
    }

    fn finalize(&mut self) {
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
        if self.is_left_clicking {
            self.do_action(GameAction::ReleaseLeftClick);
        }
        if self.is_right_clicking {
            self.do_action(GameAction::ReleaseRightClick);
        }
        if self.is_scrolling_down {
            self.do_action(GameAction::StopScrollingDown);
        }
        if self.is_scrolling_up {
            self.do_action(GameAction::StopScrollingUp);
        }
        if self.is_animation_cancelling {
            self.do_action(GameAction::StopAnimationCanceling);
        }
        if self.is_activating_tile {
            self.do_action(GameAction::StopActivatingTile);
        }
        if self.is_confirming_menu {
            self.do_action(GameAction::StopConfirmingMenu);
        }
        if self.is_exiting_menu {
            self.do_action(GameAction::StopExitingMenu);
        }

        if self.is_holding_tool
            && self
                .actions
                .iter()
                .all(|action| !matches!(action.action, GameAction::HoldTool))
        {
            self.do_action(GameAction::ReleaseTool);
        }

        if self.is_moving
            && self
                .actions
                .iter()
                .all(|action| !matches!(action.action, GameAction::Move(_)))
        {
            self.do_action(GameAction::StopMoving);
        }

        if self.is_holding_left_shift
            && self.actions.iter().all(|action| {
                !matches!(action.action, GameAction::HoldLeftShift)
            })
        {
            self.do_action(GameAction::StopHoldingLeftShift);
        }
    }
}

impl GameActionAnnotator<'_> {
    pub fn annotate(&mut self, annotation: impl Into<Cow<'static, str>>) {
        if let Some(detail) = &mut self.detail {
            let _ = detail.insert(annotation.into());
        }
    }
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
                LogicStackItem::interrupt(super::StepCountForLuck::new()),
                LogicStackItem::interrupt(super::SkipCutscenes::new()),
                LogicStackItem::Goal(Box::new(super::GenericDay)),
            ],
            action_dead_time: ActionForbiddenUntil::initial(),

            recently_finished: Default::default(),
            recent_actions: Default::default(),
            verbose,
            recursed_during_current_update: Default::default(),
        }
    }

    pub fn update(
        &mut self,
        game_state: &GameState,
    ) -> Result<(Vec<GameAction>, bool), Error> {
        // Mark actions which may be repeated, now that enough time
        // has passed.
        self.action_dead_time = self
            .action_dead_time
            .clone()
            .at_game_mode_tick(game_state.globals.game_mode_tick);

        // If any actions are in-progress, based on the current game
        // state, mark inputs that may not be taken again until the
        // next game tick.  (e.g. If the mouse is currently down, do
        // not send another MouseDown event.)
        self.action_dead_time = self
            .action_dead_time
            .clone()
            .merge(ActionForbiddenUntil::from_game_state(game_state));

        let mut actions =
            ActionCollector::new(game_state, self.action_dead_time.clone());

        let mut previously_produced_subgoals: Option<usize> = None;
        self.recursed_during_current_update.clear();

        loop {
            // Check if any CancelIf conditions should be triggered.
            // If so, everything above that position in the stack gets
            // removed.
            let opt_cancellation = self
                .stack
                .iter()
                .enumerate()
                .rev()
                .take_while(|(_, item)| {
                    !matches!(item, LogicStackItem::PreventInterrupt)
                })
                .filter(|(_, item)| match item {
                    LogicStackItem::CancelIf(cond) => cond(game_state),
                    LogicStackItem::PreventInterrupt
                    | LogicStackItem::Goal(_)
                    | LogicStackItem::Interrupt { .. } => false,
                })
                .last()
                .map(|(i, _)| i);
            if let Some(cancellation) = opt_cancellation {
                if self.verbose {
                    println!(
                        "Interrupt {cancellation} triggered, \
                         removing all goals above it."
                    )
                }
                // Truncate to just above the cancellation, without
                // recording those goals as completed.  Then, pop the
                // cancellation, recording it as completed.
                let old_len = self.stack.len();
                self.stack.truncate(cancellation + 1);
                self.pop_from_logic_stack();

                for i in self.stack.len()..old_len {
                    self.recursed_during_current_update.remove(&i);
                }
            }

            // Any cancellations or interrupts on the top of the stack no longer
            // have anything remaining that they can cancel/interrupt.
            while self
                .stack
                .last()
                .map(|item| {
                    matches!(
                        item,
                        LogicStackItem::PreventInterrupt
                            | LogicStackItem::CancelIf(_)
                            | LogicStackItem::Interrupt { .. }
                    )
                })
                .unwrap_or(false)
            {
                self.pop_from_logic_stack();
            }

            // If anything was removed, mark interrupts that may be
            // triggered again.
            self.reset_completed_interrupts();

            // Check if any interrupts should push a new goal onto the
            // top of the stack.
            self.check_interrupts(game_state, 0)?;

            let (i_goal, current_goal): (usize, &mut dyn BotGoal) = self
                .stack
                .iter_mut()
                .enumerate()
                .filter_map(|(i, item)| match item {
                    LogicStackItem::Goal(bot_goal) => {
                        Some((i, bot_goal.as_mut()))
                    }
                    LogicStackItem::PreventInterrupt
                    | LogicStackItem::CancelIf(_)
                    | LogicStackItem::Interrupt { .. } => None,
                })
                .last()
                .ok_or(Error::NoRemainingGoals)?;

            if self.verbose {
                println!("Running top goal '{}'", current_goal.description());
            }

            assert!(
                !self.recursed_during_current_update.contains(&i_goal),
                "Infinite loop detected: \
                 Current goal '{}' has already executed on this frame, \
                 and previously produced subgoals.  \
                 If it runs again, it will produce those same subgoals, \
                 in an infinite loop.\n\
                 Current stack:\n\t{}",
                current_goal.description(),
                self.stack
                    .iter()
                    .map(|item| item.description())
                    .format("\n\t")
            );

            let num_actions_before = actions.actions.len();
            let goal_result = current_goal.apply(game_state, &mut actions)?;
            actions.annotate_new(current_goal, num_actions_before);

            match goal_result {
                BotGoalResult::Completed => {
                    if self.verbose {
                        println!("\tGoal completed, removing from stack");
                    }

                    if let Some(prev) = previously_produced_subgoals {
                        assert!(
                            self.stack.len() > prev + 2,
                            "Infinite loop detected.  \
                             Current goal '{0}' has completed, \
                             but this returns control \
                             to the preceding goal '{1}'.  \
                             Since this preceding goal '{1}' \
                             has already been run, \
                             and chose to delegate to '{0}', \
                             executing it again would enter an infinite loop.",
                            self.current_goal().unwrap().description(),
                            match self.stack.get(prev) {
                                Some(LogicStackItem::Goal(prev_goal)) => {
                                    prev_goal.description()
                                }
                                Some(LogicStackItem::PreventInterrupt) =>
                                    format!(
                                        "Should be unreachable, \
                                     since {prev} contained a BotGoal \
                                     the first time around, \
                                     but now contains PreventInterrupt"
                                    )
                                    .into(),
                                Some(LogicStackItem::CancelIf(_)) => format!(
                                    "Should be unreachable, \
                                     since {prev} contained a BotGoal \
                                     the first time around, \
                                     but now contains CancelIf."
                                )
                                .into(),
                                Some(LogicStackItem::Interrupt {
                                    interrupt,
                                    ..
                                }) => {
                                    format!(
                                        "Should be unreachable, \
                                         since {prev} contained a BotGoal \
                                         the first time around, \
                                         but now contains interrupt '{}'",
                                        interrupt.description(),
                                    )
                                    .into()
                                }
                                None => format!(
                                    "Should be unreachable, \
                                     since {prev} contained a BotGoal \
                                     the first time around, \
                                     but is now empty."
                                )
                                .into(),
                            }
                        );
                    }

                    self.pop_from_logic_stack();
                    self.reset_completed_interrupts();
                }
                BotGoalResult::SubGoals(sub_goals) => {
                    let stack_size = self.stack.len();
                    self.recursed_during_current_update.insert(stack_size - 1);
                    previously_produced_subgoals = Some(stack_size - 1);

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

                    self.check_interrupts(game_state, stack_size)?;
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

        if self.verbose {
            println!(
                "From the goals, \
                 sending the following {} actions to game",
                actions.actions.len(),
            );
            actions
                .actions
                .iter()
                .for_each(|action| println!("\tAction: {action}"));
        }

        actions.finalize();

        // For any action taken, mark which ones may not be repeated
        // until at least two game ticks have occurred.  (e.g. If
        // sending a MouseDown event, do not send another MouseDown
        // event until two frames have passed..)
        self.action_dead_time = self.action_dead_time.clone().merge(
            ActionForbiddenUntil::from_actions(
                game_state.globals.game_mode_tick,
                actions
                    .actions
                    .iter()
                    .filter(|verbose_action| verbose_action.allowed)
                    .map(|verbose_action| verbose_action.action),
            ),
        );

        let mut output_actions = Vec::new();
        for verbose_action in actions.actions {
            if verbose_action.allowed {
                output_actions.push(verbose_action.action);
            }
            self.recent_actions.push_back(verbose_action);
            while self.recent_actions.len() > MAX_RECENT_ACTIONS {
                self.recent_actions.pop_front();
            }
        }

        Ok((output_actions, actions.refresh_current_location))
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

    fn check_interrupts(
        &mut self,
        game_state: &GameState,
        min_index: usize,
    ) -> Result<(), Error> {
        for i_item in (min_index..self.stack.len()).rev() {
            let current_stack_size = self.stack.len();
            let item = &mut self.stack[i_item];

            if matches!(item, LogicStackItem::PreventInterrupt) {
                break;
            }

            let LogicStackItem::Interrupt {
                interrupt,
                active_goal,
            } = item
            else {
                continue;
            };
            if active_goal.is_some() {
                continue;
            }
            let Some(interrupt_stack) = interrupt.check(game_state)? else {
                continue;
            };

            if self.verbose {
                println!("Interrupt '{}' triggered", interrupt.description());
            }

            assert!(
                !self.recursed_during_current_update.contains(&i_item),
                "Infinite loop detected.  \
                 Interrupt '{}' fired multiple times \
                 during a single update.  \
                 The second time, it produced goals [{}].",
                interrupt.description(),
                interrupt_stack
                    .0
                    .iter()
                    .map(|item| format!("'{}'", item.description()))
                    .format(", ")
            );

            self.recursed_during_current_update.insert(i_item);
            *active_goal = Some(current_stack_size);
            interrupt_stack.0.into_iter().for_each(|item| {
                if self.verbose {
                    println!("\tPushing '{}' onto stack", item.description());
                }
                self.stack.push(item)
            });
        }

        Ok(())
    }

    fn pop_from_logic_stack(&mut self) {
        let item = self.stack.pop().expect("Bot has run out of goals");
        self.recursed_during_current_update
            .remove(&self.stack.len());

        self.recently_finished.push_back(item);

        while self.recently_finished.len() > MAX_RECENT_GOALS {
            self.recently_finished.pop_front();
        }
    }

    pub fn iter(
        &self,
    ) -> impl DoubleEndedIterator<Item = &LogicStackItem> + '_ {
        self.stack.iter()
    }

    pub fn iter_recent_actions(
        &self,
    ) -> impl DoubleEndedIterator<Item = &VerboseAction> + ExactSizeIterator + '_
    {
        self.recent_actions.iter()
    }

    pub fn iter_recently_completed(
        &self,
    ) -> impl DoubleEndedIterator<Item = &LogicStackItem> + '_ {
        self.recently_finished.iter().rev()
    }

    pub fn current_goal(&self) -> Option<&dyn BotGoal> {
        self.stack.iter().rev().find_map(|item| match item {
            LogicStackItem::Goal(bot_goal) => Some(bot_goal.as_ref()),
            _ => None,
        })
    }
}

impl Default for LogicStack {
    fn default() -> Self {
        Self::new()
    }
}

impl LogicStack {
    pub fn new() -> Self {
        Self(Default::default())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn with_item(mut self, item: LogicStackItem) -> Self {
        self.0.push_front(item);
        self
    }

    pub fn then(mut self, goal: impl BotGoal + 'static) -> Self {
        // Push onto the beginning of the stack, such that the new
        // goal will be the last item executed.
        self.0.push_front(goal.into());
        self
    }

    pub fn then_if(
        self,
        goal: impl BotGoal + 'static,
        condition: bool,
    ) -> Self {
        if condition {
            self.then(goal)
        } else {
            self
        }
    }

    pub fn then_iter<Iter, Goal>(mut self, iter_goal: Iter) -> Self
    where
        Iter: IntoIterator<Item = Goal>,
        Goal: Into<LogicStackItem>,
    {
        // Push onto the beginning of the stack, such that the new
        // goal will be the last item executed.
        for goal in iter_goal {
            self.0.push_front(goal.into());
        }
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

    pub fn with_interrupt(mut self, interrupt: impl BotInterrupt) -> Self {
        self.0.push_front(LogicStackItem::interrupt(interrupt));
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
            LogicStackItem::PreventInterrupt => "PreventInterrupt".into(),
        }
    }

    pub fn interrupt(interrupt: impl BotInterrupt) -> Self {
        Self::Interrupt {
            interrupt: Box::new(interrupt),
            active_goal: None,
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

impl From<LogicStackItem> for LogicStack {
    fn from(stack_item: LogicStackItem) -> Self {
        Self(std::iter::once(stack_item).collect())
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

impl From<LogicStackItem> for BotGoalResult {
    fn from(stack_item: LogicStackItem) -> Self {
        Self::SubGoals(stack_item.into())
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

impl Display for VerboseAction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: ", self.game_mode_tick)?;

        if !self.allowed {
            write!(f, "(suppressed) ")?;
        }

        write!(f, "{}", self.action)?;

        if let Some(goal) = &self.goal {
            write!(f, " from '{goal}'")?;
        }

        if let Some(detail) = &self.detail {
            write!(f, " because '{detail}'")?;
        }

        Ok(())
    }
}

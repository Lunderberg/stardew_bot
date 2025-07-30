use geometry::Direction;

use crate::{
    predictors::StepCountPredictor, Error, GameAction, GameStateExt as _,
    ItemIterExt as _, StopMovingGoal,
};
use game_state::{GameState, ItemId};

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
    start_time_without_gift: i32,

    /// The time in the day when the steps should start being checked,
    /// on days when a gift is being manipulated.  Defaults to 1300
    /// (1:00 PM).
    start_time_with_gift: i32,

    /// The maximum number of steps that may be taken to manipulate
    /// the step counter, when not manipulating for a gift.
    ///
    /// While in principle, we could always take enough steps that the
    /// day will be guaranteed to have the best possible luck (+100 on
    /// an RNG roll from -100 to +100), taking that many steps may
    /// require taking more steps than there is time to do.
    /// Therefore, select the best possible luck that can be achieved
    /// within the next `max_lookahead` steps.
    max_lookahead_without_gift: usize,

    /// The maximum number of steps that may be taken to manipulate
    /// the step counter, when manipulating for a gift.
    max_lookahead_with_gift: usize,

    /// A cache of the result of `self.select_gifter`.
    preferred_gifter: Option<Option<PreferredGifter>>,

    /// The target number of steps when the day ends.
    ///
    /// When the time reaches `start_time`, this value is decided
    /// based on the current number of steps taken.
    target_steps: Option<u32>,
}

#[derive(Debug, Clone)]
struct PreferredGifter {
    /// The NPC's index in the `friendshipData` lookup.  For the
    /// target NPC to be selected as the gift giver, the
    /// `StepCountPrediction.friendship_index` field should be equal
    /// to the index.
    index: usize,

    /// The current number of hearts in the relationship between the
    /// player and the target NPC.  For the gift to be sent, the
    /// `StepCountPrediction.min_hearts_for_gift` field should be no
    /// greater than the current hearts.
    current_hearts: i32,
}

impl StepCountForLuck {
    pub fn new() -> Self {
        Self {
            start_time_with_gift: 1300,
            start_time_without_gift: 2500,
            max_lookahead_without_gift: 30,
            max_lookahead_with_gift: 500,
            preferred_gifter: None,
            target_steps: None,
        }
    }

    pub fn is_completed(&self, game_state: &GameState) -> Result<bool, Error> {
        let prediction = StepCountPredictor::new(game_state).predict(0);

        let opt_gifter = self
            .preferred_gifter
            .clone()
            .map_or_else(|| self.select_gifter(game_state), Ok)?;

        let is_completed = if let Some(gifter) = opt_gifter {
            prediction.friendship_index == gifter.index
                && prediction.min_hearts_for_gift <= gifter.current_hearts
                && prediction.daily_luck > 0
        } else {
            prediction.daily_luck > 90
        };

        Ok(is_completed)
    }

    /// Determine if step-manipulation should select an NPC to give a
    /// gift.
    ///
    /// If an NPC should give a gift, returns the index of the NPC
    /// that should give a gift, which can be compared against
    /// `StepCountPrediction.friendship_index`.  Otherwise, returns
    /// `None`.
    fn select_gifter(
        &self,
        game_state: &GameState,
    ) -> Result<Option<PreferredGifter>, Error> {
        let available_items = game_state.iter_accessible_items()?.item_counts();
        let opt_gifter = game_state
            .iter_reserved_items()?
            .filter(|(id, num_reserved)| {
                let num_available = available_items.get(id).unwrap_or(&0);
                num_available < num_reserved
            })
            .filter_map(|(id, _)| {
                // TODO: Determine potential gifts based on the
                // contents of the letters that would get sent by each
                // NPC.
                if id == &ItemId::BATTERY {
                    Some("Pam")
                } else {
                    None
                }
            })
            .find_map(|name| {
                game_state.player.friendships.iter().enumerate().find_map(
                    |(index, friendship)| {
                        let current_hearts = friendship.points / 250;
                        (friendship.name == name && current_hearts > 0).then(
                            || PreferredGifter {
                                index,
                                current_hearts,
                            },
                        )
                    },
                )
            });

        Ok(opt_gifter)
    }

    fn plan_steps(
        &self,
        game_state: &GameState,
        opt_gifter: Option<PreferredGifter>,
    ) -> Result<Option<u32>, Error> {
        let steps_taken: u32 =
            game_state.globals.get_stat("stepsTaken").unwrap_or(0);
        let predictor = StepCountPredictor::new(game_state);

        let max_lookahead = if opt_gifter.is_some() {
            self.max_lookahead_with_gift
        } else {
            self.max_lookahead_without_gift
        };

        let opt_target_steps = (0u32..)
            .map(|additional_steps| {
                (
                    steps_taken + additional_steps,
                    predictor.predict(additional_steps),
                )
            })
            .take(max_lookahead)
            .filter(|(_, prediction)| {
                opt_gifter
                    .as_ref()
                    .map(|gifter| {
                        gifter.index == prediction.friendship_index
                            && gifter.current_hearts
                                >= prediction.min_hearts_for_gift
                    })
                    .unwrap_or(true)
            })
            .max_by_key(|(_, prediction)| prediction.daily_luck)
            .map(|(total_steps, _)| total_steps);

        Ok(opt_target_steps)
    }

    fn reached_lucky_step_count(
        &mut self,
        game_state: &GameState,
    ) -> Result<bool, Error> {
        if self.preferred_gifter.is_none() {
            self.preferred_gifter = Some(self.select_gifter(game_state)?);
        }

        if self.target_steps.is_none() {
            let opt_gifter = self
                .preferred_gifter
                .clone()
                .expect("Just populated the self.preferred_gifter cache");
            self.target_steps = self.plan_steps(game_state, opt_gifter)?;
        }
        if self.target_steps.is_none() {
            // It would take too many steps to get a gift from the
            // preferred gifter.  Disable the gift manipulation for
            // today, and fall back to normal luck manipulation.
            self.preferred_gifter = Some(None);
            self.target_steps = self.plan_steps(game_state, None)?;
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
            format!(
                "Wait until {}, then step-count",
                self.start_time_without_gift
            )
            .into()
        }
    }

    fn check(
        &mut self,
        game_state: &GameState,
    ) -> Result<Option<LogicStack>, Error> {
        if game_state.globals.in_game_time < self.start_time_with_gift {
            return Ok(None);
        }

        if self.preferred_gifter.is_none() {
            self.preferred_gifter = Some(self.select_gifter(game_state)?);
        }
        let opt_gifter = self
            .preferred_gifter
            .clone()
            .expect("Just populated the self.preferred_gifter cache");

        if opt_gifter.is_none()
            && game_state.globals.in_game_time < self.start_time_without_gift
        {
            return Ok(None);
        }

        if self.target_steps.is_none() {
            self.target_steps = self.plan_steps(game_state, opt_gifter)?;
        }
        if self.target_steps.is_none() {
            // It would take too many steps to get a gift from the
            // preferred gifter.  Disable the gift manipulation for
            // today, and fall back to normal luck manipulation.
            self.preferred_gifter = Some(None);
            self.target_steps = self.plan_steps(game_state, None)?;
            if game_state.globals.in_game_time < self.start_time_without_gift {
                // We aren't going to manipulate for a gift in the
                // mail, but it's also too early to manipulate for
                // normal luck.
                return Ok(None);
            }
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

        if game_state.player.animation.animation_frame % 4 != 3 {
            // The stepsTaken counter is incremented after the
            // animation frame is incremented, and the resulting frame
            // counter is divisible by 4.  Therefore, movement can
            // continue on frames 0/1/2, but should be stopped on
            // frame 3 so that we don't advance the stepTaken counter.
            //
            // The duration of frame 3 varies depending on the
            // direction the player is facing, and is either 60 ms or
            // 100 ms.  That gives 3 frames in which the movement can
            // be stopped, enough to consistently avoid incrementing
            // the frame counter.
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

use crate::{
    bot_logic::StopMovingGoal, game_state::SeededRng, Error, GameState,
};

use super::bot_logic::{BotInterrupt, LogicStack};

pub struct StepCountForLuck {
    /// The time in the day when the steps should start being checked.
    /// Defaults to 2500 (1:00 AM).
    start_time: i32,

    /// The maximum number of steps that may be taken to manipulate
    /// the step counter.
    ///
    /// While in principle, we could always take enough steps that the
    /// day will be guaranteed to have the best possible luck (+100 on
    /// an RNG roll from -100 to +100), taking that many steps may
    /// require taking more steps than there is time to do.
    /// Therefore, select the best possible luck that can be achieved
    /// within the next `max_lookahead` steps.
    max_lookahead: usize,

    /// The target number of steps when the day ends.
    ///
    /// When the time reaches `start_time`, this value is decided
    /// based on the current number of steps taken.
    target_steps: Option<u32>,
}

impl StepCountForLuck {
    pub fn new() -> Self {
        Self {
            start_time: 2500,
            max_lookahead: 30,
            target_steps: None,
        }
    }

    fn plan_steps(&self, game_state: &GameState) -> Result<u32, Error> {
        let additional_steps = (0..(self.max_lookahead as u32))
            .max_by_key(|additional_steps| {
                predict_daily_luck(game_state, *additional_steps)
            })
            .unwrap();

        let steps_taken: u32 =
            game_state.globals.get_stat("stepsTaken").unwrap_or(0);

        Ok(steps_taken + additional_steps)
    }
}

impl BotInterrupt for StepCountForLuck {
    fn description(&self) -> std::borrow::Cow<str> {
        if let Some(steps) = self.target_steps {
            format!("Stop at {steps} steps").into()
        } else {
            format!("Wait until {}, then step-count", self.start_time).into()
        }
    }

    fn check(
        &mut self,
        game_state: &GameState,
    ) -> Result<Option<LogicStack>, Error> {
        if game_state.globals.in_game_time < self.start_time {
            return Ok(None);
        }

        if self.target_steps.is_none() {
            self.target_steps = Some(self.plan_steps(game_state)?);
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

        if steps_taken < target_steps {
            // Still have more steps to take before reaching target
            // step count.
            return Ok(None);
        }

        if game_state.player.movement.is_none() {
            // Not currently moving, no need to change anything.
            return Ok(None);
        }

        // The player is currently moving, and has alreaady reached
        // the step count.
        Ok(Some(StopMovingGoal.into()))
    }
}

/// Check if a value is allowed to be the dish of the day
///
/// The dish-of-the-day roll is repeated until a valid dish is
/// selected.  Since the daily luck roll occurs after the
/// dish-of-the-day roll, predicting the daily luck requires knowing
/// how many PRNG rolls are consumed by the dish-of-the-day updates.
fn is_allowed_dish_of_the_day(value: i32) -> bool {
    assert!(
        194 <= value && value < 240,
        "Value {value} is outside allowed range \
         for the DishOfTheDay."
    );
    match value {
        // Exclusions to prevent the DishOfTheDay from being something
        // that is already sold by the Saloon.
        346 => false, // Beer
        196 => false, // Salad
        216 => false, // Bread
        224 => false, // Spaghetti
        206 => false, // Pizza
        395 => false, // Coffee

        // Exclusions for non-existent items.  This shouldn't vary,
        // but if it does, can unpack and compare against the
        // ItemRegistry.
        217 => false,

        // Everything else is allowed.
        _ => true,
    }
}

/// Generate a seeded RNG for the following day
///
/// Valid for Stardew Valley 1.6.15, with legacy RNG disabled.
///
/// In 1.5 and earlier, or with legacy RNG enabled, the components are
/// added together instead of being hashed together.  It's also
/// possible that some versions of 1.6 only perform a single round of
/// hashing rather than two rounds of hashing, as I've seen that
/// described in some posts, but 1.6.15 does perform two rounds of
/// hashing, one before distributing the seed to all clients, and one
/// after.
fn new_day_rng(game_state: &GameState, additional_steps: u32) -> SeededRng {
    let unique_id: u32 = game_state.globals.unique_id as u32;

    let days_played: u32 =
        game_state.globals.get_stat("daysPlayed").unwrap_or(1);
    // The daysPlayed counter is incremented before seeding the RNG.
    let days_played = days_played + 1;

    let steps_taken: u32 =
        game_state.globals.get_stat("stepsTaken").unwrap_or(0);
    let steps_taken = steps_taken + additional_steps;

    let seed = SeededRng::stardew_seed([
        (unique_id / 100) as f64,
        (days_played * 10 + 1) as f64,
        steps_taken as f64,
    ]);

    SeededRng::from_stardew_seed([seed as f64])
}

/// Predict the daily luck, based on the current game state.
///
/// Returns the luck as `i8`, which will always be on the range
/// [-100,100], inclusive.
pub fn predict_daily_luck(game_state: &GameState, additional_steps: u32) -> i8 {
    let days_played: u32 =
        game_state.globals.get_stat("daysPlayed").unwrap_or(1) + 1;

    let mut rng = new_day_rng(game_state, additional_steps);

    // RNG advances based on which day it is
    let day_of_month = (days_played - 1) % 28 + 1;
    for _ in 0..day_of_month {
        rng.rand_i32();
    }

    // Update the dish of the day.  This call re-rolls on failure
    loop {
        let dish_of_day = rng.rand_in_range(194..240);
        if is_allowed_dish_of_the_day(dish_of_day) {
            break;
        }
    }
    // Two calls to decide how many instances are available for
    // DishOfTheDay.
    rng.rand_i32();
    rng.rand_i32();

    // Object constructor
    rng.rand_i32();

    // RNG call to decide whether the player will receive the rarecrow
    // recipe after having collected all 8 rarecrows (90.5% chance).
    rng.rand_i32();

    // RNG call to decide which NPC may send a gift in the mail,
    // followed by a check to see if they will send a gift.
    // Technically, these calls may be skipped if the player hasn't
    // met any NPCs, but since both Robin and Lewis are met in the
    // opening cutscene, these RNG calls will always be made.
    rng.rand_i32();
    rng.rand_i32();

    // TODO: RNG calls for cursed mannequins in the same location as a
    // sleeping player.

    // Now the call that we care about
    let luck = rng.rand_in_range(-100..101);

    luck as i8
}

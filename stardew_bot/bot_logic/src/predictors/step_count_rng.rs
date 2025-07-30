use game_state::{GameState, SeededRng};

pub struct StepCountPredictor {
    game_id: u32,
    days_played: u32,
    steps_taken: u32,
    num_friendships: usize,
}

#[derive(Debug)]
pub struct StepCountPrediction {
    /// The index in `game_state.player.friendships` of the NPC who
    /// will attempt to give a gift to the player.
    pub friendship_index: usize,

    /// The nummber of hearts required with the selected friend in
    /// order for the gift-giving to succeed.
    pub min_hearts_for_gift: i32,

    /// The daily luck, within the range of [-100,100], inclusive.
    pub daily_luck: i8,
}

impl StepCountPredictor {
    pub fn new(game_state: &GameState) -> Self {
        let game_id: u32 = game_state.globals.game_id as u32;

        // The daysPlayed counter is incremented before seeding the
        // RNG, so all predictions should be based on tomorrow's date,
        // not today's date.
        let days_played: u32 =
            game_state.globals.get_stat("daysPlayed").unwrap_or(1) + 1;

        let steps_taken: u32 =
            game_state.globals.get_stat("stepsTaken").unwrap_or(0);

        let num_friendships = game_state.player.friendships.len();

        Self {
            game_id,
            days_played,
            steps_taken,
            num_friendships,
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
    fn new_day_rng(&self, additional_steps: u32) -> SeededRng {
        let seed = SeededRng::stardew_seed([
            (self.game_id / 100) as f64,
            (self.days_played * 10 + 1) as f64,
            (self.steps_taken + additional_steps) as f64,
        ]);

        SeededRng::from_stardew_seed([seed as f64])
    }

    /// Predict the outcome resulting from ending the day, based on
    /// the current step count, and the number of `additional_steps`
    /// taken between now and the end of the day.
    pub fn predict(&self, additional_steps: u32) -> StepCountPrediction {
        let mut rng = self.new_day_rng(additional_steps);

        // RNG advances based on which day it is
        let day_of_month = (self.days_played - 1) % 28 + 1;
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

        // RNG call to decide which NPC may send a gift in the mail,
        // followed by a check to see if they will send a gift.
        // Technically, these calls may be skipped if the player hasn't
        // met any NPCs, but since both Robin and Lewis are met in the
        // opening cutscene, these RNG calls will always be made.
        let friendship_index =
            rng.rand_in_range(0..self.num_friendships as i32) as usize;
        let min_hearts_for_gift = rng.rand_in_range(1..11);

        // RNG call to decide whether the player will receive the rarecrow
        // recipe after having collected all 8 rarecrows (90.5% chance).
        rng.rand_i32();

        // TODO: RNG calls for cursed mannequins in the same location as a
        // sleeping player.

        // Now the important call, the daily luck for the next day.
        let daily_luck = rng.rand_in_range(-100..101) as i8;

        StepCountPrediction {
            daily_luck,
            friendship_index,
            min_hearts_for_gift,
        }
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

use crate::{game_state::SeededRng, GameState};

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

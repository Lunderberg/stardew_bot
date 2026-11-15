use game_state::{GameState, SeededRng};
use geometry::Vector;

pub struct FarmCoalRockPredictor {
    game_id: u32,
    days_played: u32,
    rng_chance: f32,
}

impl FarmCoalRockPredictor {
    pub fn new(game_state: &GameState) -> Self {
        let game_id: u32 = game_state.globals.game_id as u32;
        let days_played: u32 =
            game_state.globals.get_stat("daysPlayed").unwrap_or(1);

        let daily_luck = game_state.daily.daily_luck as f32;
        let mining_level = game_state.player.skills.mining_level() as f32;

        let chance_modifier = daily_luck / 2.0 + mining_level * 0.005;
        let rng_chance = 0.05 * (1.0 + chance_modifier);

        Self {
            game_id,
            days_played,
            rng_chance,
        }
    }

    pub fn will_produce_coal(&self, tile: Vector<isize>) -> bool {
        let mut rng = SeededRng::from_stardew_seed([
            self.days_played as f64,
            (self.game_id / 2) as f64,
            (tile.right * 1000) as f64,
            tile.down as f64,
        ]);

        // First roll for coal, based on profession and dwarf statue
        // bonuses.  Since I'm currently just using this on Day1, this
        // roll will always fail.
        rng.rand_float();

        // Second roll for coal, which may actually succeed.
        rng.rand_float() < self.rng_chance
    }
}

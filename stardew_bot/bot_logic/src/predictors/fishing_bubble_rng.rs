use std::ops::Range;

use game_state::{GameState, TileMap, TimeDelta, Timestamp};
use geometry::Vector;
use itertools::Itertools as _;

use crate::Error;

pub struct FishingBubblePredictor {
    game_id: u32,
    days_played: u32,
    dist_from_land: TileMap<u8>,
    may_contain_bubbles: TileMap<bool>,
}

pub struct FishingBubblePrediction {
    /// The tile on which the bubbles occur.
    pub tile: Vector<isize>,

    /// The start/end time of the bubbles.
    pub duration: Range<Timestamp>,
}

impl FishingBubblePredictor {
    pub fn new(game_state: &GameState, room: &str) -> Result<Self, Error> {
        let game_id = game_state.globals.game_id as u32;
        let days_played =
            game_state.globals.get_stat("daysPlayed").unwrap_or(1);

        let loc = game_state.get_room(room)?;

        let dist_from_land = {
            let mut dist_from_land = TileMap::full(
                5u8,
                loc.water_tiles.width(),
                loc.water_tiles.height(),
            );
            loc.water_tiles
                .iter_false()
                .cartesian_product(-6..=6)
                .cartesian_product(-6..=6)
                .for_each(|((land, di), dj)| {
                    let offset = Vector::new(di, dj);
                    if let Some(min_dist) =
                        dist_from_land.get_mut(land + offset)
                    {
                        let dist = di.abs().max(dj.abs()) as u8;
                        *min_dist = (*min_dist).min(dist);
                    }
                });

            dist_from_land
        };

        let may_contain_bubbles = dist_from_land.imap(|tile, &dist| {
            // Overpredicts, dist==2 doesn't actually occur.  My
            // Chebyshev distance check is one larger than the game's
            // `distanceToLand` check.
            //
            // let passes_dist_check = dist > 1 && dist < 5;

            // Overpredicts, dist==6 doesn't actually occur in-game.
            // This is what I think it should be, to match the game's
            // `dist > 1 && dist < 5` condition, but doing so gives
            // inaccurate results.
            //
            // let passes_dist_check = dist > 2 && dist < 6

            let passes_dist_check = dist > 2 && dist < 5;

            passes_dist_check
                && loc.has_building_layer.is_unset(tile)
                && loc.no_fishing.is_unset(tile)
        });

        Ok(Self {
            game_id,
            days_played,
            dist_from_land,
            may_contain_bubbles,
        })
    }

    pub fn on_day(self, day: u32) -> Self {
        Self {
            days_played: day,
            ..self
        }
    }

    pub fn predict(&self) -> Vec<FishingBubblePrediction> {
        // The tile of bubbles, along with the time at which they started.
        let mut current_bubbles: Option<(Vector<isize>, Timestamp)> = None;
        let mut results = Vec::<FishingBubblePrediction>::new();

        (6..=25)
            .flat_map(|hour| (0..6).map(move |ten_min| (hour, ten_min * 10)))
            .map(|(hour, minute)| {
                Timestamp::new(600)
                    + TimeDelta::from_minutes(hour * 60 + minute)
            })
            .for_each(|time| {
                let mut rng = game_state::SeededRng::from_stardew_seed([
                    self.days_played as f64,
                    (self.game_id / 2) as f64,
                    time.raw() as f64,
                    self.dist_from_land.width() as f64,
                ]);

                if let Some((tile, start_time)) = current_bubbles {
                    let elapsed = time - start_time;
                    let stop_chance = 0.1 + (elapsed.minutes() as f32) / 1800.0;
                    let frenzy = false; // TODO: Predict fish-frenzies.
                    let min_duration =
                        TimeDelta::from_minutes(if frenzy { 120 } else { 60 });
                    let bubbles_stop = rng.rand_float() < stop_chance
                        && elapsed > min_duration;
                    if bubbles_stop {
                        results.push(FishingBubblePrediction {
                            tile,
                            duration: start_time..time,
                        });
                        current_bubbles = None;
                    }
                } else {
                    let start_bubbles = rng.rand_bool();
                    let opt_new_bubbles = (0..2)
                        .map(|_| {
                            let width = self.dist_from_land.width() as i32;
                            let i = rng.rand_in_range(0..width) as isize;

                            let height = self.dist_from_land.height() as i32;
                            let j = rng.rand_in_range(0..height) as isize;

                            Vector::<isize>::new(i, j)
                        })
                        .filter(|_| start_bubbles)
                        .find(|tile| self.may_contain_bubbles.is_set(*tile));

                    if let Some(tile) = opt_new_bubbles {
                        current_bubbles = Some((tile, time));
                    }
                }
            });

        if let Some((tile, start_time)) = current_bubbles {
            let end_time = Timestamp::new(2600);
            results.push(FishingBubblePrediction {
                tile,
                duration: start_time..end_time,
            });
        }

        results
    }

    pub fn dist_from_land(&self, tile: Vector<isize>) -> u8 {
        self.dist_from_land.get(tile).cloned().unwrap_or(0)
    }
}

use std::ops::Range;

use game_state::{GameState, TileMap};
use geometry::Vector;
use itertools::Itertools as _;

use crate::Error;

pub struct FishingBubblePredictor {
    game_id: u32,
    days_played: u32,
    may_contain_bubbles: TileMap<bool>,
}

pub struct FishingBubblePrediction {
    /// The tile on which the bubbles occur.
    pub tile: Vector<isize>,

    /// The start/end time of the bubbles.
    pub duration: Range<i32>,
}

impl FishingBubblePredictor {
    pub fn new(game_state: &GameState, room: &str) -> Result<Self, Error> {
        let game_id = game_state.globals.game_id as u32;
        let days_played =
            game_state.globals.get_stat("daysPlayed").unwrap_or(1);

        let loc = game_state.get_room(room)?;

        let dist_from_land = {
            let w = loc.shape.right as usize;
            let h = loc.shape.down as usize;
            let mut dist_from_land = TileMap::<isize>::full(10, w, h);
            loc.water_tiles
                .iter_false()
                .cartesian_product(-6..=6)
                .cartesian_product(-6..=6)
                .for_each(|((land, di), dj)| {
                    let offset = Vector::new(di, dj);
                    if let Some(min_dist) =
                        dist_from_land.get_mut(land + offset)
                    {
                        let dist = di.abs().max(dj.abs());
                        *min_dist = (*min_dist).min(dist);
                    }
                });
            dist_from_land
        };
        let may_contain_bubbles =
            dist_from_land.map(|&dist| dist > 1 && dist < 5);

        Ok(Self {
            game_id,
            days_played,
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
        let mut current_bubbles: Option<(Vector<isize>, (i32, i32))> = None;
        let mut results = Vec::<FishingBubblePrediction>::new();

        (6..=25)
            .flat_map(|hour| (0..6).map(move |ten_min| (hour, ten_min * 10)))
            .for_each(|(hour, minute)| {
                let time = hour * 100 + minute;
                let mut rng = game_state::SeededRng::from_stardew_seed([
                    self.days_played as f64,
                    (self.game_id / 2) as f64,
                    time as f64,
                    self.may_contain_bubbles.width() as f64,
                ]);

                if let Some((tile, (start_hour, start_minute))) =
                    current_bubbles
                {
                    let elapsed_minutes =
                        (hour - start_hour) * 60 + (minute - start_minute);
                    let stop_chance = 0.1 + (elapsed_minutes as f32) / 1800.0;
                    let frenzy = false; // TODO: Predict fish-frenzies.
                    let min_duration = if frenzy { 120 } else { 60 };
                    let bubbles_stop = rng.rand_float() < stop_chance
                        && elapsed_minutes > min_duration;
                    if bubbles_stop {
                        let start_time = start_hour * 100 + start_minute;
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
                            let width = self.may_contain_bubbles.width() as i32;
                            let i = rng.rand_in_range(0..width) as isize;

                            let height =
                                self.may_contain_bubbles.height() as i32;
                            let j = rng.rand_in_range(0..height) as isize;

                            Vector::<isize>::new(i, j)
                        })
                        .filter(|_| start_bubbles)
                        .find(|tile| self.may_contain_bubbles.is_set(*tile));

                    if let Some(tile) = opt_new_bubbles {
                        current_bubbles = Some((tile, (hour, minute)));
                    }
                }
            });

        if let Some((tile, (start_hours, start_minutes))) = current_bubbles {
            let start_time = start_hours * 100 + start_minutes;
            let end_time = 2600;
            results.push(FishingBubblePrediction {
                tile,
                duration: start_time..end_time,
            });
        }

        results
    }
}

use std::collections::HashSet;

use geometry::Vector;

use crate::{Error, GameAction, LocationExt as _, MovementGoal};
use game_state::{GameState, ItemId, ObjectKind, SeededRng};

use super::{
    InventoryGoal, MaintainStaminaGoal, UseItemOnTile,
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
};

pub struct ClayFarmingGoal {
    // TODO: Extract these out into a more general handling that can
    // be applied to any goal.  Will require moving the cleanup
    // (e.g. releasing keys/buttons, exiting menus) outside of the
    // goal, since an interrupted goal wouldn't be able to clean up
    // after itself.
    stop_at_stamina: f32,
    stop_at_time: Option<i32>,
    hard_stop: bool,

    /// The path to be followed in order to pick up the remaining
    /// clay.  If `None`, the path should be populated.  If `Some`,
    /// then the points on the path should be popped off one by one
    /// and followed.  If the vector is empty, the cleanup has been
    /// completed and the bot should be near the exit of the beach.
    cleanup_path: Option<Vec<Vector<isize>>>,
}

pub struct ClayPredictor {
    game_id: u64,
    days_played: u32,
    dirt_hoed: u32,
}

impl ClayPredictor {
    pub fn new(game_state: &GameState) -> Self {
        let dirt_hoed = game_state.globals.get_stat("dirtHoed").unwrap_or(0);
        let days_played =
            game_state.globals.get_stat("daysPlayed").unwrap_or(1);
        let game_id = game_state.globals.game_id;
        Self {
            game_id,
            days_played,
            dirt_hoed,
        }
    }

    fn will_produce_clay_with_offset(
        &self,
        tile: Vector<isize>,
        offset_dirt_hoed: u32,
    ) -> bool {
        let mut rng = SeededRng::from_stardew_seed([
            self.days_played as f64,
            (self.game_id / 2) as f64,
            (tile.right * 2000) as f64,
            (tile.down * 77) as f64,
            (self.dirt_hoed + offset_dirt_hoed) as f64,
        ]);
        rng.rand_float() < 0.03
    }

    /// Returns true if the tile will produce clay if hoed.
    pub fn will_produce_clay(&self, tile: Vector<isize>) -> bool {
        self.will_produce_clay_with_offset(tile, 0)
    }

    /// Returns an iterator that returns whether the tile will produce
    /// clay at some point in the future.
    ///
    /// The first element of the iterator will be the same as
    /// `self.will_produce_clay(tile)`.  Element i of the iterator
    /// indicates if the tile will produce clay if the hoe is used
    /// (i-1) times elsewhere, with the i-th usage then being on the
    /// tile itself.
    pub fn iter_will_produce_clay(
        &self,
        tile: Vector<isize>,
    ) -> impl Iterator<Item = bool> + '_ {
        (0..)
            .map(move |offset| self.will_produce_clay_with_offset(tile, offset))
    }
}

impl Default for ClayFarmingGoal {
    fn default() -> Self {
        Self::new()
    }
}

impl ClayFarmingGoal {
    pub fn new() -> Self {
        Self {
            stop_at_stamina: 2.0,
            stop_at_time: None,
            hard_stop: false,
            cleanup_path: None,
        }
    }

    pub fn stop_at_stamina(self, stamina: f32) -> Self {
        Self {
            stop_at_stamina: stamina,
            ..self
        }
    }

    pub fn stop_at_time(self, time: i32) -> Self {
        Self {
            stop_at_time: Some(time),
            ..self
        }
    }

    pub fn hard_stop(self) -> Self {
        Self {
            hard_stop: true,
            ..self
        }
    }

    pub fn done_digging(&self, game_state: &GameState) -> bool {
        let after_stopping_time = self
            .stop_at_time
            .map(|time| game_state.globals.in_game_time >= time)
            .unwrap_or(false);
        let below_stopping_stamina =
            game_state.player.current_stamina <= self.stop_at_stamina;

        after_stopping_time || below_stopping_stamina
    }

    pub fn clay_to_pick_up(
        &mut self,
        game_state: &GameState,
    ) -> Option<Vector<f32>> {
        if self.hard_stop || game_state.player.room_name != "Beach" {
            return None;
        }
        if self.cleanup_path.is_none() {
            let beach = game_state.get_room("Beach").ok()?;
            let pathfinding = beach.pathfinding(&game_state.statics);

            let player_tile = game_state.player.tile();
            let player_distance = pathfinding.distances(player_tile);
            let exit_tile = beach
                .warps
                .iter()
                .map(|warp| warp.location)
                .map(|tile| Vector::new(tile.right, tile.down.max(0)))
                .filter(|tile| player_distance.is_some(*tile))
                .min_by_key(|tile| (tile.down, tile.right))
                .expect("Exit tile not reachable");
            let exit_distance = pathfinding.distances(exit_tile);

            let points: Vec<Vector<isize>> = std::iter::empty()
                .chain([player_tile])
                .chain(
                    beach
                        .items
                        .iter()
                        .filter(|floating_item| {
                            floating_item.id == ItemId::CLAY
                        })
                        .map(|item| {
                            (item.position / 64.0).map(|x| x.round() as isize)
                        })
                        .filter(|&tile| {
                            player_distance.get_opt(tile).is_some()
                                && exit_distance.get_opt(tile).is_some()
                        }),
                )
                .chain([exit_tile])
                .collect();

            let distance_matrix: Vec<u64> = points
                .iter()
                .flat_map(|from| {
                    let dist_map = pathfinding.scaled_distances(*from);
                    points.iter().map(move |to| {
                        dist_map
                            .get_opt(*to)
                            .cloned()
                            .expect("All points should be reachable")
                    })
                })
                .collect();
            let n = points.len();
            let get_dist =
                |i: usize, j: usize| -> u64 { distance_matrix[j * n + i] };

            let mut path: Vec<usize> = (0..n).rev().collect();
            // Initial greedy path
            for i in 1..(n - 1) {
                let prev = path[i - 1];
                let j = (i..(n - 1))
                    .min_by_key(|j| get_dist(prev, path[*j]))
                    .expect("At least one point exists");
                path.swap(i, j);
            }

            for _ in 0..3 {
                // Check pairs of edges, and see whether reversing the
                // path between those edges would be beneficial.  (N^2
                // for number of edges, typically 30-ish pieces of
                // clay to pick up.)
                //
                // Technically, I'd then repeat the pairwise checks
                // until converged, but I want an upper bound on the
                // compute time, so I limit it to 3 passes.  In
                // practice, it seems to finish after 1-2 passes
                // anyways.
                //
                // This takes longer to plan than just walking toward
                // the closest piece of clay, but only for the first
                // frame of the bot.  Spending the extra 20-25 ms to
                // plan the route does miss a few frames, but makes
                // the overall collection be much consistently faster.
                let mut made_swap = false;

                for i in 1..(n - 2) {
                    for j in (i + 1)..(n - 1) {
                        // Before: A - B - (mid) - C - D
                        // After: A - C - (mid reversed) - B - D

                        let a = path[i - 1];
                        let b = path[i];
                        let c = path[j];
                        let d = path[j + 1];
                        let before = get_dist(a, b) + get_dist(c, d);
                        let after = get_dist(a, c) + get_dist(b, d);
                        if after < before {
                            path[i..=j].reverse();
                            made_swap = true;
                        }
                    }
                }

                if !made_swap {
                    break;
                }
            }

            self.cleanup_path =
                Some(path.into_iter().map(|i| points[i]).collect());
        }

        self.cleanup_path
            .as_mut()
            .expect("Just populated cleanup_path")
            .pop()
            .map(|tile| Vector::new(tile.right as f32, tile.down as f32) * 64.0)
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        self.done_digging(game_state)
            && (self.hard_stop
                || game_state.player.room_name != "Beach"
                || game_state
                    .get_room("Beach")
                    .ok()
                    .into_iter()
                    .flat_map(|loc| loc.items.iter())
                    .all(|floating_item| floating_item.id != ItemId::CLAY))
    }
}

impl BotGoal for ClayFarmingGoal {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Clay farming".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        if self.done_digging(game_state) {
            let finalizing =
                if let Some(clay_item_pos) = self.clay_to_pick_up(game_state) {
                    let goal = MovementGoal::new("Beach", clay_item_pos / 64.0)
                        .with_tolerance(1.0);
                    if goal.is_completed(game_state) {
                        BotGoalResult::InProgress
                    } else {
                        goal.into()
                    }
                } else {
                    BotGoalResult::Completed
                };
            return Ok(finalizing);
        }

        let beach = game_state.get_room("Beach")?;
        let player = &game_state.player;

        // Pick up the hoe for use in the rest of the goal.
        let goal = InventoryGoal::current()
            .with(ItemId::HOE)
            .with(ItemId::PICKAXE);
        if !goal.is_completed(game_state)? {
            return Ok(goal.into());
        }

        // Go to the Beach
        //
        // TODO: Expose the room-to-room movement without requiring
        // local movement afterwards.  In this case, I want to go to
        // the farm, but don't care where I start out within the farm.
        if player.room_name != "Beach" {
            return Ok(MovementGoal::new("Beach", Vector::new(38.0, 1.0))
                .with_tolerance(100.0)
                .into());
        }

        let goal = MaintainStaminaGoal::new();
        if !goal.is_completed(game_state)? {
            return Ok(goal.into());
        }

        let player_tile = player.tile();

        let clay_predictor = ClayPredictor::new(game_state);

        // The set of tiles that will yield clay if the hoe is used
        // once elsewhere, and then once on the tile.  This is used to
        // avoid needing to re-predict the (N+1)-th clay tiles for
        // each possible choice of the N-th clay tile.
        let next_clay_tiles: Vec<Vector<isize>> = beach
            .diggable
            .iter_true()
            .filter(|tile| {
                clay_predictor.will_produce_clay_with_offset(*tile, 1)
            })
            .collect();

        let current_hoe_dirt: HashSet<Vector<isize>> = beach
            .objects
            .iter()
            .filter(|obj| matches!(obj.kind, ObjectKind::HoeDirt(_)))
            .map(|obj| obj.tile)
            .collect();

        /// If the closest tile would require using the pickaxe to
        /// clear out HoeDirt before using the Hoe, it may be
        /// better to go to a different tile.  This value
        /// determines how far the bot will go out of its way in
        /// order to find a tile that can be hoed without first
        /// requiring a use of the pickaxe.
        const TILES_TO_AVOID_PICKAXE: u64 = 10;

        let distances = beach
            .pathfinding(&game_state.statics)
            .distances(player_tile);

        let opt_closest_clay: Option<Vector<isize>> = beach
            .diggable
            .iter_true()
            .filter(|tile| distances.is_some(*tile))
            .filter(|tile| clay_predictor.will_produce_clay(*tile))
            .min_by_key(|tile| {
                let dist =
                    distances[*tile].expect("Protected by distances.is_some()");
                let pickaxe_penalty = if current_hoe_dirt.contains(tile) {
                    TILES_TO_AVOID_PICKAXE
                } else {
                    0
                };
                let next_dist = next_clay_tiles
                    .iter()
                    .map(|next| {
                        let offset =
                            (*tile - *next).map(|x| x.unsigned_abs() as u64);
                        let pickaxe_penalty = if next == tile
                            || current_hoe_dirt.contains(next)
                        {
                            TILES_TO_AVOID_PICKAXE
                        } else {
                            0
                        };
                        offset.right + offset.down + pickaxe_penalty
                    })
                    .min()
                    .unwrap_or(0);

                dist + pickaxe_penalty + next_dist
            });

        let Some(closest_clay) = opt_closest_clay else {
            // There exists a clay tile, but we can't reach it.
            return Ok(BotGoalResult::Completed);
        };

        actions.do_action(GameAction::MouseOverTile(closest_clay));
        let tool = if current_hoe_dirt.contains(&closest_clay) {
            ItemId::PICKAXE
        } else {
            ItemId::HOE
        };

        let total_dirt_hoed =
            game_state.globals.get_stat("dirtHoed").unwrap_or(0);

        let goal = UseItemOnTile::new(tool, "Beach", closest_clay).cancel_if(
            move |game_state| {
                game_state.globals.get_stat("dirtHoed").unwrap_or(0)
                    != total_dirt_hoed
            },
        );
        Ok(goal.into())
    }
}

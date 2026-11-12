use std::collections::{HashMap, HashSet};

use geometry::{Rectangle, Vector};

use crate::{Error, GameAction, GoToRoomGoal, LocationExt as _, MovementGoal};
use game_state::{GameState, ItemId, ObjectKind, SeededRng};

use super::{
    InventoryGoal, MaintainStaminaGoal, UseItemOnTile,
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
};

pub struct ClayFarmingGoal {
    /// The room in which to farm clay.  Defaults to "Beach".
    room: String,

    /// If the closest tile would require using a tool, either to
    /// clear out previous HoeDirt, or to clear out clutter before
    /// using the Hoe, it may be better to go to a different tile.
    /// This value determines how far the bot will go out of its
    /// way in order to find a tile that can be hoed without first
    /// requiring a use of a tool.  Defaults to 10.
    ///
    /// For example, if the bot is 15 tiles away from an tile that
    /// would produce clay, but 2 tiles away from a tile with existing
    /// HoeDirt that would produce clay, then it's worth using the
    /// pickaxe to clear out the existing HoeDirt, spending more
    /// stamina in order to spend less time.
    distance_for_clear_tile: u64,

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
    /// completed and the bot should be near the room's exit.
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
            room: "Beach".into(),
            distance_for_clear_tile: 10,
        }
    }

    pub fn room(self, room: impl Into<String>) -> Self {
        Self {
            room: room.into(),
            ..self
        }
    }

    pub fn distance_for_clear_tile(self, distance_for_clear_tile: u64) -> Self {
        Self {
            distance_for_clear_tile,
            ..self
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

    fn iter_reachable_clay<'a>(
        &self,
        game_state: &'a GameState,
    ) -> Result<impl Iterator<Item = Vector<isize>> + 'a, Error> {
        let loc = game_state.get_room(&self.room)?;
        let pathfinding = loc.pathfinding(&game_state.statics);

        let player_tile = game_state.player.tile();
        let player_distance = pathfinding.distances_forward(player_tile);
        let exit_tile = loc
            .warps
            .iter()
            .map(|warp| warp.location)
            .map(|tile| Vector::new(tile.right, tile.down.max(0)))
            .filter(|tile| player_distance.is_some(*tile))
            .min_by_key(|tile| (tile.down, tile.right))
            .expect("Exit tile not reachable");
        let exit_distance = pathfinding.distances_reverse(exit_tile);

        let iter = loc
            .items
            .iter()
            .filter(|floating_item| floating_item.id == ItemId::CLAY)
            .map(|item| (item.position / 64.0).map(|x| x.round() as isize))
            .filter(move |&tile| {
                player_distance.get_opt(tile).is_some()
                    && exit_distance.get_opt(tile).is_some()
            });
        Ok(iter)
    }

    pub fn clay_to_pick_up(
        &mut self,
        game_state: &GameState,
    ) -> Result<Option<Vector<f32>>, Error> {
        if self.hard_stop || game_state.player.room_name != self.room {
            return Ok(None);
        }
        if self.cleanup_path.is_none() {
            let loc = game_state.get_room(&self.room)?;
            let pathfinding = loc.pathfinding(&game_state.statics);

            let player_tile = game_state.player.tile();
            let player_distance = pathfinding.distances_forward(player_tile);
            let exit_tile = loc
                .warps
                .iter()
                .map(|warp| warp.location)
                .map(|tile| Vector::new(tile.right, tile.down.max(0)))
                .filter(|tile| player_distance.is_some(*tile))
                .min_by_key(|tile| (tile.down, tile.right))
                .expect("Exit tile not reachable");

            let points: Vec<Vector<isize>> = std::iter::empty()
                .chain([player_tile])
                .chain(self.iter_reachable_clay(game_state)?)
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

            // Having the player's location and the exit location in
            // the path makes the bookkeeping a lot easier.  However,
            // now that we've finished, strip those points off.  The
            // player's location isn't necessary since that movement
            // goal would already be completed.  The exit location
            // isn't necessary since whichever goal occurs next would
            // need to go to the exit anyways.
            self.cleanup_path = Some(
                path[1..(n - 1)].into_iter().map(|i| points[*i]).collect(),
            );
        }

        Ok(self
            .cleanup_path
            .as_mut()
            .expect("Just populated cleanup_path")
            .pop()
            .map(|tile| {
                Vector::new(tile.right as f32, tile.down as f32) * 64.0
            }))
    }

    pub fn is_completed(&self, game_state: &GameState) -> Result<bool, Error> {
        Ok(self.done_digging(game_state)
            && (self.hard_stop
                || game_state.player.room_name != self.room
                || self.iter_reachable_clay(game_state)?.next().is_none()))
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
            let finalizing = if let Some(clay_item_pos) =
                self.clay_to_pick_up(game_state)?
            {
                let goal =
                    MovementGoal::new(self.room.clone(), clay_item_pos / 64.0)
                        .with_tolerance(1.0);
                if goal.is_completed(game_state) {
                    BotGoalResult::InProgress
                } else {
                    let room = self.room.clone();
                    goal.cancel_if(move |game_state| {
                        game_state
                            .get_room(&room)
                            .ok()
                            .into_iter()
                            .flat_map(|loc| loc.items.iter())
                            .filter(|floating_item| {
                                floating_item.id == ItemId::CLAY
                            })
                            .map(|item| item.position.dist(clay_item_pos))
                            .all(|dist| dist > 1.5 * 64.0)
                    })
                    .into()
                }
            } else {
                BotGoalResult::Completed
            };
            return Ok(finalizing);
        }

        let loc = game_state.get_room(&self.room)?;
        let player = &game_state.player;

        // Pick up the hoe for use in the rest of the goal.
        let goal = InventoryGoal::current()
            .with(ItemId::HOE)
            .with(ItemId::PICKAXE);
        if !goal.is_completed(game_state)? {
            return Ok(goal.into());
        }

        // Go to the correct location for the clay farming.
        let goal = GoToRoomGoal::new(self.room.clone());
        if !goal.is_completed(game_state) {
            return Ok(goal.into());
        }

        let goal = MaintainStaminaGoal::new();
        if !goal.is_completed(game_state)? {
            return Ok(goal.into());
        }

        let player_tile = player.tile();

        let clay_predictor = ClayPredictor::new(game_state);

        // Tiles that we shouldn't touch as part of this clay-farming.
        // For example, when clay-farming on the Farm, we shouldn't
        // dig up any seeds that we've already planted.
        let mut forbidden = HashSet::<Vector<isize>>::new();
        // The tool that must be used on the tile prior to digging it
        // up for clay.  For example, using the pickaxe in order to
        // clear out stone, or clearing out previous empty hoe dirt.
        let mut pre_hoe_tool = HashMap::<Vector<isize>, ItemId>::new();
        for obj in &loc.objects {
            match &obj.kind {
                ObjectKind::Tree(_)
                | ObjectKind::FruitTree(_)
                | ObjectKind::Chest(_)
                | ObjectKind::CraftingMachine(_)
                | ObjectKind::Sprinkler(_)
                | ObjectKind::Scarecrow
                | ObjectKind::Unknown => {
                    forbidden.insert(obj.tile);
                }
                ObjectKind::HoeDirt(dirt) if dirt.crop.is_some() => {
                    forbidden.insert(obj.tile);
                }
                ObjectKind::HoeDirt(dirt) if dirt.crop.is_none() => {
                    pre_hoe_tool.insert(obj.tile, ItemId::PICKAXE);
                }
                ObjectKind::Stone(_) => {
                    pre_hoe_tool.insert(obj.tile, ItemId::PICKAXE);
                }
                ObjectKind::Wood => {
                    pre_hoe_tool.insert(obj.tile, ItemId::AXE);
                }
                ObjectKind::Fiber | ObjectKind::Grass => {
                    pre_hoe_tool.insert(obj.tile, ItemId::SCYTHE);
                }
                _ => {}
            }
        }
        std::iter::empty()
            .chain(loc.buildings.iter().map(|b| match b.kind.as_str() {
                "Greenhouse" => Rectangle {
                    top_left: b.shape.top_left + Vector::new(-1, 0),
                    shape: Vector::new(9, 8),
                },
                _ => b.shape,
            }))
            .chain(loc.resource_clumps.iter().map(|c| c.shape))
            .chain(loc.bushes.iter().map(|b| b.rectangle()))
            .flat_map(|shape| shape.iter_points())
            .for_each(|tile| {
                forbidden.insert(tile);
            });

        // The set of tiles that will yield clay if the hoe is used
        // once elsewhere, and then once on the tile.  This is used to
        // avoid needing to re-predict the (N+1)-th clay tiles for
        // each possible choice of the N-th clay tile.
        let next_clay_tiles: Vec<Vector<isize>> = loc
            .diggable
            .iter_true()
            .filter(|tile| !forbidden.contains(tile))
            .filter(|tile| {
                clay_predictor.will_produce_clay_with_offset(*tile, 1)
            })
            .collect();

        let apply_pre_clear_penalty = |tile: Vector<isize>| -> bool {
            if let Some(tool) = pre_hoe_tool.get(&tile)
                && *tool != ItemId::SCYTHE
            {
                true
            } else {
                false
            }
        };

        let distances = loc
            .pathfinding(&game_state.statics)
            .distances_forward(player_tile);

        let opt_closest_clay: Option<Vector<isize>> = loc
            .diggable
            .iter_true()
            .filter(|tile| distances.is_some(*tile))
            .filter(|tile| !forbidden.contains(tile))
            .filter(|tile| clay_predictor.will_produce_clay(*tile))
            .min_by_key(|tile| {
                let dist =
                    distances[*tile].expect("Protected by distances.is_some()");

                let pickaxe_penalty = if apply_pre_clear_penalty(*tile) {
                    self.distance_for_clear_tile
                } else {
                    0
                };
                let next_dist = next_clay_tiles
                    .iter()
                    .map(|next| {
                        let offset =
                            (*tile - *next).map(|x| x.unsigned_abs() as u64);
                        let pickaxe_penalty =
                            if next == tile || apply_pre_clear_penalty(*next) {
                                self.distance_for_clear_tile
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
        let tool = pre_hoe_tool
            .get(&closest_clay)
            .cloned()
            .unwrap_or(ItemId::HOE);

        let total_dirt_hoed =
            game_state.globals.get_stat("dirtHoed").unwrap_or(0);

        let goal = UseItemOnTile::new(tool, self.room.clone(), closest_clay)
            .cancel_if(move |game_state| {
                game_state.globals.get_stat("dirtHoed").unwrap_or(0)
                    != total_dirt_hoed
            });
        Ok(goal.into())
    }
}

use std::collections::HashSet;

use geometry::Vector;

use crate::{Error, GameAction, LocationExt as _, MovementGoal};
use game_state::{GameState, ItemId, ObjectKind, SeededRng};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    InventoryGoal, MaintainStaminaGoal, UseItemOnTile,
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

impl ClayFarmingGoal {
    pub fn new() -> Self {
        Self {
            stop_at_stamina: 2.0,
            stop_at_time: None,
            hard_stop: false,
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
        &self,
        game_state: &GameState,
    ) -> Option<Vector<f32>> {
        if self.hard_stop || game_state.player.room_name != "Beach" {
            return None;
        }

        let beach = game_state.get_room("Beach").ok()?;
        let pathfinding = beach.pathfinding(&game_state.statics);

        let player_tile = game_state.player.tile();
        let player_distance = pathfinding.distances(player_tile);

        let exit_tile = beach
            .warps
            .iter()
            .map(|warp| warp.location)
            .map(|tile| {
                let offset = if tile.down == -1 {
                    Vector::new(1, 0)
                } else {
                    Vector::zero()
                };
                tile + offset
            })
            .filter(|tile| player_distance.is_some(*tile))
            .min_by_key(|tile| (tile.down, tile.right));
        let exit_distance = pathfinding.distances(exit_tile);

        beach
            .items
            .iter()
            .filter(|floating_item| floating_item.id == ItemId::CLAY)
            .min_by_key(|item| {
                let tile = (item.position / 64.0).map(|x| x.round() as isize);
                match (
                    player_distance.get_opt(tile),
                    exit_distance.get_opt(tile),
                ) {
                    (Some(from_player), Some(from_exit)) => {
                        (*from_player as i64) - (*from_exit as i64)
                    }
                    _ => i64::MAX,
                }
            })
            .map(|item| item.position)
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        self.done_digging(game_state)
            && self.clay_to_pick_up(game_state).is_none()
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
                        let offset = (*tile - *next).map(|x| x.abs() as u64);
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

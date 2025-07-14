use std::collections::HashSet;

use geometry::Vector;
use itertools::Itertools as _;

use crate::{
    bot_logic::{
        bot_logic::LogicStack, graph_search::GraphSearch as _, BotError,
        LocationExt as _, MovementGoal, SelectItemGoal,
    },
    Error, GameAction, GameState,
};
use game_state::{
    Inventory, Item, ItemId, Key, ObjectKind, Quality, SeededRng,
};

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

        let player_loc = game_state.player.center_pos();
        game_state
            .get_room("Beach")
            .into_iter()
            .flat_map(|loc| loc.items.iter())
            .filter(|floating_item| floating_item.id == ItemId::CLAY)
            .min_by_key(|item| {
                (item.position / 64.0).dist2(player_loc) as isize
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
        if !goal.is_completed(game_state) {
            return Ok(goal.into());
        }

        let player_tile = player.tile();

        let total_dirt_hoed =
            game_state.globals.get_stat("dirtHoed").unwrap_or(0);

        let clay_predictor = ClayPredictor::new(game_state);
        let clay_tiles: HashSet<Vector<isize>> = beach
            .diggable
            .iter()
            .filter(|(_, is_diggable)| **is_diggable)
            .map(|(tile, _)| tile)
            .filter(|tile| clay_predictor.will_produce_clay(*tile))
            .collect();

        let has_hoe_dirt = |tile: Vector<isize>| -> bool {
            beach
                .objects
                .iter()
                .filter(|obj| matches!(obj.kind, ObjectKind::HoeDirt(_)))
                .any(|obj| obj.tile == tile)
        };

        let opt_closest_clay: Option<Vector<isize>> = 'clay_tile: {
            /// If the closest tile would require using the pickaxe to
            /// clear out HoeDirt before using the Hoe, it may be
            /// better to go to a different tile.  This value
            /// determines how far the bot will go out of its way in
            /// order to find a tile that can be hoed without first
            /// requiring a use of the pickaxe.
            const TILES_TO_AVOID_PICKAXE: u64 = 10;

            let mut clay_with_pickaxe: Option<(u64, Vector<isize>)> = None;
            for (tile, dist) in beach.pathfinding().iter_dijkstra(player_tile) {
                if clay_tiles.contains(&tile) {
                    if has_hoe_dirt(tile) {
                        // This tile will produce clay, but would
                        // first require using the pickaxe.
                        clay_with_pickaxe.get_or_insert((dist, tile));
                    } else {
                        // This tile will produce clay without
                        // requiring the pickaxe.
                        break 'clay_tile Some(tile);
                    }
                }

                if let Some((prev_dist, prev_tile)) = clay_with_pickaxe {
                    if dist > prev_dist + TILES_TO_AVOID_PICKAXE {
                        // We've searched all tiles that may produce
                        // clay without requiring a pickaxe, and are
                        // within the configured range.  Therefore,
                        // settle for the previous result, even though
                        // it will require using the pickaxe.
                        break 'clay_tile Some(prev_tile);
                    }
                }
            }

            clay_with_pickaxe.map(|(_, tile)| tile)
        };

        let Some(closest_clay) = opt_closest_clay else {
            // There exists a clay tile, but we can't reach it.
            return Ok(BotGoalResult::Completed);
        };

        actions.do_action(GameAction::MouseOverTile(closest_clay));
        let tool = if has_hoe_dirt(closest_clay) {
            ItemId::PICKAXE
        } else {
            ItemId::HOE
        };

        let goal = UseItemOnTile::new(tool, "Beach", closest_clay).cancel_if(
            move |game_state| {
                game_state.globals.get_stat("dirtHoed").unwrap_or(0)
                    != total_dirt_hoed
            },
        );
        Ok(goal.into())
    }
}

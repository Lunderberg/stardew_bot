use std::collections::HashSet;

use itertools::Itertools as _;

use crate::{
    bot_logic::{
        bot_logic::SubGoals, graph_search::GraphSearch as _, BotError,
        MovementGoal, SelectItemGoal,
    },
    game_state::{
        Inventory, Item, Key, ObjectKind, Quality, SeededRng, Vector,
    },
    Direction, Error, GameAction, GameState,
};

use super::{
    bot_logic::{BotGoal, BotGoalResult},
    RecoverStaminaGoal,
};

pub struct ClayFarmingGoal {
    // TODO: Extract these out into a more general handling that can
    // be applied to any goal.  Will require moving the cleanup
    // (e.g. releasing keys/buttons, exiting menus) outside of the
    // goal, since an interrupted goal wouldn't be able to clean up
    // after itself.
    stop_at_stamina: f32,
    stop_at_time: Option<i32>,
}

impl ClayFarmingGoal {
    pub fn new() -> Self {
        Self {
            stop_at_stamina: 4.0,
            stop_at_time: None,
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
        let clay_id = "(O)330";
        let player_loc = game_state.player.center_pos();
        game_state
            .get_room("Beach")
            .into_iter()
            .flat_map(|loc| loc.items.iter())
            .filter(|floating_item| floating_item.item.item_id == clay_id)
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
    fn description(&self) -> std::borrow::Cow<str> {
        "Clay farming".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        do_action: &mut dyn FnMut(GameAction),
    ) -> Result<BotGoalResult, Error> {
        if self.done_digging(game_state) {
            let finalizing =
                if let Some(clay_item_pos) = self.clay_to_pick_up(game_state) {
                    let goal = MovementGoal::new("Beach", clay_item_pos / 64.0)
                        .with_tolerance(0.5);
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

        if player.using_tool
            && player.last_click == Vector::zero()
            && player
                .selected_item()
                .map(|item| item.item_id.starts_with("(T)"))
                .unwrap_or(false)
        {
            do_action(GameAction::AnimationCancel);
        }

        // Pick up the hoe for use in the rest of the goal.
        let hoe = Item::new("(T)Hoe");
        let pickaxe = Item::new("(T)Pickaxe");
        for item in [&hoe, &pickaxe] {
            let goal = super::InventoryGoal::new(item.clone());
            if !goal.contains_target_item(&player.inventory) {
                return Ok(goal.into());
            }
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

        let player_tile = player.tile();

        let total_dirt_hoed =
            game_state.globals.get_stat("dirtHoed").unwrap_or(0);

        let days_played = game_state.globals.get_stat("daysPlayed")?;

        let clay_tiles: HashSet<Vector<isize>> = beach
            .diggable
            .iter()
            .filter(|(_, is_diggable)| **is_diggable)
            .map(|(tile, _)| tile)
            .filter(|tile| {
                let mut rng = SeededRng::from_stardew_seed([
                    days_played as f64,
                    (game_state.globals.unique_id / 2) as f64,
                    (tile.right * 2000) as f64,
                    (tile.down * 77) as f64,
                    total_dirt_hoed as f64,
                ]);
                rng.rand_float() < 0.03
            })
            .collect();

        let has_hoe_dirt = |tile: Vector<isize>| -> bool {
            beach
                .objects
                .iter()
                .filter(|obj| matches!(obj.kind, ObjectKind::HoeDirt(_)))
                .any(|obj| obj.tile == tile)
        };

        let clear_tiles = beach.collect_clear_tiles();

        let opt_closest_clay: Option<Vector<isize>> = 'clay_tile: {
            /// If the closest tile would require using the pickaxe to
            /// clear out HoeDirt before using the Hoe, it may be
            /// better to go to a different tile.  This value
            /// determines how far the bot will go out of its way in
            /// order to find a tile that can be hoed without first
            /// requiring a use of the pickaxe.
            const TILES_TO_AVOID_PICKAXE: u64 = 10;

            let mut clay_with_pickaxe: Option<(u64, Vector<isize>)> = None;
            for (tile, metadata) in clear_tiles.dijkstra_search(player_tile) {
                let dist = metadata.initial_to_node;

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
                    if dist > prev_dist + 2 * TILES_TO_AVOID_PICKAXE {
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

        let opt_movement = match player_tile.manhattan_dist(closest_clay) {
            0 => {
                // Currently standing on the clay tile.  Move to an
                // adjacent tile
                let dir = Direction::iter()
                    .filter(|dir| dir.is_cardinal())
                    .filter(|dir| clear_tiles[player_tile + dir.offset()])
                    .sorted_by(|&dir_a, &dir_b| {
                        let dot_product = |dir: Direction| {
                            let tile_center = closest_clay.map(|x| x as f32);
                            let dir_offset = dir.offset().map(|x| x as f32);

                            (tile_center - player.center_pos()).dot(dir_offset)
                        };
                        dot_product(dir_a).total_cmp(&dot_product(dir_b))
                    })
                    .next()
                    .expect("At least one direction should be accessible");
                Some(MovementGoal::new(
                    "Beach",
                    (closest_clay + dir.offset()).into(),
                ))
            }
            1 => None,
            _ => {
                // Move to within range of the clay tile
                Some(
                    MovementGoal::new("Beach", closest_clay.into())
                        .with_tolerance(1.1),
                )
            }
        };
        if let Some(movement) = opt_movement {
            return Ok(movement.into());
        }

        if game_state.player.current_stamina < 200.0 {
            let goal = RecoverStaminaGoal::new();
            if goal.item_to_eat(game_state).is_some() {
                return Ok(goal.into());
            }
        }

        do_action(GameAction::MouseOverTile(closest_clay));
        let tool = if has_hoe_dirt(closest_clay) {
            pickaxe
        } else {
            hoe
        };

        let select_tool = SelectItemGoal::new(tool);
        if !select_tool.is_completed(game_state) {
            return Ok(select_tool.into());
        }

        // Swinging a tool by clicking the mouse uses the previous
        // frame's mouse position, not the current mouse position.
        // While both the MovementGoal and the `MouseOverTile`
        // prior to `SelectItemGoal` try to preemptively move the
        // cursor, if neither substep was necessary then the
        // cursor may not have been updated yet.
        if !player.using_tool
            && closest_clay == game_state.inputs.mouse_tile_location
        {
            do_action(GameAction::LeftClick);
        }

        Ok(BotGoalResult::InProgress)
    }
}

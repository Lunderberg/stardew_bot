use std::collections::{HashMap, HashSet};

use itertools::Itertools as _;

use crate::{
    bot_logic::{ActivateTile, InventoryGoal, MovementGoal, UseItemOnTile},
    game_state::{Item, ObjectKind, Vector},
    Error, GameAction, GameState,
};

use super::{
    bot_logic::{BotGoal, BotGoalResult},
    GameStateExt as _,
};

pub struct ExpandTreeFarm;

impl ExpandTreeFarm {
    pub fn new() -> Self {
        Self
    }
}

impl BotGoal for ExpandTreeFarm {
    fn description(&self) -> std::borrow::Cow<str> {
        "Expand Tree Farm".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        do_action: &mut dyn FnMut(GameAction),
    ) -> Result<BotGoalResult, Error> {
        let farm = game_state.get_room("Farm")?;
        let farm_door = game_state.get_farm_door()?;
        let crops_top_right = farm_door + Vector::new(3, 5);
        let tree_top_right = crops_top_right + Vector::new(0, 12);

        let seed_types = [Item::OAK_SEED, Item::MAPLE_SEED, Item::PINE_SEED];

        let opt_nearby_seed = {
            let pos = game_state.player.center_pos();
            game_state
                .current_room()?
                .items
                .iter()
                .filter(|item| {
                    seed_types
                        .iter()
                        .any(|seed_type| item.item_id == seed_type.item_id)
                })
                .map(|item| item.position / 64.0 - pos)
                .filter(|offset| offset.mag2() < 3.0 * 3.0)
                .min_by(|a, b| {
                    num::traits::float::TotalOrder::total_cmp(
                        &a.mag2(),
                        &b.mag2(),
                    )
                })
        };
        if let Some(offset) = opt_nearby_seed {
            do_action(GameAction::Move(offset.closest_direction()));
            return Ok(BotGoalResult::InProgress);
        }

        let pathfinding = farm.pathfinding().include_border(true);

        let reachable = pathfinding.reachable(farm_door);

        let seed_loc: HashMap<Vector<isize>, Option<Item>> = farm
            .objects
            .iter()
            .filter(|obj| reachable[obj.tile])
            .filter_map(|obj| {
                let opt_tool = match &obj.kind {
                    ObjectKind::Tree(tree) if tree.growth_stage == 0 => {
                        Some(Item::AXE)
                    }
                    ObjectKind::Tree(tree) if tree.has_seed => None,

                    _ => {
                        return None;
                    }
                };
                Some((obj.tile, opt_tool))
            })
            .filter(|(tile, opt_tool)| {
                let offset = *tile - tree_top_right;
                let is_tree_farm_tile = offset.right <= 0
                    && offset.down >= 0
                    && offset.right % 2 == 0
                    && offset.down % 2 == 0;

                // If a seed is already planted in a location that
                // will become the tree farm, let it remain where it
                // is.
                let should_collect = opt_tool.is_none() || !is_tree_farm_tile;
                should_collect
            })
            .collect();

        let initial_tile = if game_state.player.room_name == "Farm" {
            game_state.player.tile()
        } else {
            farm_door
        };

        let opt_closest_seed = pathfinding
            .iter_dijkstra(initial_tile)
            .find_map(|(tile, _)| {
                seed_loc.get(&tile).map(|opt_tool| (tile, opt_tool))
            });
        if let Some((tile, opt_tool)) = opt_closest_seed {
            let goal = InventoryGoal::new(Item::AXE);
            if !goal.is_completed(game_state) {
                return Ok(goal.into());
            }

            if let Some(tool) = opt_tool {
                let goal = UseItemOnTile::new(tool.clone(), "Farm", tile);
                return Ok(goal.into());
            } else {
                let goal = ActivateTile::new("Farm", tile);
                return Ok(goal.into());
            }
        }

        let opt_seed_to_plant =
            game_state.player.inventory.iter_items().find(|item| {
                seed_types
                    .iter()
                    .any(|seed_type| seed_type.is_same_item(item))
            });
        if let Some(seed_to_plant) = opt_seed_to_plant {
            let reachable = farm.pathfinding().reachable(farm_door);
            let opt_tile_to_plant = (0..=tree_top_right.right)
                .rev()
                .step_by(2)
                .cartesian_product(
                    (tree_top_right.down..farm.shape.down).step_by(2),
                )
                .map(|(right, down)| Vector::new(right, down))
                .find(|tile| reachable[*tile]);
            if let Some(tile_to_plant) = opt_tile_to_plant {
                let goal = UseItemOnTile::new(
                    seed_to_plant.clone(),
                    "Farm",
                    tile_to_plant,
                );
                return Ok(goal.into());
            }
        }

        Ok(BotGoalResult::Completed)
    }
}

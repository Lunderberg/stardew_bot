use std::collections::{HashMap, HashSet};

use itertools::Itertools as _;

use crate::{
    bot_logic::{ActivateTile, InventoryGoal, MovementGoal, UseItemOnTile},
    game_state::{Item, ObjectKind, Vector},
    Error, GameAction, GameState,
};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    FarmPlan, GameStateExt as _,
};

pub struct ExpandTreeFarm;

impl ExpandTreeFarm {
    pub fn new() -> Self {
        Self
    }
}

impl BotGoal for ExpandTreeFarm {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Expand Tree Farm".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let plan = FarmPlan::plan(game_state)?;
        let farm = game_state.get_room("Farm")?;
        let farm_door = game_state.get_farm_door()?;

        let seed_types = [Item::OAK_SEED, Item::MAPLE_SEED, Item::PINE_SEED];

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
                    ObjectKind::Tree(tree)
                        if tree.has_seed && !tree.is_stump =>
                    {
                        None
                    }

                    _ => {
                        return None;
                    }
                };
                Some((obj.tile, opt_tool))
            })
            .filter(|(tile, opt_tool)| {
                let is_tree_farm_tile = plan.is_planned_tree(*tile);

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

        let opt_seed_to_grab = || {
            pathfinding
                .iter_dijkstra(initial_tile)
                .find_map(|(tile, _)| {
                    seed_loc
                        .get(&tile)
                        .map(|opt_tool| (tile, opt_tool.as_ref()))
                })
        };

        let opt_seed_to_plant = || {
            game_state
                .player
                .inventory
                .iter_items()
                .find(|item| {
                    seed_types
                        .iter()
                        .any(|seed_type| seed_type.is_same_item(item))
                })
                .and_then(|seed_to_plant| {
                    let mut valid_loc = farm.pathfinding().reachable(farm_door);
                    farm.objects
                        .iter()
                        .filter(|obj| matches!(obj.kind, ObjectKind::Tree(_)))
                        .map(|obj| obj.tile)
                        .for_each(|tile| {
                            if valid_loc.in_bounds(tile) {
                                valid_loc[tile] = false;
                            }
                        });

                    let opt_tile_to_plant = plan
                        .iter_planned_trees()
                        .find(|tile| valid_loc.is_set(*tile));
                    opt_tile_to_plant.map(|tile| (tile, Some(seed_to_plant)))
                })
        };

        if let Some((tile, opt_item)) =
            opt_seed_to_grab().or_else(opt_seed_to_plant)
        {
            let inventory = &game_state.player.inventory;
            if !inventory.contains(Item::AXE) || inventory.num_empty_slots() < 4
            {
                let goal = seed_types
                    .iter()
                    .map(|seed| seed.clone().with_count(100))
                    .fold(InventoryGoal::empty(), |goal, seed| goal.with(seed))
                    .with(Item::AXE)
                    .with(Item::HOE);
                return Ok(goal.into());
            }

            if let Some(item) = opt_item {
                let goal = UseItemOnTile::new(item.clone(), "Farm", tile);
                return Ok(goal.into());
            } else {
                let goal = ActivateTile::new("Farm", tile);
                return Ok(goal.into());
            }
        }

        Ok(BotGoalResult::Completed)
    }
}

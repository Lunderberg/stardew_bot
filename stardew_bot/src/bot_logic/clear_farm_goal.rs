use std::collections::{HashMap, HashSet};

use itertools::Itertools as _;

use crate::{
    bot_logic::{
        bot_logic::LogicStack, graph_search::GraphSearch as _, BotError,
        MovementGoal, SelectItemGoal,
    },
    game_state::{
        Inventory, Item, Key, Location, Object, ObjectKind, Quality, Vector,
    },
    Direction, Error, GameAction, GameState,
};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    GameStateExt as _, MaintainStaminaGoal, ObjectKindExt as _, Pathfinding,
    UseItemOnTile,
};

pub struct ClearFarmGoal {
    clear_trees: bool,
    clear_stone: bool,
    priority_tiles: HashSet<Vector<isize>>,
}

impl ClearFarmGoal {
    pub fn new() -> Self {
        Self {
            priority_tiles: HashSet::new(),
            clear_trees: false,
            clear_stone: true,
        }
    }

    #[allow(dead_code)]
    pub fn clear_trees(self, clear_trees: bool) -> Self {
        Self {
            clear_trees,
            ..self
        }
    }

    #[allow(dead_code)]
    pub fn clear_stone(self, clear_stone: bool) -> Self {
        Self {
            clear_stone,
            ..self
        }
    }

    pub fn is_completed(&self, game_state: &GameState) -> Result<bool, Error> {
        let farm = game_state.get_room("Farm")?;
        let farm_door = game_state.get_farm_door()?;

        let pathfinding = farm
            .pathfinding()
            .allow_diagonal(false)
            .include_border(true)
            .fiber_clearing_cost(1);

        let pathfinding = if game_state.player.current_stamina > 2.0 {
            pathfinding
                .stone_clearing_cost(10)
                .wood_clearing_cost(10)
                .tree_clearing_cost(50)
        } else {
            pathfinding
        };

        let reachable = pathfinding.reachable(farm_door);

        let reachable_clutter = farm
            .objects
            .iter()
            .any(|obj| reachable[obj.tile] && obj.kind.get_tool().is_some());

        Ok(!reachable_clutter)
    }

    fn pathfinding(farm: &Location) -> Pathfinding {
        farm.pathfinding()
            .allow_diagonal(false)
            .stone_clearing_cost(10)
            .wood_clearing_cost(10)
            .fiber_clearing_cost(1)
            .grass_movement_cost(1)
            .tree_clearing_cost(50)
    }

    fn fill_priority_tiles(
        &mut self,
        game_state: &GameState,
    ) -> Result<(), Error> {
        if !self.priority_tiles.is_empty() {
            return Ok(());
        }

        let farm = game_state.get_room("Farm")?;
        let farm_door = game_state.get_farm_door()?;

        let pathfinding = Self::pathfinding(farm).do_not_clear_trees();
        let reachable = pathfinding.reachable(farm_door);

        self.priority_tiles = farm
            .warps
            .iter()
            .filter(|warp| warp.target_room != "Greenhouse")
            .sorted_by_key(|warp| &warp.target_room)
            .flat_map(|warp| {
                // Technically, warps for screen transitions are just off
                // the edge of the map, and aren't actually reachable.
                // Instead, make sure that the path can reach any tiles
                // that is adjacent to a warp.
                warp.location.iter_cardinal()
            })
            .filter(|warp_tile| reachable.is_set(*warp_tile))
            .flat_map(|warp_tile| {
                pathfinding
                    .path_between(farm_door, warp_tile)
                    .expect("Guarded by check on reachable tiles")
            })
            .collect();

        Ok(())
    }
}

impl BotGoal for ClearFarmGoal {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Clear farm".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        self.fill_priority_tiles(game_state)?;

        let goal = MaintainStaminaGoal::new();
        if !goal.is_completed(game_state) {
            return Ok(goal.into());
        }

        if self.is_completed(game_state)? {
            return Ok(BotGoalResult::Completed);
        }

        let farm = game_state.get_room("Farm")?;
        let farm_door = game_state.get_farm_door()?;
        let player = &game_state.player;

        // First, prioritize clearing out clutter that is along the
        // fastest route from the FarmHouse to any of the warps
        // present on the farm.  Clearing these tiles will allow
        // faster movement around the map in the future.
        let has_priority_clutter = farm
            .objects
            .iter()
            .filter(|obj| obj.kind.get_tool().is_some())
            .any(|obj| self.priority_tiles.contains(&obj.tile));

        let clearable_tile: HashMap<Vector<isize>, &Object> = farm
            .objects
            .iter()
            .filter(|obj| {
                if has_priority_clutter {
                    self.priority_tiles.contains(&obj.tile)
                } else {
                    match &obj.kind {
                        ObjectKind::Tree(tree) => {
                            tree.is_stump
                                || (self.clear_trees && tree.growth_stage > 0)
                        }
                        ObjectKind::Stone => self.clear_stone,
                        _ => true,
                    }
                }
            })
            .filter(|obj| obj.kind.get_tool().is_some())
            .map(|obj| (obj.tile, obj))
            .filter(|(_, obj)| {
                game_state.player.current_stamina > 2.0
                    || obj.kind.get_tool().unwrap().is_same_item(&Item::SCYTHE)
            })
            .collect();

        if clearable_tile.is_empty() {
            return Ok(BotGoalResult::Completed);
        }

        let is_grown_tree = |obj: &Object| -> bool {
            match &obj.kind {
                ObjectKind::Tree(tree) => {
                    tree.growth_stage >= 5 && !tree.is_stump
                }
                _ => false,
            }
        };

        let pathfinding_without_clearing = farm
            .pathfinding()
            .allow_diagonal(false)
            .include_border(true);

        let reachable_without_clearing =
            pathfinding_without_clearing.reachable(farm_door);

        let clearable_tile = if has_priority_clutter {
            clearable_tile
        } else if game_state.player.skills.foraging_xp < 100
            && clearable_tile.iter().any(|(tile, obj)| {
                reachable_without_clearing[*tile] && is_grown_tree(obj)
            })
        {
            // Second, if foraging level 1 has not yet been reached,
            // prioritize cutting down trees.

            clearable_tile
                .into_iter()
                .filter(|(tile, obj)| {
                    reachable_without_clearing[*tile] && is_grown_tree(obj)
                })
                .collect()
        } else {
            // Third, if all prioritized clutter has been cleared out,
            // preferentially clear out clutter that is close to the farm
            // house.

            let Some(min_dist) = pathfinding_without_clearing
                .iter_dijkstra(farm_door)
                .filter(|(tile, _)| clearable_tile.contains_key(tile))
                .map(|(_, dist)| dist)
                .next()
            else {
                // There is still clutter remaining, but it is not
                // accessible.
                return Ok(BotGoalResult::Completed);
            };

            pathfinding_without_clearing
                .iter_dijkstra(farm_door)
                .take_while(|(_, dist)| *dist < min_dist + 5)
                .filter_map(|(tile, _)| {
                    clearable_tile.get(&tile).map(|obj| (tile, *obj))
                })
                .collect()
        };

        // Pick up the items that we'll need for the rest of the goal.
        //
        // TODO: Let InventoryGoal accept a list of items.  The
        // current implementation opens/closes a chest once for each
        // tool, even if they are all stored in the same chest.
        let items = [Item::PICKAXE, Item::AXE, Item::SCYTHE];
        for item in items {
            let goal = super::InventoryGoal::new(item);
            if !goal.is_completed(game_state)? {
                return Ok(goal.into());
            }
        }

        // Go to the Farm
        //
        // TODO: Expose the room-to-room movement without requiring
        // local movement afterwards.  In this case, I want to go to
        // the farm, but don't care where I start out within the farm.
        if player.room_name != "Farm" {
            let target_pos = game_state
                .locations
                .iter()
                .flat_map(|loc| loc.warps.iter())
                .find(|warp| warp.target_room == "Farm")
                .map(|warp| warp.target.map(|x| x as f32))
                .ok_or_else(|| BotError::UnknownRoom("Farm".into()))?;
            return Ok(MovementGoal::new("Farm", target_pos)
                .with_tolerance(100.0)
                .into());
        }

        let player_tile = player.tile();

        let opt_goal = Self::pathfinding(farm)
            .include_border(true)
            .iter_dijkstra(player_tile)
            .map(|(tile, _)| tile)
            .find_map(|tile| {
                clearable_tile.get(&tile).map(|obj| {
                    let tool = obj.kind.get_tool().expect(
                        "Lookup should only contain tiles \
                         that can be cleared by using a tool.",
                    );
                    UseItemOnTile::new(tool, "Farm", tile)
                })
            });

        if let Some(goal) = opt_goal {
            Ok(goal.into())
        } else {
            // There's still clutter on the farm, but we can't reach it.
            Ok(BotGoalResult::Completed)
        }
    }
}

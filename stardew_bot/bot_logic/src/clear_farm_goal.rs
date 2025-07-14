use std::collections::{HashMap, HashSet};

use geometry::Vector;
use itertools::Itertools as _;

use crate::{bot_logic::LogicStack, Error, GameAction, MovementGoal};
use game_state::{
    GameState, Inventory, Item, ItemId, Key, Location, Object, ObjectKind,
    Quality,
};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    GameStateExt as _, InventoryGoal, LocationExt as _, MaintainStaminaGoal,
    ObjectKindExt as _, Pathfinding, UseItemOnTile,
};

pub struct ClearFarmGoal {
    stop_time: i32,
    clear_trees: bool,
    clear_stone: bool,
    use_stamina: bool,
    use_priority_tiles: bool,
    priority_tiles: HashSet<Vector<isize>>,

    /// When deciding what to clear next, the relative weight between
    /// the distance from the player (first element) and the distance
    /// from the target tile (second element).
    relative_weights: (u64, u64),

    /// The location around which to clear.  Defaults to the location
    /// of the farm door.
    target_tile: Option<Vector<isize>>,
}

impl ClearFarmGoal {
    pub fn new() -> Self {
        Self {
            stop_time: 2600,
            priority_tiles: HashSet::new(),
            clear_trees: false,
            clear_stone: true,
            use_stamina: true,
            use_priority_tiles: true,
            relative_weights: (1, 1),
            target_tile: None,
        }
    }

    pub fn stop_time(self, stop_time: i32) -> Self {
        Self { stop_time, ..self }
    }

    pub fn use_priority_tiles(self, use_priority_tiles: bool) -> Self {
        Self {
            use_priority_tiles,
            ..self
        }
    }

    pub fn relative_weights(self, relative_weights: (u64, u64)) -> Self {
        Self {
            relative_weights,
            ..self
        }
    }

    pub fn target_tile(self, target_tile: Vector<isize>) -> Self {
        Self {
            target_tile: Some(target_tile),
            ..self
        }
    }

    pub fn use_stamina(self, use_stamina: bool) -> Self {
        Self {
            use_stamina,
            ..self
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
        if game_state.globals.in_game_time >= self.stop_time {
            return Ok(true);
        }

        let farm = game_state.get_room("Farm")?;
        let target_tile = self
            .target_tile
            .map_or_else(|| game_state.get_farm_door(), Ok)?;

        let pathfinding = farm
            .pathfinding()
            .allow_diagonal(false)
            .include_border(true)
            .breakable_clearing_cost(1);

        let pathfinding =
            if self.use_stamina && game_state.player.current_stamina > 2.0 {
                pathfinding
                    .stone_clearing_cost(10)
                    .wood_clearing_cost(10)
                    .tree_clearing_cost(50)
            } else {
                pathfinding
            };

        let reachable = pathfinding.reachable(target_tile);

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
            .breakable_clearing_cost(1)
            .grass_movement_cost(1)
            .tree_clearing_cost(50)
    }

    fn fill_priority_tiles(
        &mut self,
        game_state: &GameState,
    ) -> Result<(), Error> {
        if !self.priority_tiles.is_empty() {
            // Priority tiles have already been populated.
            return Ok(());
        }
        if !self.use_priority_tiles {
            // Use of priority tiles has been disabled.
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
        let target_tile = self
            .target_tile
            .map_or_else(|| game_state.get_farm_door(), Ok)?;
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

        let clearable_tiles: HashMap<Vector<isize>, &Object> = farm
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
                        ObjectKind::Stone(_) => self.clear_stone,
                        _ => true,
                    }
                }
            })
            .filter(|obj| obj.kind.get_tool().is_some())
            .map(|obj| (obj.tile, obj))
            .filter(|(_, obj)| {
                (self.use_stamina && game_state.player.current_stamina > 2.0)
                    || obj.kind.get_tool().unwrap() == ItemId::SCYTHE
            })
            .collect();

        if clearable_tiles.is_empty() {
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

        let steps_from_target_tile =
            pathfinding_without_clearing.distances(target_tile);

        let clearable_tiles = if has_priority_clutter {
            clearable_tiles
        } else if game_state.player.skills.foraging_xp < 100
            && clearable_tiles.iter().any(|(tile, obj)| {
                steps_from_target_tile.is_some(*tile) && is_grown_tree(obj)
            })
        {
            // Second, if foraging level 1 has not yet been reached,
            // prioritize cutting down trees.

            clearable_tiles
                .into_iter()
                .filter(|(tile, obj)| {
                    steps_from_target_tile.is_some(*tile) && is_grown_tree(obj)
                })
                .collect()
        } else {
            clearable_tiles
        };

        // Pick up the items that we'll need for the rest of the goal.
        // Anything else gets stashed away.
        let goal = InventoryGoal::current()
            .with(ItemId::PICKAXE)
            .with(ItemId::AXE)
            .with(ItemId::SCYTHE)
            .with(ItemId::HOE)
            .stamina_recovery_slots(1);
        if !goal.is_completed(game_state)? {
            let goal = goal.otherwise_empty().stamina_recovery_slots(4);
            return Ok(goal.into());
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
                .ok_or_else(|| Error::UnknownRoom("Farm".into()))?;
            return Ok(MovementGoal::new("Farm", target_pos)
                .with_tolerance(100.0)
                .into());
        }

        let player_tile = player.tile();

        let steps_from_player =
            pathfinding_without_clearing.distances(player_tile);

        let next_clear_heuristic = |tile: Vector<isize>| {
            let (weight_player, weight_target) = self.relative_weights;
            let from_target = steps_from_target_tile
                .get(tile)
                .map(|opt| opt.as_ref())
                .flatten()
                .cloned()
                .unwrap_or(10000);
            let from_player = steps_from_player
                .get(tile)
                .map(|opt| opt.as_ref())
                .flatten()
                .cloned()
                .unwrap_or(10000);
            from_target * weight_target + from_player * weight_player
        };
        let min_total_steps = clearable_tiles
            .iter()
            .map(|(tile, _)| next_clear_heuristic(*tile))
            .min()
            .expect("Guarded by earlier clearable_tiles.is_empty() check");

        let opt_goal = Self::pathfinding(farm)
            .include_border(true)
            .iter_dijkstra(player_tile)
            .map(|(tile, _)| tile)
            .filter(|tile| next_clear_heuristic(*tile) <= min_total_steps)
            .find_map(|tile| {
                clearable_tiles.get(&tile).map(|obj| {
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

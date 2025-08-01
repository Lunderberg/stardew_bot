use std::collections::{HashMap, HashSet};

use geometry::Vector;
use itertools::Itertools as _;

use crate::{ClearTreeGoal, Error, FarmPlan, MovementGoal};
use game_state::{
    GameState, ItemId, ObjectKind, ResourceClumpKind, Tree, TreeKind,
};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    GameStateExt as _, InventoryGoal, LocationExt as _, MaintainStaminaGoal,
    ObjectKindExt as _, Pathfinding, UseItemOnTile,
};

pub struct ClearFarmGoal {
    stop_time: i32,
    clear_trees: bool,

    /// Clear trees types that are added by Stardew Valley Expanded.
    clear_expanded_trees: bool,

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

    plan: Option<FarmPlan>,
}

impl Default for ClearFarmGoal {
    fn default() -> Self {
        Self::new()
    }
}

impl ClearFarmGoal {
    pub fn new() -> Self {
        Self {
            stop_time: 2600,
            priority_tiles: HashSet::new(),
            clear_trees: false,
            clear_expanded_trees: false,
            clear_stone: true,
            use_stamina: true,
            use_priority_tiles: true,
            relative_weights: (1, 1),
            target_tile: None,
            plan: None,
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
    pub fn clear_expanded_trees(self, clear_expanded_trees: bool) -> Self {
        Self {
            clear_expanded_trees,
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
            .pathfinding(&game_state.statics)
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

    fn pathfinding(game_state: &GameState) -> Result<Pathfinding, Error> {
        let farm = game_state.get_room("Farm")?;

        let pathfinding = farm
            .pathfinding(&game_state.statics)
            .allow_diagonal(false)
            .stone_clearing_cost(10)
            .wood_clearing_cost(10)
            .breakable_clearing_cost(1)
            .grass_movement_cost(1)
            .tree_clearing_cost(50);

        Ok(pathfinding)
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

        let pathfinding = Self::pathfinding(game_state)?.do_not_clear_trees();
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

    fn fill_farm_plan(&mut self, game_state: &GameState) -> Result<(), Error> {
        if self.plan.is_none() {
            self.plan = Some(FarmPlan::plan(game_state)?);
        }
        Ok(())
    }

    fn choose_target_tile(
        &mut self,
        game_state: &GameState,
        clearable_tiles: &HashMap<Vector<isize>, ItemId>,
    ) -> Result<Vector<isize>, Error> {
        if let Some(tile) = self.target_tile {
            return Ok(tile);
        }

        self.fill_farm_plan(game_state)?;
        let plan = self.plan.as_ref().unwrap();

        let opt_region_center =
            plan.arable_regions.iter().take(3).find_map(|region| {
                let mut num_tiles = 0isize;
                let mut tile_sum = Vector::<isize>::zero();
                region
                    .map
                    .iter_true()
                    .filter(|tile| clearable_tiles.contains_key(tile))
                    .for_each(|tile| {
                        num_tiles += 1;
                        tile_sum = tile_sum + tile;
                    });
                (num_tiles > 0).then(|| tile_sum / num_tiles)
            });
        if let Some(tile) = opt_region_center {
            return Ok(tile);
        }

        let farm = game_state.get_room("Farm")?;
        let farm_door = game_state.get_farm_door()?;

        let currently_reachable =
            farm.pathfinding(&game_state.statics).reachable(farm_door);
        let potentially_reachable = farm
            .pathfinding(&game_state.statics)
            .ignoring_small_obstacles()
            .reachable(farm_door);

        let opt_warp = farm
            .warps
            .iter()
            .sorted_by_key(|warp| {
                (
                    warp.target_room == "Greenhouse",
                    &warp.target_room,
                    warp.location.right,
                    warp.location.down,
                )
            })
            .flat_map(|warp| {
                // Technically, warps for screen transitions are just off
                // the edge of the map, and aren't actually reachable.
                // Instead, make sure that the path can reach any tiles
                // that is adjacent to a warp.
                warp.location.iter_cardinal()
            })
            .find(|&tile| {
                potentially_reachable.is_set(tile)
                    && currently_reachable.is_unset(tile)
            });
        if let Some(tile) = opt_warp {
            return Ok(tile);
        }

        Ok(farm_door)
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
        if !goal.is_completed(game_state)? {
            return Ok(goal.into());
        }

        if self.is_completed(game_state)? {
            return Ok(BotGoalResult::Completed);
        }

        let farm = game_state.get_room("Farm")?;
        let player = &game_state.player;
        let opt_axe = game_state.current_axe()?;

        // First, prioritize clearing out clutter that is along the
        // fastest route from the FarmHouse to any of the warps
        // present on the farm.  Clearing these tiles will allow
        // faster movement around the map in the future.
        let has_priority_clutter = false
            && farm
                .objects
                .iter()
                .filter(|obj| obj.kind.get_tool().is_some())
                .any(|obj| self.priority_tiles.contains(&obj.tile));

        self.fill_farm_plan(game_state)?;
        let plan = self.plan.as_ref().unwrap();

        let iter_objects = farm
            .objects
            .iter()
            .filter(|obj| {
                if has_priority_clutter {
                    self.priority_tiles.contains(&obj.tile)
                } else {
                    match &obj.kind {
                        ObjectKind::Tree(Tree { is_stump: true, .. }) => true,
                        ObjectKind::Tree(tree) => {
                            (self.clear_trees
                                && tree.growth_stage > 0
                                && !plan.is_planned_tree(obj.tile))
                                || (self.clear_expanded_trees
                                    && matches!(
                                        tree.kind,
                                        TreeKind::Fir | TreeKind::Birch
                                    ))
                        }
                        ObjectKind::Stone(_) => self.clear_stone,

                        ObjectKind::Chest(_)
                        | ObjectKind::CraftingMachine(_)
                        | ObjectKind::Scarecrow
                        | ObjectKind::Sprinkler(_) => false,
                        _ => true,
                    }
                }
            })
            .filter_map(|obj| {
                let tool = obj.kind.get_tool()?;
                Some((obj.tile, tool))
            })
            .filter_map(|(tile, tool)| {
                let tool = if tool == ItemId::AXE {
                    opt_axe?.clone()
                } else {
                    tool
                };
                Some((tile, tool))
            });

        let iter_clumps = farm
            .resource_clumps
            .iter()
            .filter_map(|clump| {
                let tool = match &clump.kind {
                    ResourceClumpKind::Stump
                        if opt_axe == Some(&ItemId::COPPER_AXE) =>
                    {
                        opt_axe
                    }
                    _ => None,
                }?;
                let iter = clump
                    .shape
                    .iter_points()
                    .map(move |tile| (tile, tool.clone()));
                Some(iter)
            })
            .flatten();

        let clearable_tiles: HashMap<Vector<isize>, ItemId> = iter_objects
            .chain(iter_clumps)
            .filter(|(_, tool)| {
                (self.use_stamina && game_state.player.current_stamina > 2.0)
                    || tool == &ItemId::SCYTHE
            })
            .collect();

        if clearable_tiles.is_empty() {
            return Ok(BotGoalResult::Completed);
        }

        // Pick up the items that we'll need for the rest of the goal.
        // Anything else gets stashed away.
        let goal = InventoryGoal::current()
            .with(ItemId::PICKAXE)
            .with(opt_axe.into_iter().cloned())
            .with(ItemId::SCYTHE)
            .with(ItemId::HOE)
            .stamina_recovery_slots(2);
        if game_state.player.inventory.num_empty_slots() < 2
            || !goal.is_completed(game_state)?
        {
            let goal = goal.otherwise_empty().stamina_recovery_slots(9);
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
                .ok_or_else(|| Error::UnknownRoom("Farm".into()))?;
            return Ok(MovementGoal::new("Farm", target_pos)
                .with_tolerance(100.0)
                .into());
        }

        let player_tile = player.tile();

        let target_tile =
            self.choose_target_tile(game_state, &clearable_tiles)?;

        let steps_from_player = farm
            .pathfinding(&game_state.statics)
            .allow_diagonal(false)
            .include_border(true)
            .distances(player_tile);

        let steps_from_target_tile = farm
            .pathfinding(&game_state.statics)
            .ignoring_small_obstacles()
            .distances(target_tile);

        let next_clear_heuristic = |tile: Vector<isize>| {
            let (weight_player, weight_target) = self.relative_weights;
            let from_target = steps_from_target_tile
                .get_opt(tile)
                .cloned()
                .unwrap_or(10000);
            let from_player =
                steps_from_player.get_opt(tile).cloned().unwrap_or(10000);
            from_target * weight_target + from_player * weight_player
        };
        let min_total_steps = clearable_tiles
            .iter()
            .filter(|(tile, _)| steps_from_player.is_some(**tile))
            .map(|(tile, _)| next_clear_heuristic(*tile))
            .min()
            .expect("Guarded by earlier clearable_tiles.is_empty() check");

        let opt_goal_tile = Self::pathfinding(game_state)?
            .include_border(true)
            .iter_dijkstra(player_tile)
            .map(|(tile, _)| tile)
            .filter(|tile| next_clear_heuristic(*tile) <= min_total_steps)
            .find(|tile| clearable_tiles.contains_key(tile));
        let Some(goal_tile) = opt_goal_tile else {
            // There's still clutter on the farm, but we can't reach it.
            return Ok(BotGoalResult::Completed);
        };

        let tool = clearable_tiles
            .get(&goal_tile)
            .expect("Protected by clearable_tiles.contains_key");

        let is_grown_tree = farm.objects.iter().any(|obj| {
            obj.tile == goal_tile
                && match &obj.kind {
                    ObjectKind::Tree(tree) => {
                        tree.growth_stage >= 5 && !tree.is_stump
                    }
                    _ => false,
                }
        });

        if is_grown_tree {
            Ok(ClearTreeGoal::new(goal_tile).into())
        } else {
            Ok(UseItemOnTile::new(tool.clone(), "Farm", goal_tile).into())
        }
    }
}

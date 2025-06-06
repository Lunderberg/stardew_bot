use std::collections::{HashMap, HashSet};

use itertools::Itertools as _;

use crate::{
    bot_logic::{
        bot_logic::SubGoals, graph_search::GraphSearch as _, BotError,
        MovementGoal, SelectItemGoal,
    },
    game_state::{Inventory, Item, Key, Location, ObjectKind, Quality, Vector},
    Direction, Error, GameAction, GameState,
};

use super::{
    bot_logic::{BotGoal, BotGoalResult},
    GameStateExt as _, MaintainStaminaGoal, ObjectKindExt as _, Pathfinding,
    UseItemOnTile,
};

pub struct ClearFarmGoal {
    priority_tiles: HashSet<Vector<isize>>,
}

impl ClearFarmGoal {
    pub fn new() -> Self {
        Self {
            priority_tiles: HashSet::new(),
        }
    }

    pub fn is_completed(&self, game_state: &GameState) -> Result<bool, Error> {
        let farm = game_state.get_room("Farm")?;
        let farm_door = game_state.get_farm_door()?;

        let reachable = Self::pathfinding(farm).reachable(farm_door);

        let reachable_clutter = farm
            .objects
            .iter()
            .any(|obj| reachable[obj.tile] && obj.kind.get_tool().is_some());

        let has_stamina = game_state.player.current_stamina > 2.0;

        let is_ongoing = reachable_clutter && has_stamina;

        Ok(!is_ongoing)
    }

    fn pathfinding(farm: &Location) -> Pathfinding {
        farm.pathfinding()
            .allow_diagonal(false)
            .stone_clearing_cost(10)
            .wood_clearing_cost(10)
            .fiber_clearing_cost(1)
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

        let pathfinding = Self::pathfinding(farm);
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
            .filter(|warp_tile| {
                reachable.get(*warp_tile).cloned().unwrap_or(false)
            })
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
    fn description(&self) -> std::borrow::Cow<str> {
        "Clear farm".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _do_action: &mut dyn FnMut(GameAction),
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
        let player = &game_state.player;
        let pathfinding = Self::pathfinding(farm);

        // First, prioritize clearing out clutter that is along the
        // fastest route from the FarmHouse to any of the warps
        // present on the farm.  Clearing these tiles will allow
        // faster movement around the map in the future.
        let has_priority_clutter = farm
            .objects
            .iter()
            .filter(|obj| obj.kind.get_tool().is_some())
            .any(|obj| self.priority_tiles.contains(&obj.tile));

        let tool_to_use: HashMap<Vector<isize>, Item> = farm
            .objects
            .iter()
            .filter(|obj| {
                !has_priority_clutter || self.priority_tiles.contains(&obj.tile)
            })
            .filter_map(|obj| obj.kind.get_tool().map(|tool| (obj.tile, tool)))
            .collect();

        if tool_to_use.is_empty() {
            return Ok(BotGoalResult::Completed);
        }

        // Pick up the items that we'll need for the rest of the goal.
        //
        // TODO: Let InventoryGoal accept a list of items.  The
        // current implementation opens/closes a chest once for each
        // tool, even if they are all stored in the same chest.
        let items = [Item::PICKAXE, Item::AXE, Item::SCYTHE];
        for item in items {
            let goal = super::InventoryGoal::new(item);
            if !goal.contains_target_item(&player.inventory) {
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

        let opt_goal = pathfinding
            .iter_dijkstra(player_tile)
            .map(|(tile, _)| tile)
            .find_map(|tile| {
                tool_to_use
                    .get(&tile)
                    .map(|tool| UseItemOnTile::new(tool.clone(), "Farm", tile))
            });

        if let Some(goal) = opt_goal {
            Ok(goal.into())
        } else {
            // There's still clutter on the farm, but we can't reach it.
            Ok(BotGoalResult::Completed)
        }
    }
}

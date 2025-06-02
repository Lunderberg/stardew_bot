use std::collections::HashMap;

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
    GameStateExt as _, ObjectKindExt as _, Pathfinding, UseItemOnTile,
};

pub struct ClearFarmGoal;

impl ClearFarmGoal {
    pub fn is_completed(&self, game_state: &GameState) -> Result<bool, Error> {
        let farm = game_state.get_room("Farm")?;
        let farm_door = game_state.get_farm_door()?;

        let reachable = Self::pathfinding(farm).reachable(farm_door);

        let reachable_clutter = farm
            .objects
            .iter()
            .any(|obj| reachable[obj.tile] && obj.kind.get_tool().is_some());

        Ok(!reachable_clutter)
    }

    fn pathfinding(farm: &Location) -> Pathfinding {
        farm.pathfinding()
            .stone_clearing_cost(10)
            .wood_clearing_cost(10)
            .fiber_clearing_cost(10)
            .tree_clearing_cost(10)
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
        let farm = game_state.get_room("Farm")?;
        let player = &game_state.player;

        let tool_to_use: HashMap<Vector<isize>, Item> = farm
            .objects
            .iter()
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

        let opt_goal = farm
            .pathfinding()
            .stone_clearing_cost(10)
            .wood_clearing_cost(10)
            .fiber_clearing_cost(10)
            .tree_clearing_cost(10)
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

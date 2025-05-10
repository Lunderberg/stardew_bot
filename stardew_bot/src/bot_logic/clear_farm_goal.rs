use std::collections::HashMap;

use crate::{
    bot_logic::{
        bot_logic::SubGoals, graph_search::GraphSearch as _, BotError,
        MovementGoal, SelectItemGoal,
    },
    game_state::{Inventory, Item, Key, ObjectKind, Quality, Vector},
    Direction, Error, GameAction, GameState,
};

use super::bot_logic::{BotGoal, BotGoalResult};

pub struct ClearFarmGoal;

impl BotGoal for ClearFarmGoal {
    fn description(&self) -> std::borrow::Cow<str> {
        "Clear farm".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
    ) -> Result<BotGoalResult, Error> {
        if game_state.inputs.left_mouse_down() {
            return Ok(GameAction::ReleaseLeftClick.into());
        }

        let farm = game_state.get_room("Farm")?;
        let player = &game_state.player;

        let clutter: HashMap<Vector<isize>, &'static str> = farm
            .objects
            .iter()
            .filter_map(|obj| {
                let opt_tool = match &obj.kind {
                    ObjectKind::Stone => Some("(T)Pickaxe"),
                    ObjectKind::Wood => Some("(T)Axe"),
                    ObjectKind::Fiber => Some("(W)47"),
                    _ => None,
                };
                opt_tool.map(|tool| (obj.tile, tool))
            })
            .collect();

        if clutter.is_empty() {
            return Ok(BotGoalResult::Completed);
        }

        // Pick up the items that we'll need for the rest of the goal.
        //
        // TODO: Let InventoryGoal accept a list of items.  The
        // current implementation opens/closes a chest once for each
        // tool, even if they are all stored in the same chest.
        let items = [
            Item::new("(T)Pickaxe"),
            Item::new("(T)Axe"),
            Item::new("(W)47"),
        ];
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
            return Ok(MovementGoal::new("Farm".into(), target_pos)
                .with_tolerance(100.0)
                .into());
        }

        let player_tile = player.tile();

        let opt_adjacent_clutter = Direction::iter()
            .map(|dir| player_tile + dir.offset())
            .find_map(|tile| clutter.get(&tile).map(|tool| (tile, *tool)));
        if let Some((tile, tool)) = opt_adjacent_clutter {
            let goal = SelectItemGoal::new(Item::new(tool));
            if !goal.is_completed(game_state) {
                return Ok(goal.into());
            }

            let action = if player.using_tool {
                GameAction::Wait
            } else {
                GameAction::LeftClickTile(tile).into()
            };
            return Ok(action.into());
        }

        let clear_tiles = {
            let mut map = farm.collect_clear_tiles();
            for (&tile, _) in &clutter {
                map[tile] = true;
            }
            map
        };

        let opt_closest_clutter = clear_tiles
            .dijkstra_search(player_tile)
            .map(|(tile, _)| tile)
            .find(|tile| clutter.contains_key(&tile));

        if let Some(closest_clutter) = opt_closest_clutter {
            let goal = MovementGoal::new("Farm".into(), closest_clutter.into())
                .with_tolerance(1.1);
            Ok(goal.into())
        } else {
            // There's still clutter on the farm, but we can't reach it.
            Ok(BotGoalResult::Completed)
        }
    }
}

use std::collections::HashSet;

use crate::{
    bot_logic::GameStateExt as _, game_state::Vector, Error, GameState,
};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    ActivateTile, InventoryGoal, MovementGoal,
};

pub struct HarvestCropsGoal {}

impl HarvestCropsGoal {
    pub fn new() -> Self {
        Self {}
    }

    fn iter_harvestable<'a>(
        &'a self,
        game_state: &'a GameState,
    ) -> Result<impl Iterator<Item = Vector<isize>> + 'a, Error> {
        let iter = game_state
            .get_room("Farm")?
            .objects
            .iter()
            .filter(|obj| {
                obj.kind
                    .as_hoe_dirt()
                    .map(|hoe_dirt| hoe_dirt.can_harvest())
                    .unwrap_or(false)
            })
            .map(|obj| obj.tile);
        Ok(iter)
    }

    pub fn is_completed(&self, game_state: &GameState) -> Result<bool, Error> {
        Ok(self.iter_harvestable(game_state)?.next().is_none())
    }
}

impl BotGoal for HarvestCropsGoal {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Harvest crops".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        if self.is_completed(game_state)? {
            return Ok(BotGoalResult::Completed);
        }

        let farm = game_state.get_room("Farm")?;
        let player_tile = if game_state.player.room_name == "Farm" {
            game_state.player.tile()
        } else {
            game_state.get_farm_door()?
        };
        let harvestable: HashSet<Vector<isize>> =
            self.iter_harvestable(game_state)?.collect();

        let opt_next_tile = farm
            .pathfinding()
            .include_border(true)
            .iter_dijkstra(player_tile)
            .map(|(tile, _)| tile)
            .find(|tile| harvestable.contains(tile));
        let Some(next_tile) = opt_next_tile else {
            return Ok(BotGoalResult::Completed);
        };

        let free_slots = game_state.player.inventory.num_empty_slots();
        if free_slots < 3 {
            return Ok(InventoryGoal::empty().into());
        }

        if game_state.player.can_move {
            // While harvesting, the player cannot move, but can still
            // click onto an adjacent tile.  Since animation canceling
            // checks `player.UsingTool`, and the harvesting animation
            // doesn't use a tool, we can't just animation cancel our
            // way out of it.  Instead, first position the player in a
            // location that can reach the target tile, and maximizes
            // the number of crops that can be harvested during the
            // animation.
            //
            // If we are not currently able to move, then this likely
            // has already occurred, and we're frozen in place.  In
            // that case, attempt to click on the tile as it may be in
            // range.
            let distances = farm.pathfinding().distances(player_tile);

            let count_adjacent_crops = |tile: Vector<isize>| -> usize {
                tile.iter_nearby()
                    .filter(|adj| harvestable.contains(adj))
                    .count()
            };

            let (standing_tile, num_harvestable) = next_tile
                .iter_nearby()
                .filter(|adj| distances.is_some(*adj))
                .map(|adj| (adj, count_adjacent_crops(adj)))
                .max_by_key(|&(adj, count)| {
                    (count, std::cmp::Reverse(distances[adj].unwrap()))
                })
                .expect("At minimum, will contain itself");
            if num_harvestable > 1 && player_tile != standing_tile {
                let goal = MovementGoal::new("Farm", standing_tile.into());
                return Ok(goal.into());
            }
        }

        if game_state.player.can_move
            || player_tile.iter_nearby().any(|adj| adj == next_tile)
        {
            // Player cannot move, so try to harvest an adjacent crop
            Ok(ActivateTile::new("Farm", next_tile).into())
        } else {
            // Player cannot move, no adjacent crops available
            Ok(BotGoalResult::InProgress)
        }
    }
}

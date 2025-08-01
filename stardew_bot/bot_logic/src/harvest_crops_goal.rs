use std::collections::HashSet;

use geometry::Vector;
use itertools::Itertools as _;

use crate::{Error, GameStateExt as _};
use game_state::{CropPhase, GameState, ItemId};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    ActivateTile, CropQualityPredictor, InventoryGoal, LocationExt as _,
    MovementGoal, UseItemOnTile,
};

pub struct HarvestCropsGoal {}

impl Default for HarvestCropsGoal {
    fn default() -> Self {
        Self::new()
    }
}

impl HarvestCropsGoal {
    pub fn new() -> Self {
        Self {}
    }

    fn iter_harvestable<'a>(
        &'a self,
        game_state: &'a GameState,
    ) -> Result<impl Iterator<Item = (Vector<isize>, &'a ItemId)> + 'a, Error>
    {
        let iter =
            game_state
                .get_room("Farm")?
                .objects
                .iter()
                .filter_map(|obj| {
                    obj.kind
                        .as_hoe_dirt()
                        .and_then(|hoe_dirt| hoe_dirt.crop.as_ref())
                        .filter(|crop| {
                            matches!(crop.phase, CropPhase::Harvestable)
                        })
                        .map(|crop| (obj.tile, &crop.seed))
                });
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

        let current_predictor = CropQualityPredictor::new(game_state, 0);
        let upcoming_predictor = {
            let upcoming_farming_xp = self
                .iter_harvestable(game_state)?
                .map(|(_, seed)| seed)
                .counts()
                .iter()
                .map(|(seed, count)| {
                    game_state
                        .statics
                        .get_crop(seed)
                        .map(|crop| (crop.xp_per_harvest as usize) * count)
                })
                .sum::<Result<usize, _>>()?;
            let upcoming_farming_level = game_state
                .player
                .skills
                .upcoming_farming_level(upcoming_farming_xp);
            CropQualityPredictor::new(game_state, 0)
                .with_farming_level(upcoming_farming_level)
        };

        let player_tile = if game_state.player.room_name == "Farm" {
            game_state.player.tile()
        } else {
            game_state.get_farm_door()?
        };
        let distances = farm
            .pathfinding(&game_state.statics)
            .include_border(true)
            .distances(player_tile);
        let opt_next_tile = self
            .iter_harvestable(game_state)?
            .filter(|(tile, _)| distances.is_some(*tile))
            .min_by_key(|(tile, _)| {
                let improved_by_level_up = current_predictor.predict(*tile)
                    != upcoming_predictor.predict(*tile);
                let dist = distances[*tile]
                    .expect("Protected by distances.is_some())");
                (improved_by_level_up, dist)
            });
        let Some((next_tile, next_seed)) = opt_next_tile else {
            return Ok(BotGoalResult::Completed);
        };

        let must_handle_inventory = {
            let inventory = &game_state.player.inventory;
            let free_slots = inventory.num_empty_slots();
            let close_to_inventory =
                distances.get_opt(player_tile).cloned().unwrap_or(0)
                    < distances.get_opt(next_tile).cloned().unwrap();
            free_slots < if close_to_inventory { 6 } else { 2 }
        };
        let goal = InventoryGoal::current()
            .with(ItemId::SCYTHE)
            .with(ItemId::WATERING_CAN)
            .with(ItemId::HOE)
            .with(ItemId::PICKAXE)
            .stamina_recovery_slots(1);
        if !goal.is_completed(game_state)? || must_handle_inventory {
            let goal = goal.otherwise_empty().stamina_recovery_slots(2);
            if !goal.is_completed(game_state)? {
                return Ok(goal.into());
            }
        }

        if game_state.statics.get_crop(next_seed)?.harvest_with_scythe {
            // Crop is harvested with the scythe, which can be
            // animation-cancelled.  No extra movement planning is
            // required.
            let goal = UseItemOnTile::new(ItemId::SCYTHE, "Farm", next_tile);
            return Ok(goal.into());
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
            let distances =
                farm.pathfinding(&game_state.statics).distances(player_tile);

            let harvestable: HashSet<Vector<isize>> = self
                .iter_harvestable(game_state)?
                .map(|(tile, _)| tile)
                .collect();

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

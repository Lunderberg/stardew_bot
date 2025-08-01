use std::collections::HashSet;

use geometry::Vector;

use crate::{Error, MaintainStaminaGoal, MovementGoal, UseItemOnTile};
use game_state::{GameState, ItemId};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    FillWateringCan, GameStateExt as _, InventoryGoal, LocationExt as _,
};

pub struct WaterCropsGoal {}

impl Default for WaterCropsGoal {
    fn default() -> Self {
        Self::new()
    }
}

impl WaterCropsGoal {
    pub fn new() -> Self {
        Self {}
    }

    fn iter_needs_watering(
        game_state: &GameState,
    ) -> Result<impl Iterator<Item = Vector<isize>> + '_, Error> {
        let iter = game_state
            .get_room("Farm")?
            .objects
            .iter()
            .filter(|obj| {
                obj.kind
                    .as_hoe_dirt()
                    .map(|hoe_dirt| hoe_dirt.requires_watering())
                    .unwrap_or(false)
            })
            .map(|obj| obj.tile);

        Ok(iter)
    }

    pub fn is_completed(&self, game_state: &GameState) -> Result<bool, Error> {
        let has_unwatered_crop =
            Self::iter_needs_watering(game_state)?.next().is_some();

        Ok(!has_unwatered_crop)
    }
}

impl BotGoal for WaterCropsGoal {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Water crops".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        if self.is_completed(game_state)? {
            return Ok(BotGoalResult::Completed);
        }

        let goal = MaintainStaminaGoal::new();
        if !goal.is_completed(game_state)? {
            return Ok(goal.into());
        }
        if game_state.player.current_stamina < 2.0 {
            return Ok(BotGoalResult::Completed);
        }

        if game_state.player.room_name != "Farm" {
            let goal = MovementGoal::new(
                "Farm",
                game_state.get_farm_door()?.map(|x| x as f32),
            )
            .with_tolerance(1000.0);

            return Ok(goal.into());
        }

        let farm = game_state.get_room("Farm")?;
        let player_tile = game_state.player.tile();

        let needs_watering: HashSet<Vector<isize>> =
            Self::iter_needs_watering(game_state)?.collect();

        let opt_next_tile = farm
            .pathfinding(&game_state.statics)
            .include_border(true)
            .iter_dijkstra(player_tile)
            .map(|(tile, _)| tile)
            .find(|tile| needs_watering.contains(tile));
        let Some(tile) = opt_next_tile else {
            return Ok(BotGoalResult::Completed);
        };

        let goal = InventoryGoal::new(ItemId::WATERING_CAN);
        if !goal.is_completed(game_state)? {
            return Ok(goal.otherwise_empty().stamina_recovery_slots(2).into());
        }

        let goal = FillWateringCan::if_empty();
        if !goal.is_completed(game_state) {
            return Ok(goal.into());
        }

        let action = UseItemOnTile::new(ItemId::WATERING_CAN, "Farm", tile);
        Ok(action.into())
    }
}

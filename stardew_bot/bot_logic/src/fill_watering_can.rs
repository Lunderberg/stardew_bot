use crate::Error;
use game_state::{GameState, ItemId, WateringCan};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    LocationExt as _, UseItemOnTile,
};

pub struct FillWateringCan(WhenToFill);

#[allow(dead_code)]
enum WhenToFill {
    IfEmpty,
    IfNotFull,
}

impl FillWateringCan {
    pub fn if_empty() -> Self {
        Self(WhenToFill::IfEmpty)
    }

    #[allow(dead_code)]
    pub fn if_not_full() -> Self {
        Self(WhenToFill::IfNotFull)
    }

    fn watering_can(game_state: &GameState) -> Option<&WateringCan> {
        game_state
            .player
            .inventory
            .iter_items()
            .find_map(|item| item.as_watering_can())
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        let Some(watering_can) = Self::watering_can(game_state) else {
            return true;
        };

        match self.0 {
            WhenToFill::IfEmpty => watering_can.remaining_water > 0,
            WhenToFill::IfNotFull => {
                watering_can.remaining_water < watering_can.max_water
            }
        }
    }
}

impl BotGoal for FillWateringCan {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Fill watering can".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        if self.is_completed(game_state) {
            return Ok(BotGoalResult::Completed);
        }

        let loc = game_state.current_room()?;
        let player_tile = game_state.player.tile();

        let closest_water = loc
            .pathfinding()
            .include_border(true)
            .iter_dijkstra(player_tile)
            .map(|(tile, _)| tile)
            .find(|tile| loc.is_water(*tile))
            .expect("Handle case where no water is reachable");

        let action =
            UseItemOnTile::new(ItemId::WATERING_CAN, &loc.name, closest_water);

        Ok(action.into())
    }
}

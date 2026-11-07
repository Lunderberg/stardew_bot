use game_state::GameState;
use geometry::Vector;

use crate::{
    Error, GameAction, MovementGoal, SelectItemGoal,
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
};

pub struct PickUpFurnitureGoal {
    room: String,
    tile: Vector<isize>,
}

impl PickUpFurnitureGoal {
    pub fn new(room: impl Into<String>, tile: Vector<isize>) -> Self {
        Self {
            room: room.into(),
            tile,
        }
    }

    pub fn is_completed(&self, game_state: &GameState) -> Result<bool, Error> {
        let should_pick_up = game_state.player.inventory.has_empty_slot()
            && self.furniture_exists(game_state)?;
        Ok(!should_pick_up)
    }

    fn furniture_exists(&self, game_state: &GameState) -> Result<bool, Error> {
        Ok(game_state
            .get_room(&self.room)?
            .furniture
            .iter()
            .any(|furniture| furniture.shape.contains(self.tile)))
    }
}

impl BotGoal for PickUpFurnitureGoal {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        format!("Pick up furniture from {}", self.tile).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        if !self.furniture_exists(game_state)? {
            return Ok(BotGoalResult::Completed);
        }

        let select_empty = SelectItemGoal::empty_slot();
        if !select_empty.is_completed(game_state) {
            return Ok(select_empty.into());
        }

        let room = self.room.clone();
        let tile = self.tile;
        let is_in_range = move |game_state: &GameState| -> bool {
            // TODO: Use actual range whenever not in the FarmHouse.
            // (Within the FarmHouse, the furniture just needs to be on
            // screen.)
            game_state.player.room_name == room
                && game_state.display.tile_is_in_view(tile)
        };

        if !is_in_range(game_state) {
            return Ok(MovementGoal::new(
                self.room.clone(),
                self.tile.map(|x| x as f32),
            )
            .cancel_if(is_in_range)
            .into());
        }

        actions.do_action(GameAction::MouseOverTile(self.tile));
        if self.tile == game_state.inputs.mouse_tile_location {
            actions.do_action(GameAction::LeftClick);
        }
        Ok(BotGoalResult::InProgress)
    }
}

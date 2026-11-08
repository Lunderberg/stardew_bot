use game_state::GameState;
use geometry::Vector;

use crate::{
    Error, GameAction, MovementGoal, SelectItemGoal,
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
};

#[derive(Clone)]
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

    pub fn is_in_range(&self, game_state: &GameState) -> bool {
        let player = &game_state.player;

        let correct_room = player.room_name == self.room;
        let near_furniture = if self.room == "FarmHouse" {
            game_state.display.tile_is_in_view(self.tile)
        } else {
            player.tile().chessboard_dist(self.tile) <= 1
        };

        correct_room && near_furniture
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

        if !self.is_in_range(game_state) {
            let cloned = self.clone();
            return Ok(MovementGoal::new(
                self.room.clone(),
                self.tile.map(|x| x as f32),
            )
            .cancel_if(move |game_state| cloned.is_in_range(game_state))
            .into());
        }

        actions.do_action(GameAction::MouseOverTile(self.tile));
        if self.tile == game_state.inputs.mouse_tile_location {
            actions.do_action(GameAction::LeftClick);
        }
        Ok(BotGoalResult::InProgress)
    }
}

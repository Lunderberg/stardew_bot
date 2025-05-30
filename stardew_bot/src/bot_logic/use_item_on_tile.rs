use crate::{
    game_state::{Item, Vector},
    Error, GameAction, GameState,
};

use super::{
    bot_logic::{BotGoal, BotGoalResult},
    MovementGoal, SelectItemGoal,
};

pub struct UseItemOnTile {
    item: Item,
    room: String,
    tile: Vector<isize>,
    item_has_been_used: bool,
}

impl UseItemOnTile {
    pub fn new(
        item: Item,
        room: impl Into<String>,
        tile: Vector<isize>,
    ) -> Self {
        let room = room.into();
        Self {
            item,
            room,
            tile,
            item_has_been_used: false,
        }
    }
}

impl BotGoal for UseItemOnTile {
    fn description(&self) -> std::borrow::Cow<str> {
        format!("Use {} on {} in {}", self.item, self.tile, self.room).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        do_action: &mut dyn FnMut(GameAction),
    ) -> Result<BotGoalResult, Error> {
        let player = &game_state.player;

        if player.using_tool
            && player
                .selected_item()
                .map(|item| item.item_id.starts_with("(T)"))
                .unwrap_or(false)
        {
            if player.last_click == Vector::zero() {
                do_action(GameAction::AnimationCancel);
            }
            return Ok(BotGoalResult::InProgress);
        }

        if self.item_has_been_used {
            return Ok(BotGoalResult::Completed);
        }

        let movement =
            MovementGoal::new(self.room.clone(), self.tile.map(|x| x as f32))
                .with_tolerance(1.4);
        if !movement.is_completed(game_state) {
            return Ok(movement.into());
        }

        let select_item = SelectItemGoal::new(self.item.clone());
        if !select_item.is_completed(game_state) {
            return Ok(select_item.into());
        }

        // let requires_adjacent_tile = match self.item.item_id {

        // };
        // let player_tile = game_state.player.tile();
        // if requires_adjacent_tile && player_tile==self.tile {
        //     // TODO: Move to an adjacent tile instead of standing on it.
        // }

        do_action(GameAction::MouseOverTile(self.tile));
        if game_state.inputs.mouse_tile_location == self.tile
            && !game_state.inputs.left_mouse_down()
        {
            do_action(GameAction::LeftClick);
            self.item_has_been_used = true;
        }
        Ok(BotGoalResult::InProgress)
    }
}

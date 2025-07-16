use crate::{Error, GameAction};
use game_state::{GameState, ItemId};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    SelectItemGoal,
};

pub struct UseItem {
    item: ItemId,
    item_used: bool,
}

impl UseItem {
    pub fn new(item: ItemId) -> Self {
        Self {
            item,
            item_used: false,
        }
    }
}

impl BotGoal for UseItem {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        format!("Use {}", self.item).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let player = &game_state.player;
        let player_tile = player.tile();

        if self.item_used {
            return Ok(BotGoalResult::Completed);
        }

        if !player.inventory.contains(&self.item) {
            return Ok(BotGoalResult::Completed);
        }

        let select_item = SelectItemGoal::new(self.item.clone());
        if !select_item.is_completed(game_state) {
            return Ok(select_item.into());
        }

        actions.do_action(GameAction::MouseOverTile(player_tile));
        if game_state.inputs.mouse_tile_location == player_tile {
            self.item_used = true;
            actions.do_action(GameAction::RightClick);
        }
        Ok(BotGoalResult::InProgress)
    }
}

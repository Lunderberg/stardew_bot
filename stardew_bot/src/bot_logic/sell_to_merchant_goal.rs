use crate::{
    bot_logic::GoToActionTile, game_state::Item, Error, GameAction, GameState,
};

use super::bot_logic::{BotGoal, BotGoalResult};

pub struct SellToMerchantGoal {
    merchant: String,
    item: Item,
    movement_goal: GoToActionTile,
}

impl SellToMerchantGoal {
    pub fn new(merchant: impl Into<String>, item: Item) -> Self {
        let merchant = merchant.into();
        Self {
            item,
            movement_goal: GoToActionTile::new(merchant.clone()),
            merchant,
        }
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        game_state
            .player
            .inventory
            .iter_items()
            .all(|item| !item.is_same_item(&self.item))
    }
}

impl BotGoal for SellToMerchantGoal {
    fn description(&self) -> std::borrow::Cow<str> {
        format!("Sell {} to {}", self.item, self.merchant).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        do_action: &mut dyn FnMut(GameAction),
    ) -> Result<BotGoalResult, Error> {
        // TODO: Handle these are part of some return-to-default
        // logic, rather than needing each goal to release each
        // button.
        {
            let mut cleanup = false;
            if game_state.inputs.left_mouse_down() {
                do_action(GameAction::ReleaseLeftClick);
                cleanup = true;
            }
            if game_state.inputs.right_mouse_down() {
                do_action(GameAction::ReleaseRightClick);
                cleanup = true;
            }
            if cleanup {
                return Ok(BotGoalResult::InProgress);
            }
        }

        if self.is_completed(game_state) {
            if let Some(menu) = &game_state.shop_menu {
                do_action(GameAction::MouseOverPixel(menu.exit_button));
                do_action(GameAction::LeftClick);
                return Ok(BotGoalResult::InProgress);
            } else {
                return Ok(BotGoalResult::Completed);
            }
        }

        if let Some(menu) = &game_state.shop_menu {
            let pixel = menu
                .player_item_locations
                .iter()
                .zip(game_state.player.inventory.iter_slots())
                .find(|(_, opt_item)| {
                    opt_item
                        .map(|item| item.is_same_item(&self.item))
                        .unwrap_or(false)
                })
                .map(|(pixel, _)| *pixel)
                .expect("Already checked that player has the item to sell");
            do_action(GameAction::MouseOverPixel(pixel));
            do_action(GameAction::LeftClick);
            Ok(BotGoalResult::InProgress)
        } else if let Some(menu) = &game_state.dialogue_menu {
            if let Some(pixel) = menu.responses.get(0) {
                do_action(GameAction::MouseOverPixel(*pixel));
            }
            do_action(GameAction::LeftClick);
            Ok(BotGoalResult::InProgress)
        } else {
            Ok(self.movement_goal.clone().into())
        }
    }
}

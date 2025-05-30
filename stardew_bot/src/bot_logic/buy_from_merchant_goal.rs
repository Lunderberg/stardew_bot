use crate::{
    bot_logic::{BotError, GoToActionTile},
    game_state::Item,
    Error, GameAction, GameState,
};

use super::bot_logic::{BotGoal, BotGoalResult};

pub struct BuyFromMerchantGoal {
    merchant: String,
    item: Item,
    movement_goal: GoToActionTile,
}

impl BuyFromMerchantGoal {
    pub fn new(merchant: impl Into<String>, item: Item) -> Self {
        let merchant = merchant.into();
        Self {
            item,
            movement_goal: GoToActionTile::new(merchant.clone()),
            merchant,
        }
    }

    pub fn item_count(&self, game_state: &GameState) -> usize {
        game_state.player.inventory.count_item(&self.item)
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        let num_in_inventory = self.item_count(game_state);
        num_in_inventory >= self.item.count
    }
}

impl BotGoal for BuyFromMerchantGoal {
    fn description(&self) -> std::borrow::Cow<str> {
        format!("Buy {} from {}", self.item, self.merchant).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        do_action: &mut dyn FnMut(GameAction),
    ) -> Result<BotGoalResult, Error> {
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
            if let Some(held_item) = &menu.held_item {
                let is_correct_item = held_item.is_same_item(&self.item);
                let should_add_to_inventory =
                    !is_correct_item || held_item.count >= self.item.count;

                if should_add_to_inventory {
                    let open_inventory_slot = game_state
                        .player
                        .inventory
                        .iter_slots()
                        .zip(menu.player_item_locations.iter())
                        .filter(|(opt_slot, _)| opt_slot.is_none())
                        .map(|(_, pixel)| *pixel)
                        .next()
                        .expect("TODO: Handle full inventory");
                    do_action(GameAction::MouseOverPixel(open_inventory_slot));
                    do_action(GameAction::LeftClick);

                    return Ok(BotGoalResult::InProgress);
                }
            }

            let to_buy_index = menu
                .for_sale
                .iter()
                .enumerate()
                .find(|(_, item)| item.is_same_item(&self.item))
                .map(|(i, _)| i)
                .ok_or_else(|| BotError::ItemNotSold {
                    merchant: self.merchant.clone(),
                    item: self.item.clone(),
                })?;
            if !menu.visible_items().contains(&to_buy_index) {
                todo!("Implement scrolling through shop menu.")
            }

            let button_index = to_buy_index
                .checked_sub(menu.for_sale_scroll_index)
                .expect("Guarded by menu.visible_items().contains");
            let pixel = menu.for_sale_buttons[button_index];

            do_action(GameAction::MouseOverPixel(pixel));
            do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
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

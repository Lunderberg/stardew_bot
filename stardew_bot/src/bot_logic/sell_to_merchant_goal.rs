use crate::{
    bot_logic::GoToActionTile, game_state::Item, Error, GameAction, GameState,
};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    MenuCloser,
};

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

    pub fn item_count(&self, game_state: &GameState) -> usize {
        game_state
            .player
            .inventory
            .iter_items()
            .filter(|item| item.is_same_item(&self.item))
            .map(|item| item.count)
            .sum::<usize>()
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        self.item_count(game_state) == 0
    }
}

impl BotGoal for SellToMerchantGoal {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        format!("Sell {} to {}", self.item, self.merchant).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        if self.is_completed(game_state) {
            let cleanup = MenuCloser::new();
            if cleanup.is_completed(game_state) {
                return Ok(BotGoalResult::Completed);
            } else {
                return Ok(cleanup.into());
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
            actions.do_action(GameAction::MouseOverPixel(pixel));
            actions.do_action(GameAction::LeftClick);
            Ok(BotGoalResult::InProgress)
        } else if let Some(menu) = &game_state.dialogue_menu {
            if let Some(pixel) = menu.responses.get(0) {
                actions.do_action(GameAction::MouseOverPixel(*pixel));
            }
            actions.do_action(GameAction::LeftClick);
            Ok(BotGoalResult::InProgress)
        } else {
            Ok(self.movement_goal.clone().into())
        }
    }
}

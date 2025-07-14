use crate::{Error, GameAction, GoToActionTile};
use game_state::{GameState, Item, ItemSet};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    MenuCloser,
};

pub struct SellToMerchantGoal {
    merchant: String,
    items: Vec<Item>,
    movement_goal: GoToActionTile,
    min_to_sell: usize,
}

impl SellToMerchantGoal {
    pub fn new(merchant: impl Into<String>, item: impl ItemSet) -> Self {
        let merchant = merchant.into();
        Self {
            items: item.iter_item_set().collect(),
            movement_goal: GoToActionTile::new(merchant.clone()),
            merchant,
            min_to_sell: 0,
        }
    }

    pub fn min_to_sell(self, min_to_sell: usize) -> Self {
        Self {
            min_to_sell,
            ..self
        }
    }

    fn next_slot_to_sell(&self, game_state: &GameState) -> Option<usize> {
        // The minimum number to sell is to avoid running across the
        // map to sell one or two items.  If we're already within the
        // shop menu, then the minimum count isn't applied.
        let check_min_count = !self.movement_goal.is_completed(game_state);

        game_state
            .player
            .inventory
            .iter_filled_slots()
            .find(|(_, item)| {
                (item.count >= self.min_to_sell || !check_min_count)
                    && self
                        .items
                        .iter()
                        .any(|to_sell| to_sell.is_same_item(item))
            })
            .map(|(slot, _)| slot)
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        self.next_slot_to_sell(game_state).is_none()
    }
}

impl BotGoal for SellToMerchantGoal {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        if self.items.len() == 1 {
            format!("Sell {} to {}", self.items[0], self.merchant).into()
        } else {
            format!("Sell to {}", self.merchant).into()
        }
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let Some(slot) = self.next_slot_to_sell(game_state) else {
            let cleanup = MenuCloser::new();
            if cleanup.is_completed(game_state) {
                return Ok(BotGoalResult::Completed);
            } else {
                return Ok(cleanup.into());
            }
        };

        if let Some(menu) = &game_state.shop_menu {
            let pixel = menu.player_item_locations[slot];
            actions.do_action(GameAction::MouseOverPixel(pixel));
            actions.do_action(GameAction::LeftClick);
            Ok(BotGoalResult::InProgress)
        } else if let Some(menu) = &game_state.dialogue_menu {
            let Some(pixel) = menu.response_pixel("Shop") else {
                return Ok(MenuCloser::new().into());
            };

            actions.do_action(GameAction::MouseOverPixel(pixel));
            actions.do_action(GameAction::LeftClick);
            Ok(BotGoalResult::InProgress)
        } else {
            Ok(self.movement_goal.clone().into())
        }
    }
}

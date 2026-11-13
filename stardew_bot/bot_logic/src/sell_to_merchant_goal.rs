use crate::{Error, GameAction, GoToActionTile, TransferSize};
use game_state::{GameState, Item, ItemSet};

use super::{
    MenuCloser,
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
};

pub struct SellToMerchantGoal {
    merchant: String,
    items: Vec<Item>,
    movement_goal: GoToActionTile,
    min_to_sell: usize,
}

struct Sale {
    slot: usize,
    size: TransferSize,
}

impl SellToMerchantGoal {
    pub fn new(merchant: impl Into<String>) -> Self {
        let merchant = merchant.into();
        Self {
            items: Vec::new(),
            movement_goal: GoToActionTile::new(merchant.clone()),
            merchant,
            min_to_sell: 0,
        }
    }

    pub fn sell_all(mut self, items: impl ItemSet) -> Self {
        for item in items.iter_item_set() {
            self.items.push(item.with_count(0));
        }
        self
    }

    pub fn sell_down_to(mut self, items: impl ItemSet) -> Self {
        for item in items.iter_item_set() {
            self.items.push(item);
        }
        self
    }

    pub fn min_to_sell(self, min_to_sell: usize) -> Self {
        Self {
            min_to_sell,
            ..self
        }
    }

    fn next_sale(&self, game_state: &GameState) -> Option<Sale> {
        // The minimum number to sell is to avoid running across the
        // map to sell one or two items.  If we're already within the
        // shop menu, then the minimum count isn't applied.
        let check_min_count = !self.movement_goal.is_completed(game_state);

        game_state.player.inventory.iter_filled_slots().find_map(
            |(slot, item)| {
                self.items
                    .iter()
                    .filter(|sell_item| sell_item.is_same_item(item))
                    .filter(|sell_item| item.count > sell_item.count)
                    .find_map(|sell_item| {
                        let num_to_sell = item.count - sell_item.count;
                        if check_min_count && num_to_sell < self.min_to_sell {
                            return None;
                        }

                        let size = if num_to_sell == item.count {
                            TransferSize::All
                        } else if num_to_sell > item.count / 2 {
                            TransferSize::Half
                        } else {
                            TransferSize::One
                        };

                        Some(Sale { slot, size })
                    })
            },
        )
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        self.next_sale(game_state).is_none()
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
        let Some(sale) = self.next_sale(game_state) else {
            let cleanup = MenuCloser::new();
            if cleanup.is_completed(game_state) {
                return Ok(BotGoalResult::Completed);
            } else {
                return Ok(cleanup.into());
            }
        };

        if let Some(menu) = game_state.shop_menu() {
            let pixel = menu.player_item_locations[sale.slot];
            actions.do_action(GameAction::MouseOverPixel(pixel));
            sale.size.send_inputs(game_state, actions);

            Ok(BotGoalResult::InProgress)
        } else if let Some(menu) = game_state.dialogue_menu() {
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

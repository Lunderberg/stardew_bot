use itertools::Either;

use crate::{Error, GameAction, GoToActionTile};
use game_state::{GameState, Item, ItemId};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    LocationExt as _, MenuCloser,
};

pub struct BuyFromMerchantGoal {
    merchant: String,
    item: Item,
    movement_goal: GoToActionTile,
    include_stored_items: Option<String>,
}

impl BuyFromMerchantGoal {
    pub fn new(merchant: impl Into<String>, item: impl Into<Item>) -> Self {
        let merchant = merchant.into();
        let item = item.into();
        Self {
            item,
            movement_goal: GoToActionTile::new(merchant.clone()),
            merchant,
            include_stored_items: None,
        }
    }

    pub fn include_stored_items(
        self,
        storage_location: impl Into<String>,
    ) -> Self {
        Self {
            include_stored_items: Some(storage_location.into()),
            ..self
        }
    }

    pub fn item_count(&self, game_state: &GameState) -> Result<usize, Error> {
        let iter_inventory = game_state.player.inventory.iter_items();

        let iter_stored = self
            .include_stored_items
            .as_ref()
            .map(|storage_location| -> Result<_, Error> {
                let iter_stored =
                    game_state.get_room(storage_location)?.iter_stored_items();
                Ok(iter_stored)
            })
            .transpose()?
            .into_iter()
            .flatten();

        let iter_held_item = game_state
            .shop_menu
            .as_ref()
            .and_then(|menu| menu.held_item.as_ref());

        let iter_items = std::iter::empty()
            .chain(iter_inventory)
            .chain(iter_stored)
            .chain(iter_held_item);

        let item_count = iter_items
            .filter(|item| item.is_same_item(&self.item))
            .map(|item| item.count)
            .sum();

        Ok(item_count)
    }

    fn buy_price(&self, _game_state: &GameState) -> i32 {
        // TODO: Read these from memory, rather than hard-coding them.
        if self.item == ItemId::SALAD {
            220
        } else if self.item == ItemId::PARSNIP_SEEDS {
            20
        } else if self.item == ItemId::KALE_SEEDS {
            70
        } else if self.item == ItemId::COPPER_ORE {
            75
        } else if self.item == ItemId::WOOD {
            10
        } else {
            0
        }
    }

    pub fn is_completed(&self, game_state: &GameState) -> Result<bool, Error> {
        let num_in_inventory = self.item_count(game_state)?;
        let has_desired_amount = num_in_inventory >= self.item.count;
        let can_buy_another =
            game_state.player.current_money >= self.buy_price(game_state);

        Ok(has_desired_amount || !can_buy_another)
    }
}

impl BotGoal for BuyFromMerchantGoal {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        format!("Buy {} from {}", self.item, self.merchant).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        if self.is_completed(game_state)? {
            let cleanup = MenuCloser::new();
            if cleanup.is_completed(game_state) {
                return Ok(BotGoalResult::Completed);
            } else {
                return Ok(cleanup.into());
            }
        }

        if let Some(menu) = &game_state.shop_menu {
            let num_current = self.item_count(game_state)?;
            let num_remaining_to_buy =
                self.item.count.saturating_sub(num_current);

            if let Some(held_item) = &menu.held_item {
                let should_add_to_inventory = {
                    let is_correct_item = held_item.is_same_item(&self.item);

                    let has_desired_amount = num_remaining_to_buy == 0;
                    let can_buy_another = game_state.player.current_money
                        >= self.buy_price(game_state);
                    !is_correct_item || has_desired_amount || !can_buy_another
                };

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
                    actions.do_action(GameAction::MouseOverPixel(
                        open_inventory_slot,
                    ));
                    actions.do_action(GameAction::LeftClick);

                    return Ok(BotGoalResult::InProgress);
                }
            }

            let to_buy_index = menu
                .for_sale
                .iter()
                .enumerate()
                .find(|(_, item)| item.is_same_item(&self.item))
                .map(|(i, _)| i)
                .ok_or_else(|| Error::ItemNotSold {
                    merchant: self.merchant.clone(),
                    item: self.item.clone(),
                })?;
            if !menu.visible_items().contains(&to_buy_index) {
                if to_buy_index > menu.for_sale_scroll_index {
                    actions.do_action(GameAction::ScrollDown);
                } else {
                    actions.do_action(GameAction::ScrollUp);
                }
                return Ok(BotGoalResult::InProgress);
            }

            let button_index = to_buy_index
                .checked_sub(menu.for_sale_scroll_index)
                .expect("Guarded by menu.visible_items().contains");
            let pixel = menu.for_sale_buttons[button_index];

            actions.do_action(GameAction::MouseOverPixel(pixel));
            actions.do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        } else if let Some(menu) = &game_state.dialogue_menu {
            let Some(pixel) = menu.response_pixel("Shop") else {
                let cleanup = MenuCloser::new();
                if cleanup.is_completed(game_state) {
                    return Ok(BotGoalResult::InProgress);
                } else {
                    return Ok(cleanup.into());
                }
            };

            actions.do_action(GameAction::MouseOverPixel(pixel));
            actions.do_action(GameAction::LeftClick);
            Ok(BotGoalResult::InProgress)
        } else {
            Ok(self.movement_goal.clone().into())
        }
    }
}

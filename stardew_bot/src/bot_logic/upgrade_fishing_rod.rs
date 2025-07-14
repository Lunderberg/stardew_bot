use crate::{bot_logic::SellToMerchantGoal, Error, GameAction};
use game_state::{GameState, ItemCategory, ItemId};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult, LogicStackItem},
    BuyFromMerchantGoal, DiscardItemGoal, GameStateExt as _, MenuCloser,
    MovementGoal,
};

pub struct UpgradeFishingRodGoal;

struct UnloadFishingRod {
    rod: ItemId,
}

impl UpgradeFishingRodGoal {
    pub fn new() -> Self {
        Self
    }

    pub fn is_completed(&self, game_state: &GameState) -> Result<bool, Error> {
        Ok(self.next_step(game_state)?.is_none())
    }

    fn next_step(
        &self,
        game_state: &GameState,
    ) -> Result<Option<LogicStackItem>, Error> {
        let inventory = &game_state.player.inventory;

        if inventory.contains(&ItemId::FIBERGLASS_ROD) {
            // Discard the Bamboo Pole as soon as we have the
            // Fiberglass Rod.
            let goal = DiscardItemGoal::new(ItemId::BAMBOO_POLE);
            if !goal.is_completed(game_state) {
                return Ok(Some(goal.into()));
            }
        }
        if inventory.contains(&ItemId::IRIDIUM_ROD) {
            // Unload and discard the Fiberglass Rod asas soon as we
            // have the Iridium Rod.
            let unload = UnloadFishingRod::new(ItemId::FIBERGLASS_ROD);
            if !unload.is_completed(game_state) {
                return Ok(Some(unload.into()));
            }

            let goal = DiscardItemGoal::new(ItemId::FIBERGLASS_ROD);
            if !goal.is_completed(game_state) {
                return Ok(Some(goal.into()));
            }
        }

        let Some(upgraded_rod) = self.next_upgrade(game_state)? else {
            return Ok(None);
        };

        if (upgraded_rod == ItemId::FIBERGLASS_ROD
            || upgraded_rod == ItemId::IRIDIUM_ROD)
            && !self.fish_shop_open(game_state)
        {
            return Ok(None);
        }

        let fishing_level = game_state.player.skills.fishing_level();
        if (upgraded_rod == ItemId::FIBERGLASS_ROD && fishing_level < 2)
            || (upgraded_rod == ItemId::IRIDIUM_ROD && fishing_level < 6)
        {
            return Ok(None);
        }

        let price = if upgraded_rod == ItemId::BAMBOO_POLE {
            0
        } else if upgraded_rod == ItemId::FIBERGLASS_ROD {
            1800
        } else if upgraded_rod == ItemId::IRIDIUM_ROD {
            7500
        } else {
            return Ok(None);
        };

        let current_money = game_state.player.current_money;

        let multiplier = game_state.daily.fish_price_multiplier();
        let iter_sellable = || {
            inventory.iter_items().filter(|item| {
                matches!(item.category, Some(ItemCategory::Fish))
                    || item.id == ItemId::SONAR_BOBBER
            })
        };
        let fish_money = iter_sellable()
            .map(|item| {
                let multiplier = match &item.category {
                    Some(ItemCategory::Fish) => multiplier,
                    _ => 1.0,
                };
                item.stack_price_with_perk(multiplier)
            })
            .sum::<i32>();

        let available_money = current_money + fish_money;

        if available_money < price {
            return Ok(None);
        }

        if current_money < price {
            let goal =
                SellToMerchantGoal::new("Buy Fish", iter_sellable().cloned());
            return Ok(Some(goal.into()));
        }

        if upgraded_rod == ItemId::BAMBOO_POLE {
            let goal = MovementGoal::new(
                "Beach",
                game_state.closest_entrance("Beach")?.into(),
            )
            .with_tolerance(1000.0);
            Ok(Some(goal.into()))
        } else {
            let goal = BuyFromMerchantGoal::new("Buy Fish", upgraded_rod);
            Ok(Some(goal.into()))
        }
    }

    fn next_upgrade(
        &self,
        game_state: &GameState,
    ) -> Result<Option<ItemId>, Error> {
        let opt_fishing_rod_level = game_state
            .iter_accessible_items()?
            .filter_map(|item| item.as_fishing_rod())
            .map(|rod| rod.num_attachment_slots)
            .max();

        let next_upgrade = match opt_fishing_rod_level {
            None => Some(ItemId::BAMBOO_POLE),
            Some(0) => Some(ItemId::FIBERGLASS_ROD),
            Some(1) => Some(ItemId::IRIDIUM_ROD),
            Some(2) => {
                // TODO: Handle obtaining the Deluxe Iridium Rod.  For
                // now, returning None to indicate that there is no
                // available upgrade.
                None
            }
            _ => None,
        };

        Ok(next_upgrade)
    }

    fn fish_shop_open(&self, game_state: &GameState) -> bool {
        // The fish shop is closed on the first day of the game, and
        // on every Saturday.
        let days_played = game_state.globals.days_played();
        let closed_today = days_played == 1 || days_played % 7 == 6;

        let in_game_time = game_state.globals.in_game_time;
        !closed_today && (900..1700).contains(&in_game_time)
    }
}

impl BotGoal for UpgradeFishingRodGoal {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Upgrade fishing rod".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        if let Some(next_step) = self.next_step(game_state)? {
            Ok(next_step.into())
        } else {
            Ok(BotGoalResult::Completed)
        }
    }
}

impl UnloadFishingRod {
    pub fn new(rod: ItemId) -> Self {
        Self { rod }
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        let inventory = &game_state.player.inventory;

        let has_loaded_fishing_rod = inventory
            .iter_items()
            .filter(|item| item.id == self.rod)
            .any(|item| {
                item.as_fishing_rod()
                    .map(|rod| rod.bait.is_some() || rod.tackle.is_some())
                    .unwrap_or(false)
            });

        let has_empty_slot = inventory.has_empty_slot();

        let can_unload = has_loaded_fishing_rod && has_empty_slot;

        !can_unload
    }
}

impl BotGoal for UnloadFishingRod {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        format!("Unload {}", self.rod).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let inventory = &game_state.player.inventory;
        let opt_rod_slot = inventory
            .iter_filled_slots()
            .filter(|(_, item)| item.id == self.rod)
            .find(|(_, item)| {
                item.as_fishing_rod()
                    .map(|rod| rod.bait.is_some() || rod.tackle.is_some())
                    .unwrap_or(false)
            })
            .map(|(slot, _)| slot);

        let opt_empty_slot = inventory.empty_slot();

        let (Some(rod_slot), Some(empty_slot)) = (opt_rod_slot, opt_empty_slot)
        else {
            let cleanup = MenuCloser::new();
            if !cleanup.is_completed(game_state) {
                return Ok(cleanup.into());
            }
            return Ok(BotGoalResult::Completed);
        };

        let Some(pause) = &game_state.pause_menu else {
            actions.do_action(GameAction::ExitMenu);
            return Ok(BotGoalResult::InProgress);
        };

        let Some(page) = pause.inventory_page() else {
            actions.do_action(GameAction::MouseOverPixel(pause.tab_buttons[0]));
            actions.do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        };

        if page.held_item.is_none() {
            let pixel = page.player_item_locations[rod_slot];
            actions.do_action(GameAction::MouseOverPixel(pixel));
            actions.do_action(GameAction::RightClick);
            return Ok(BotGoalResult::InProgress);
        }

        let pixel = page.player_item_locations[empty_slot];
        actions.do_action(GameAction::MouseOverPixel(pixel));
        actions.do_action(GameAction::LeftClick);

        Ok(BotGoalResult::InProgress)
    }
}

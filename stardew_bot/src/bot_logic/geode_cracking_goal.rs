use crate::{
    bot_logic::{BotError, GoToActionTile, InventoryGoal, MenuCloser},
    game_state::{Item, ItemCategory, ItemId},
    Error, GameAction, GameState,
};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult, LogicStack},
    SellToMerchantGoal,
};

pub struct GeodeCrackingGoal {
    sell_minerals: bool,
    sell_gems: bool,
}

impl GeodeCrackingGoal {
    const GEODE_TYPES: [Item; 4] = [
        Item::GEODE,
        Item::FROZEN_GEODE,
        Item::MAGMA_GEODE,
        Item::OMNI_GEODE,
    ];

    pub fn new() -> Self {
        Self {
            sell_minerals: false,
            sell_gems: false,
        }
    }

    pub fn sell_minerals(self, sell_minerals: bool) -> Self {
        Self {
            sell_minerals,
            ..self
        }
    }

    pub fn sell_gems(self, sell_gems: bool) -> Self {
        Self { sell_gems, ..self }
    }

    #[allow(dead_code)]
    pub fn is_completed(&self, game_state: &GameState) -> Result<bool, Error> {
        let mut available =
            InventoryGoal::current().total_stored_and_carried(game_state)?;
        if let Some(held_item) = Self::held_item(game_state) {
            *available.entry(&held_item.id).or_default() += held_item.count;
        }

        let has_any_geode = Self::GEODE_TYPES
            .iter()
            .any(|geode| available.contains_key(&geode.id));

        Ok(!has_any_geode)
    }

    fn held_item(game_state: &GameState) -> Option<&Item> {
        if let Some(menu) = &game_state.geode_menu {
            if let Some(held_item) = &menu.held_item {
                return Some(held_item);
            }
        }
        None
    }

    fn next_geode(
        &self,
        game_state: &GameState,
    ) -> Result<Option<ItemId>, Error> {
        let mut carried = game_state.player.inventory.to_hash_map();
        if let Some(held_item) = Self::held_item(game_state) {
            *carried.entry(held_item.id.clone()).or_default() +=
                held_item.count;
        }

        let carried: [_; 4] = std::array::from_fn(|i| {
            let kind = Self::GEODE_TYPES[i].id.clone();
            let count = carried.get(&kind).cloned().unwrap_or(0);
            (kind, count)
        });

        // TODO: Predict geode output for best order of cracking
        let opt_next = carried
            .into_iter()
            .filter(|(_, count)| *count > 0)
            .max_by_key(|(_, count)| *count)
            .map(|(id, _)| id);

        Ok(opt_next)
    }
}

impl BotGoal for GeodeCrackingGoal {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Crack geodes".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let inventory = &game_state.player.inventory;

        let opt_next_geode = self.next_geode(game_state)?;

        let to_sell: LogicStack = inventory
            .iter_items()
            .filter(|_| game_state.player.room_name == "Blacksmith")
            .filter(|item| {
                item.category
                    .map(|category| match category {
                        ItemCategory::Gem => self.sell_gems,
                        ItemCategory::Mineral => self.sell_minerals,
                        _ => false,
                    })
                    .unwrap_or(false)
            })
            .map(|item| SellToMerchantGoal::new("Blacksmith", item.clone()))
            .collect();

        let has_full_inventory = inventory.num_empty_slots().saturating_sub(
            if Self::held_item(game_state).is_some() {
                1
            } else {
                0
            },
        ) == 0;

        if opt_next_geode.is_none() || has_full_inventory {
            if game_state.geode_menu.is_some() {
                return Ok(MenuCloser::new().into());
            } else if to_sell.len() > 0 {
                return Ok(to_sell.into());
            }
        }

        let Some(next_geode) = opt_next_geode else {
            return Ok(BotGoalResult::Completed);
        };

        let prepare = Self::GEODE_TYPES
            .iter()
            .fold(InventoryGoal::current().with(Item::HOE), |goal, geode| {
                goal.with(geode.clone().with_count(1000))
            });
        if !prepare.is_completed(game_state)? {
            return Ok(prepare.otherwise_empty().into());
        }

        if let Some(menu) = &game_state.dialogue_menu {
            if menu.responses.len() < 3 {
                // Not the blacksmith menu, so close it and proceed.
                return Ok(MenuCloser::new().into());
            }

            actions.do_action(GameAction::MouseOverPixel(menu.responses[2]));
            actions.do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        }

        let Some(menu) = &game_state.geode_menu else {
            return Ok(GoToActionTile::new("Blacksmith").into());
        };

        let pixel = if let Some(held_item) = &menu.held_item {
            if held_item.is_same_item(next_geode) {
                // The cursor is holding the correct geode, so drop it onto
                // Clint's anvil.
                menu.crack_geode_button
            } else {
                // The cursor is holding an item, but it's not the
                // geode we want to open.
                let empty_slot = inventory
                    .empty_slot()
                    .ok_or(BotError::ExpectedEmptySlot)?;
                menu.player_item_locations[empty_slot]
            }
        } else {
            // The cursor is not holding an item, so pick one up.
            let next_slot = inventory
                .current_slot(&next_geode)
                .ok_or_else(|| BotError::ExpectedItemInInventory(next_geode))?;
            menu.player_item_locations[next_slot]
        };

        actions.do_action(GameAction::MouseOverPixel(pixel));
        actions.do_action(GameAction::LeftClick);
        Ok(BotGoalResult::InProgress)
    }
}

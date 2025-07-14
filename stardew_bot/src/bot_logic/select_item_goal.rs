use crate::{Error, GameAction, GameState};
use game_state::{Item, ItemId, Key};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    MenuCloser,
};

pub struct SelectItemGoal {
    item: ItemId,
}

impl SelectItemGoal {
    pub fn new(item: ItemId) -> Self {
        Self { item }
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        self.current_inventory_slot(game_state)
            == Some(game_state.player.active_hotbar_index)
    }

    pub fn current_inventory_slot(
        &self,
        game_state: &GameState,
    ) -> Option<usize> {
        game_state.player.inventory.current_slot(&self.item)
    }

    fn key_to_hotbar(key: Key) -> Option<usize> {
        match key {
            Key::Digit1 => Some(0),
            Key::Digit2 => Some(1),
            Key::Digit3 => Some(2),
            Key::Digit4 => Some(3),
            Key::Digit5 => Some(4),
            Key::Digit6 => Some(5),
            Key::Digit7 => Some(6),
            Key::Digit8 => Some(7),
            Key::Digit9 => Some(8),
            Key::Digit0 => Some(9),
            Key::OemMinus => Some(10),
            Key::OemPlus => Some(11),
            _ => None,
        }
    }
}

impl BotGoal for SelectItemGoal {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        format!("Select {}", self.item).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let opt_cleanup = game_state
            .inputs
            .keys_pressed
            .iter()
            .copied()
            .find_map(Self::key_to_hotbar)
            .map(|index| GameAction::StopSelectingHotbar(index));

        if let Some(cleanup) = opt_cleanup {
            actions.do_action(cleanup);
            return Ok(BotGoalResult::InProgress);
        }

        let opt_held_item = game_state
            .pause_menu
            .as_ref()
            .and_then(|pause| pause.inventory_page())
            .and_then(|inventory_page| {
                inventory_page
                    .held_item
                    .as_ref()
                    .map(|held_item| (inventory_page, held_item))
            });

        if let Some((inventory_page, held_item)) = opt_held_item {
            let dest_slots = if held_item.is_same_item(&self.item) {
                0..12
            } else {
                let inventory_size = game_state.player.inventory.items.len();
                12..inventory_size
            };
            let dest_slot = game_state
                .player
                .inventory
                .iter_slots()
                .enumerate()
                .skip(dest_slots.start)
                .take(dest_slots.end - dest_slots.start)
                .find(|(_, opt_item)| opt_item.is_none())
                .map(|(i, _)| i)
                .unwrap_or_else(|| {
                    // Fallback, game tick is good enough for a random
                    // selection.
                    let num_slots = dest_slots.end - dest_slots.start;
                    let offset =
                        (game_state.globals.game_tick as usize) % num_slots;
                    dest_slots.start + offset
                });

            let pixel = inventory_page.player_item_locations[dest_slot];
            actions.do_action(GameAction::MouseOverPixel(pixel));
            actions.do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        }

        let opt_current_slot = self.current_inventory_slot(game_state);

        let is_completed_or_impossible = opt_current_slot
            .map(|slot| slot == game_state.player.active_hotbar_index)
            .unwrap_or(true);
        if is_completed_or_impossible {
            let cleanup = MenuCloser::new();
            if cleanup.is_completed(game_state) {
                return Ok(BotGoalResult::Completed);
            } else {
                return Ok(cleanup.into());
            }
        }

        let current_slot =
            opt_current_slot.expect("Would hit early return for None");

        if current_slot < 12 {
            // The item is currently in the hotbar, so we just need to
            // select it.
            actions.do_action(GameAction::SelectHotbar(current_slot));
            return Ok(BotGoalResult::InProgress);
        }

        let Some(pause) = &game_state.pause_menu else {
            actions.do_action(GameAction::ExitMenu);
            return Ok(BotGoalResult::InProgress);
        };

        let Some(inventory_page) = pause.inventory_page() else {
            actions.do_action(GameAction::MouseOverPixel(pause.tab_buttons[0]));
            actions.do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        };

        let pixel = inventory_page.player_item_locations[current_slot];
        actions.do_action(GameAction::MouseOverPixel(pixel));
        actions.do_action(GameAction::LeftClick);
        Ok(BotGoalResult::InProgress)
    }
}

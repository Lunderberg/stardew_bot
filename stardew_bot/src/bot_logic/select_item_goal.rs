use crate::{
    game_state::{Item, Key},
    Error, GameAction, GameState,
};

use super::bot_logic::{BotGoal, BotGoalResult};

pub struct SelectItemGoal {
    item: Item,
}

impl SelectItemGoal {
    pub fn new(item: Item) -> Self {
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
        game_state
            .player
            .inventory
            .items
            .iter()
            .enumerate()
            .find(|(_, opt_item)| {
                opt_item
                    .as_ref()
                    .map(|item| self.item.is_same_item(item))
                    .unwrap_or(false)
            })
            .map(|(i, _)| i)
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
    fn description(&self) -> std::borrow::Cow<str> {
        format!("Select {}", self.item).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
    ) -> Result<BotGoalResult, Error> {
        let opt_cleanup = game_state
            .inputs
            .keys_pressed
            .iter()
            .copied()
            .find_map(Self::key_to_hotbar)
            .map(|index| GameAction::StopSelectingHotbar(index));

        if let Some(cleanup) = opt_cleanup {
            return Ok(cleanup.into());
        }

        let opt_current_slot = self.current_inventory_slot(game_state);

        let is_completed_or_impossible = opt_current_slot
            .map(|slot| slot == game_state.player.active_hotbar_index)
            .unwrap_or(true);
        if is_completed_or_impossible {
            return Ok(BotGoalResult::Completed);
        }

        let current_slot =
            opt_current_slot.expect("Would hit early return for None");

        if current_slot < 12 {
            // The item is currently in the hotbar, so we just need to
            // select it.
            let action = GameAction::SelectHotbar(current_slot);
            return Ok(action.into());
        }

        todo!("Handle case where item must be moved to the hotbar")
    }
}

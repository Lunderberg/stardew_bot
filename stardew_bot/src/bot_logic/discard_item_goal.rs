use crate::{game_state::Item, Error, GameAction, GameState};

use super::bot_logic::{BotGoal, BotGoalResult};

pub struct DiscardItemGoal {
    item: Item,
}

impl DiscardItemGoal {
    pub fn new(item: Item) -> Self {
        Self { item }
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        !game_state.player.inventory.contains(&self.item)
    }
}

impl BotGoal for DiscardItemGoal {
    fn description(&self) -> std::borrow::Cow<str> {
        format!("Discard {}", self.item).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        do_action: &mut dyn FnMut(GameAction),
    ) -> Result<BotGoalResult, Error> {
        if let Some(page) = game_state
            .pause_menu
            .as_ref()
            .and_then(|pause| pause.inventory_page())
        {
            if let Some(held_item) = &page.held_item {
                let pixel = if held_item.is_same_item(&self.item) {
                    page.trash_can
                } else {
                    let slot = game_state.player.inventory.empty_slot().expect(
                        "TODO: Handle case where held item has nowhere to go",
                    );
                    page.player_item_locations[slot]
                };
                do_action(GameAction::MouseOverPixel(pixel));
                do_action(GameAction::LeftClick);
                return Ok(BotGoalResult::InProgress);
            }
        }

        let Some(slot) = game_state.player.inventory.current_slot(&self.item)
        else {
            if let Some(menu) = &game_state.pause_menu {
                do_action(GameAction::MouseOverPixel(menu.exit_button));
                do_action(GameAction::LeftClick);
                return Ok(BotGoalResult::InProgress);
            } else {
                return Ok(BotGoalResult::Completed);
            }
        };

        let Some(pause) = &game_state.pause_menu else {
            do_action(GameAction::ExitMenu);
            return Ok(BotGoalResult::InProgress);
        };

        let Some(inventory_page) = pause.inventory_page() else {
            do_action(GameAction::MouseOverPixel(pause.tab_buttons[0]));
            do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        };

        assert!(
            inventory_page.held_item.is_none(),
            "Should be handled prior to the is_completed step"
        );

        let pixel = inventory_page.player_item_locations[slot];
        do_action(GameAction::MouseOverPixel(pixel));
        do_action(GameAction::LeftClick);
        Ok(BotGoalResult::InProgress)
    }
}

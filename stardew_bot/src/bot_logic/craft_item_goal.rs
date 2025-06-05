use crate::{
    bot_logic::{bot_logic::SubGoals, MovementGoal},
    game_state::{Inventory, Item, Key, ObjectKind, Quality},
    Error, GameAction, GameState,
};

use super::bot_logic::{BotGoal, BotGoalResult};

pub struct CraftItemGoal {
    item: Item,
}

impl CraftItemGoal {
    pub fn new(item: Item) -> Self {
        Self { item }
    }

    pub fn item_count(&self, game_state: &GameState) -> usize {
        game_state.player.inventory.count_item(&self.item)
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        let num_in_inventory = self.item_count(game_state);
        num_in_inventory >= self.item.count
    }
}

impl BotGoal for CraftItemGoal {
    fn description(&self) -> std::borrow::Cow<str> {
        format!("Craft {}", self.item).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        do_action: &mut dyn FnMut(GameAction),
    ) -> Result<BotGoalResult, Error> {
        if self.is_completed(game_state) {
            if let Some(pause) = &game_state.pause_menu {
                do_action(GameAction::MouseOverPixel(pause.exit_button));
                do_action(GameAction::LeftClick);
                return Ok(BotGoalResult::InProgress);
            } else {
                return Ok(BotGoalResult::Completed);
            }
        }

        let Some(pause) = &game_state.pause_menu else {
            do_action(GameAction::ExitMenu);
            return Ok(BotGoalResult::InProgress);
        };

        let Some(crafting_page) = pause.crafting_page() else {
            do_action(GameAction::MouseOverPixel(pause.tab_buttons[4]));
            do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        };

        let recipe = crafting_page
            .recipes
            .iter()
            .find(|recipe| recipe.item.is_same_item(&self.item))
            .expect("TODO: Handle case where the recipe isn't available");

        if crafting_page.current_recipe_page < recipe.recipe_page {
            let down_button = crafting_page.down_button.expect(
                "Down button should be shown for multiple recipe pages",
            );
            do_action(GameAction::MouseOverPixel(down_button));
            do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        } else if crafting_page.current_recipe_page > recipe.recipe_page {
            let up_button = crafting_page
                .up_button
                .expect("Up button should be shown for multiple recipe pages");
            do_action(GameAction::MouseOverPixel(up_button));
            do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        }

        if let Some(held_item) = &crafting_page.held_item {
            if !held_item.is_same_item(&self.item)
                || held_item.count >= self.item.count
            {
                let open_inventory_slot = game_state
                    .player
                    .inventory
                    .iter_slots()
                    .zip(crafting_page.player_item_locations.iter())
                    .filter(|(opt_slot, _)| opt_slot.is_none())
                    .map(|(_, pixel)| *pixel)
                    .next()
                    .expect("TODO: Handle full inventory");
                do_action(GameAction::MouseOverPixel(open_inventory_slot));
                do_action(GameAction::LeftClick);

                return Ok(BotGoalResult::InProgress);
            }
        }

        do_action(GameAction::MouseOverPixel(recipe.pixel_location));
        do_action(GameAction::LeftClick);

        Ok(BotGoalResult::InProgress)
    }
}

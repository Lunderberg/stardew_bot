use crate::{
    bot_logic::{bot_logic::LogicStack, MovementGoal},
    Error, GameAction, GameState,
};
use game_state::{Inventory, Item, Key, ObjectKind, Quality};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    MenuCloser,
};

pub struct CraftItemGoal {
    item: Item,
}

impl CraftItemGoal {
    pub fn new(item: impl Into<Item>) -> Self {
        let item = item.into();
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
    fn description(&self) -> std::borrow::Cow<'static, str> {
        format!("Craft {}", self.item).into()
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

        let Some(pause) = &game_state.pause_menu else {
            actions.do_action(GameAction::ExitMenu);
            return Ok(BotGoalResult::InProgress);
        };

        let Some(crafting_page) = pause.crafting_page() else {
            actions.do_action(GameAction::MouseOverPixel(pause.tab_buttons[4]));
            actions.do_action(GameAction::LeftClick);
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
            actions.do_action(GameAction::MouseOverPixel(down_button));
            actions.do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        } else if crafting_page.current_recipe_page > recipe.recipe_page {
            let up_button = crafting_page
                .up_button
                .expect("Up button should be shown for multiple recipe pages");
            actions.do_action(GameAction::MouseOverPixel(up_button));
            actions.do_action(GameAction::LeftClick);
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
                actions
                    .do_action(GameAction::MouseOverPixel(open_inventory_slot));
                actions.do_action(GameAction::LeftClick);

                return Ok(BotGoalResult::InProgress);
            }
        }

        actions.do_action(GameAction::MouseOverPixel(recipe.pixel_location));
        actions.do_action(GameAction::LeftClick);

        Ok(BotGoalResult::InProgress)
    }
}

use crate::{Error, GameAction};
use game_state::{GameState, Item, ItemId};

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

    pub fn is_completable(&self, game_state: &GameState) -> bool {
        let inventory = &game_state.player.inventory;
        let carrying = inventory.to_hash_map();
        let get_count = |id: &ItemId| carrying.get(id).cloned().unwrap_or(0);

        let num_in_inventory = get_count(&self.item.id);
        let iter_ingredients =
            || self.item.id.iter_recipe().into_iter().flatten();
        let num_craftable = iter_ingredients()
            .map(|(ingredient, count)| get_count(&ingredient) / count)
            .min()
            .unwrap_or(0);

        let can_craft_enough =
            num_in_inventory + num_craftable >= self.item.count;
        let have_inventory_space = num_in_inventory > 0
            || inventory.has_empty_slot()
            || iter_ingredients().any(|(ingredient, count)| {
                num_craftable * count == get_count(&ingredient)
            });

        can_craft_enough && have_inventory_space
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

        let num_in_inventory = self.item_count(game_state);
        if let Some(held_item) = &crafting_page.held_item {
            if !held_item.is_same_item(&self.item)
                || num_in_inventory + held_item.count >= self.item.count
            {
                let inventory = &game_state.player.inventory;
                let inventory_slot = inventory
                    .preferred_slot(&self.item)
                    .or_else(|| inventory.empty_slot())
                    .expect("TODO: Handle full inventory");
                let inventory_pixel =
                    crafting_page.player_item_locations[inventory_slot];

                actions.do_action(GameAction::MouseOverPixel(inventory_pixel));
                actions.do_action(GameAction::LeftClick);

                return Ok(BotGoalResult::InProgress);
            }
        }

        actions.do_action(GameAction::MouseOverPixel(recipe.pixel_location));
        actions.do_action(GameAction::LeftClick);

        Ok(BotGoalResult::InProgress)
    }
}

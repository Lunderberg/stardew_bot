use game_state::{FishingRod, GameState, Item, ItemCategory, ItemId};

use crate::{
    Error, GameAction, MenuCloser,
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
};

pub struct LoadUpFishingRod {
    /// The type of bait to load onto the fishing rod.  If None, will
    /// load the same type of bait as is currently on the rod.
    bait: Option<ItemId>,

    /// The type of tackle to load onto the fishing rod.  If None,
    /// will select `Dressed Spinner`, or `Spinner`, depending on
    /// availability.
    tackle: Option<ItemId>,
}

impl LoadUpFishingRod {
    pub fn new() -> Self {
        Self {
            bait: None,
            tackle: None,
        }
    }

    pub fn bait(self, bait: ItemId) -> Self {
        Self {
            bait: Some(bait),
            ..self
        }
    }

    /// Find the player's fishing rod
    ///
    /// Returns a tuple containing the inventory slot that contains
    /// the inventory slot of the fishing rod and the `FishingRod`
    /// info.
    fn find_fishing_rod<'a>(
        &self,
        game_state: &'a GameState,
    ) -> Option<(usize, &'a FishingRod)> {
        game_state
            .player
            .inventory
            .iter_filled_slots()
            .filter_map(|(slot, item)| {
                item.as_fishing_rod().map(|rod| (slot, rod))
            })
            .max_by_key(|(_, rod)| rod.num_attachment_slots)
    }

    fn iter_items(game_state: &GameState) -> impl Iterator<Item = &Item> + '_ {
        let opt_held_item = game_state
            .pause_menu()
            .and_then(|menu| menu.inventory_page())
            .and_then(|page| page.held_item.as_ref());

        opt_held_item
            .into_iter()
            .chain(game_state.player.inventory.iter_items())
    }

    fn select_bait(
        &self,
        game_state: &GameState,
        fishing_rod: &FishingRod,
    ) -> Option<ItemId> {
        self.bait
            .as_ref()
            .or_else(|| fishing_rod.bait.as_ref().map(|bait| &bait.id))
            .cloned()
            .or(Some(ItemId::BAIT))
            .filter(|_| fishing_rod.can_use_bait())
            .filter(|bait| {
                Self::iter_items(game_state).any(|item| &item.id == bait)
            })
    }

    fn select_tackle(
        &self,
        game_state: &GameState,
        fishing_rod: &FishingRod,
    ) -> Option<ItemId> {
        self.tackle
            .as_ref()
            .or_else(|| {
                Self::iter_items(game_state)
                    .filter(|item| {
                        matches!(item.category, Some(ItemCategory::Tackle))
                    })
                    .map(|item| &item.id)
                    .filter(|&id| id != &ItemId::SONAR_BOBBER)
                    .max_by_key(|&id| -> i32 {
                        if id == &ItemId::DRESSED_SPINNER {
                            2
                        } else if id == &ItemId::SPINNER {
                            1
                        } else {
                            0
                        }
                    })
            })
            .cloned()
            .filter(|_| {
                fishing_rod.can_use_tackle() && fishing_rod.tackle.is_none()
            })
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        let Some((_, rod)) = self.find_fishing_rod(game_state) else {
            return true;
        };

        let opt_bait = self.select_bait(game_state, rod);
        let opt_tackle = self.select_tackle(game_state, rod);

        opt_bait.is_none() && opt_tackle.is_none()
    }
}

impl BotGoal for LoadUpFishingRod {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Load bait/tackle on fishing rod".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let Some((rod_slot, rod)) = self.find_fishing_rod(game_state) else {
            return Ok(BotGoalResult::Completed);
        };

        let opt_bait = self.select_bait(game_state, rod);
        let opt_tackle = self.select_tackle(game_state, rod);

        if let Some(pause) = game_state.pause_menu()
            && let Some(page) = pause.inventory_page()
            && let Some(held_item) = &page.held_item
        {
            let is_holding_correct_bait = opt_bait
                .map(|bait| held_item.is_same_item(bait))
                .unwrap_or(false);
            let is_holding_correct_tackle = opt_tackle
                .map(|tackle| held_item.is_same_item(tackle))
                .unwrap_or(false);
            if is_holding_correct_bait || is_holding_correct_tackle {
                // The cursor is holding the bait, so load it
                // onto the fishing rod.
                let pixel = page.player_item_locations[rod_slot];
                actions.do_action(GameAction::MouseOverPixel(pixel));
                actions.do_action(GameAction::RightClick);
                return Ok(BotGoalResult::InProgress);
            }

            // The inventory currently holds something other
            // than the bait to be loaded (e.g. different bait
            // that has just been removed from the fishing
            // rod).  Place it in an empty slot of the
            // inventory.
            let slot =
                game_state.player.inventory.empty_slot().expect(
                    "TODO: Handle case where held item has nowhere to go",
                );
            let pixel = page.player_item_locations[slot];
            actions.do_action(GameAction::MouseOverPixel(pixel));
            actions.do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        }

        let opt_slot_to_pick_up = opt_bait
            .and_then(|bait| game_state.player.inventory.current_slot(bait))
            .or_else(|| {
                opt_tackle.and_then(|tackle| {
                    game_state.player.inventory.current_slot(tackle)
                })
            });

        let Some(slot_to_pick_up) = opt_slot_to_pick_up else {
            let cleanup = MenuCloser::new();
            if cleanup.is_completed(game_state) {
                return Ok(BotGoalResult::Completed);
            } else {
                return Ok(cleanup.into());
            }
        };

        let Some(pause) = game_state.pause_menu() else {
            actions.do_action(GameAction::ExitMenu);
            return Ok(BotGoalResult::InProgress);
        };

        let Some(page) = pause.inventory_page() else {
            actions.do_action(GameAction::MouseOverPixel(pause.tab_buttons[0]));
            actions.do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        };

        if page.held_item.is_none() {
            let pixel = page.player_item_locations[slot_to_pick_up];
            actions.do_action(GameAction::MouseOverPixel(pixel));
            actions.do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        }

        let pixel = page.player_item_locations[rod_slot];
        actions.do_action(GameAction::MouseOverPixel(pixel));
        actions.do_action(GameAction::RightClick);

        Ok(BotGoalResult::InProgress)
    }
}

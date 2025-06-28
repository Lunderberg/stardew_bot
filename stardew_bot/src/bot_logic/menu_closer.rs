use crate::{game_state::Vector, Error, GameAction, GameState};

use super::bot_logic::{ActionCollector, BotGoal, BotGoalResult};

pub struct MenuCloser {
    previous_click: Option<i32>,
}

impl MenuCloser {
    pub fn new() -> Self {
        Self {
            previous_click: None,
        }
    }

    fn exit_button(&self, game_state: &GameState) -> Option<Vector<isize>> {
        if let Some(menu) = &game_state.pause_menu {
            Some(menu.exit_button)
        } else if let Some(menu) = &game_state.chest_menu {
            Some(menu.ok_button)
        } else if let Some(menu) = &game_state.shop_menu {
            Some(menu.exit_button)
        } else if let Some(menu) = &game_state.dialogue_menu {
            menu.responses
                .is_empty()
                .then(|| menu.pixel_location.center())
        } else if let Some(menu) = &game_state.mail_menu {
            (menu.current_page + 1 < menu.num_pages)
                .then(|| menu.pixel_location.center())
        } else if let Some(menu) = &game_state.geode_menu {
            Some(menu.ok_button)
        } else {
            None
        }
    }

    fn must_press_escape(&self, game_state: &GameState) -> bool {
        game_state
            .mail_menu
            .as_ref()
            .map(|menu| menu.current_page + 1 == menu.num_pages)
            .unwrap_or(false)
            || game_state.mine_elevator_menu.is_some()
    }

    fn drop_held_item(&self, game_state: &GameState) -> Option<Vector<isize>> {
        if let Some(menu) = &game_state.pause_menu {
            if menu.held_item().is_some() {
                let empty_slot = game_state
                    .player
                    .inventory
                    .empty_slot()
                    .expect("TODO: Handle case of fully inventory");

                return Some(
                    menu.player_inventory_tiles().unwrap()[empty_slot],
                );
            }
        }

        if let Some(menu) = &game_state.geode_menu {
            if menu.held_item.is_some() {
                let empty_slot = game_state
                    .player
                    .inventory
                    .empty_slot()
                    .expect("TODO: Handle case of fully inventory");

                return Some(menu.player_item_locations[empty_slot]);
            }
        }

        None
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        self.exit_button(game_state).is_none()
            && !self.must_press_escape(game_state)
    }
}

impl BotGoal for MenuCloser {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Close the pause menu".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        if self.is_completed(game_state) {
            return Ok(BotGoalResult::Completed);
        }

        let should_wait = self
            .previous_click
            .map(|prev_tick| prev_tick + 5 <= game_state.globals.game_tick)
            .unwrap_or(false);
        if should_wait {
            return Ok(BotGoalResult::InProgress);
        }

        self.previous_click = Some(game_state.globals.game_tick);

        if let Some(empty_slot) = self.drop_held_item(game_state) {
            actions
                .do_action(GameAction::MouseOverPixel(empty_slot))
                .annotate("Mouse over empty slot");
            actions
                .do_action(GameAction::LeftClick)
                .annotate("Drop item into empty slot");
        } else if let Some(exit_button) = self.exit_button(game_state) {
            actions
                .do_action(GameAction::MouseOverPixel(exit_button))
                .annotate("Mouse over close menu button");
            actions
                .do_action(GameAction::LeftClick)
                .annotate("Click close menu button");
        } else if self.must_press_escape(game_state) {
            actions
                .do_action(GameAction::ExitMenu)
                .annotate("Use Esc to close menu");
        } else {
            unreachable!("Protected by is_completed()")
        }

        Ok(BotGoalResult::InProgress)
    }
}

use geometry::Vector;

use crate::{Error, GameAction};
use game_state::{DialogueMenu, GameState, Item, Menu};

use super::bot_logic::{ActionCollector, BotGoal, BotGoalResult};

pub struct MenuCloser {
    previous_click: Option<i32>,
}

impl Default for MenuCloser {
    fn default() -> Self {
        Self::new()
    }
}

impl MenuCloser {
    pub fn new() -> Self {
        Self {
            previous_click: None,
        }
    }

    fn is_minecart_menu(menu: &DialogueMenu) -> bool {
        menu.responses
            .iter()
            .filter(|response| {
                response.text == "Bus"
                    || response.text == "Town"
                    || response.text == "Quarry"
                    || response.text == "Mines"
            })
            .count()
            > 2
    }

    fn exit_button(&self, game_state: &GameState) -> Option<Vector<isize>> {
        match game_state.menu.as_ref()? {
            Menu::Pause(menu) => Some(menu.exit_button),
            Menu::Chest(menu) => Some(menu.ok_button),
            Menu::Dialogue(menu) => menu
                .responses
                .is_empty()
                .then(|| menu.pixel_location.center()),
            Menu::Mail(menu) => (menu.current_page + 1 < menu.num_pages)
                .then(|| menu.pixel_location.center()),
            Menu::Shop(menu) => Some(menu.exit_button),
            Menu::Geode(menu) => Some(menu.ok_button),
            Menu::MineElevator(_) => None,
            Menu::Junimo(_) => None,
            Menu::Other => None,
        }
    }

    fn must_press_escape(&self, game_state: &GameState) -> bool {
        let Some(menu) = &game_state.menu else {
            return false;
        };

        match menu {
            Menu::Pause(_) => false,
            Menu::Chest(_) => false,
            Menu::Dialogue(menu) => Self::is_minecart_menu(menu),
            Menu::Mail(menu) => menu.current_page + 1 == menu.num_pages,
            Menu::Shop(_) => false,
            Menu::Geode(_) => false,
            Menu::MineElevator(_) => true,
            Menu::Junimo(_) => true,
            Menu::Other => false,
        }
    }

    fn drop_held_item(&self, game_state: &GameState) -> Option<Vector<isize>> {
        let opt_held_item_and_inv_tiles: Option<(&Item, &[Vector<isize>])> =
            game_state.menu.as_ref().and_then(|menu| match menu {
                Menu::Pause(menu) => menu.held_item().and_then(|held_item| {
                    menu.player_inventory_tiles().map(
                        |player_inventory_tiles| {
                            (held_item, player_inventory_tiles)
                        },
                    )
                }),

                Menu::Shop(menu) => menu.held_item.as_ref().map(|held_item| {
                    (held_item, menu.player_item_locations.as_slice())
                }),
                Menu::Geode(menu) => menu.held_item.as_ref().map(|held_item| {
                    (held_item, menu.player_item_locations.as_slice())
                }),
                Menu::Junimo(menu) => {
                    menu.held_item.as_ref().map(|held_item| {
                        (held_item, menu.player_item_locations.as_slice())
                    })
                }

                Menu::Chest(_)
                | Menu::Dialogue(_)
                | Menu::Mail(_)
                | Menu::MineElevator(_)
                | Menu::Other => None,
            });

        opt_held_item_and_inv_tiles.map(
            |(held_item, player_inventory_tiles)| {
                let output_slot = game_state
                    .player
                    .inventory
                    .preferred_slot(held_item)
                    .expect("TODO: Handle case of fully inventory");

                player_inventory_tiles[output_slot]
            },
        )
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        self.exit_button(game_state).is_none()
            && !self.must_press_escape(game_state)
    }
}

impl BotGoal for MenuCloser {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Close menus".into()
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
            .map(|prev_tick| prev_tick + 5 <= game_state.globals.game_mode_tick)
            .unwrap_or(false);
        if should_wait {
            return Ok(BotGoalResult::InProgress);
        }

        self.previous_click = Some(game_state.globals.game_mode_tick);

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

use crate::{
    bot_logic::{bot_logic::SubGoals, MovementGoal},
    game_state::{Inventory, Item, Key, ObjectKind, Quality},
    Error, GameAction, GameState,
};

use super::bot_logic::{BotGoal, BotGoalResult};

pub struct InventoryGoal {
    item: Item,
}

impl InventoryGoal {
    #[allow(dead_code)]
    pub fn new(item: Item) -> Self {
        Self { item }
    }
}

impl InventoryGoal {
    pub fn contains_target_item(&self, inventory: &Inventory) -> bool {
        let num_found = inventory
            .items
            .iter()
            .filter_map(|opt_item| opt_item.as_ref())
            .filter(|item| self.item.is_same_item(item))
            .map(|item| item.count)
            .sum::<usize>();

        num_found >= self.item.count
    }
}

impl BotGoal for InventoryGoal {
    fn description(&self) -> std::borrow::Cow<str> {
        format!("Pick up {}", self.item).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        do_action: &mut dyn FnMut(GameAction),
    ) -> Result<BotGoalResult, Error> {
        // TODO: Handle these are part of some return-to-default
        // logic, rather than needing each goal to release each
        // button.
        {
            let mut cleanup = false;
            if game_state.inputs.right_mouse_down() {
                do_action(GameAction::ReleaseRightClick);
                cleanup = true;
            }
            if game_state.inputs.left_mouse_down() {
                do_action(GameAction::ReleaseLeftClick);
                cleanup = true;
            }
            if game_state.inputs.keys_pressed.contains(&Key::Escape) {
                do_action(GameAction::StopExitingMenu);
                cleanup = true;
            }
            if cleanup {
                return Ok(BotGoalResult::InProgress);
            }
        }

        let item_slot = |inventory: &Inventory| -> Option<usize> {
            inventory
                .items
                .iter()
                .enumerate()
                .filter_map(|(i, opt_item)| {
                    opt_item.as_ref().map(|item| (i, item))
                })
                .find(|(_, item)| self.item.is_same_item(item))
                .map(|(i, _)| i)
        };

        let player_has_item =
            self.contains_target_item(&game_state.player.inventory);

        if let Some(chest_menu) = &game_state.chest_menu {
            if player_has_item {
                do_action(GameAction::MouseOverPixel(chest_menu.ok_button));
                do_action(GameAction::LeftClick);
            } else if let Some(chest_index) = item_slot(&chest_menu.chest_items)
            {
                let pixel = chest_menu.chest_item_locations[chest_index];
                do_action(GameAction::MouseOverPixel(pixel));
                do_action(GameAction::LeftClick);
            } else {
                do_action(GameAction::MouseOverPixel(chest_menu.ok_button));
                do_action(GameAction::LeftClick);
            };
            return Ok(BotGoalResult::InProgress);
        }

        if player_has_item {
            let mut state = BotGoalResult::Completed;
            if game_state.inputs.left_mouse_down() {
                do_action(GameAction::ReleaseLeftClick);
                state = BotGoalResult::InProgress;
            }
            if game_state.inputs.keys_pressed.contains(&Key::Escape) {
                do_action(GameAction::StopExitingMenu);
                state = BotGoalResult::InProgress;
            };
            return Ok(state);
        }

        let opt_chest_location = game_state
            .locations
            .iter()
            .flat_map(|loc| {
                loc.objects
                    .iter()
                    .filter_map(|obj| match &obj.kind {
                        ObjectKind::Chest(inventory) => {
                            Some((obj.tile, inventory))
                        }
                        _ => None,
                    })
                    .map(move |(tile, items)| (loc, tile, items))
            })
            .find(|(_, _, items)| self.contains_target_item(items))
            .map(|(loc, tile, _)| (&loc.name, tile));

        let Some((chest_room, chest_tile)) = opt_chest_location else {
            todo!("Handle case where item isn't found in any chest")
        };

        if chest_room != &game_state.player.room_name
            || game_state.player.tile().manhattan_dist(chest_tile) > 1
        {
            let goals = SubGoals::new().then(
                MovementGoal::new(
                    chest_room.into(),
                    chest_tile.map(|x| x as f32),
                )
                .with_tolerance(1.1),
            );
            return Ok(goals.into());
        }

        do_action(GameAction::MouseOverTile(chest_tile));
        do_action(GameAction::RightClick);

        Ok(BotGoalResult::InProgress)
    }
}

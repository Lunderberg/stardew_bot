use crate::{game_state::Item, Error, GameAction, GameState};

use super::bot_logic::{BotGoal, BotGoalResult};

pub struct OrganizeInventoryGoal<Func> {
    func: Func,
}

pub enum SortedInventoryLocation {
    /// Prefer to show this item in the left side of the hotbar.
    HotBarLeft,

    /// Prefer to show this item on the left side of the hotbar.
    HotBarRight,

    /// Prefer to show this item anywhere on the hotbar.
    HotBar,

    /// Prefer to keep this item out of the hotbar.
    Hidden,
}

impl<Func> OrganizeInventoryGoal<Func> {
    pub fn new(func: Func) -> Self
    where
        Self: BotGoal,
    {
        Self { func }
    }

    /// Plan which item should be picked up next.
    fn plan_next_move(&self, game_state: &GameState) -> Option<usize>
    where
        Func: Fn(&Item) -> SortedInventoryLocation,
    {
        let func = &self.func;
        let opt_held_item = game_state
            .pause_menu
            .as_ref()
            .and_then(|menu| menu.held_item())
            .map(func);

        let iter_slots = || game_state.player.inventory.iter_slots();

        let num_hotbar_left = iter_slots()
            .take(12)
            .take_while(|opt_item| {
                matches!(
                    opt_item.map(func),
                    Some(SortedInventoryLocation::HotBarLeft)
                )
            })
            .count();

        let num_hotbar_right = iter_slots()
            .take(12)
            .rev()
            .take_while(|opt_item| {
                matches!(
                    opt_item.map(func),
                    Some(SortedInventoryLocation::HotBarRight)
                )
            })
            .count();

        let empty_hidden_slot = iter_slots()
            .enumerate()
            .skip(12)
            .find(|(_, opt_item)| opt_item.is_none())
            .map(|(i, _)| i);

        let empty_hotbar_slot = iter_slots()
            .enumerate()
            .take(12)
            .find(|(_, opt_item)| opt_item.is_none())
            .map(|(i, _)| i);

        let hide_from_hotbar = iter_slots()
            .enumerate()
            .take(12)
            .find(|(_, opt_item)| {
                matches!(
                    opt_item.map(func),
                    Some(SortedInventoryLocation::Hidden)
                )
            })
            .map(|(i, _)| i);

        let show_in_hotbar = iter_slots()
            .enumerate()
            .skip(12)
            .find(|(_, opt_item)| {
                !matches!(
                    opt_item.map(func),
                    Some(SortedInventoryLocation::Hidden)
                )
            })
            .map(|(i, _)| i);

        let move_within_hotbar = iter_slots()
            .enumerate()
            .take(12)
            .skip(num_hotbar_left)
            .rev()
            .skip(num_hotbar_right)
            .find(|(_, opt_item)| {
                matches!(
                    opt_item.map(func),
                    Some(
                        SortedInventoryLocation::HotBarLeft
                            | SortedInventoryLocation::HotBarRight
                    )
                )
            })
            .map(|(i, _)| i);

        if let Some(held_item) = opt_held_item {
            match held_item {
                SortedInventoryLocation::HotBarLeft
                    if num_hotbar_left + num_hotbar_right < 12 =>
                {
                    // Go as far to the left as we can, avoiding
                    // anything else that also wants to be far to the
                    // left.
                    Some(num_hotbar_left)
                }

                SortedInventoryLocation::HotBarRight
                    if num_hotbar_left + num_hotbar_right < 12 =>
                {
                    // Go as far to the right as we can, avoiding
                    // anything else that also wants to be far to the
                    // right.
                    Some(11 - num_hotbar_right)
                }

                SortedInventoryLocation::HotBar
                    if num_hotbar_left + num_hotbar_right < 12 =>
                {
                    // Place in the hotbar if we can.  Otherwwise,
                    // place it in the backpack.
                    hide_from_hotbar.or(empty_hotbar_slot).or(empty_hidden_slot)
                }

                SortedInventoryLocation::HotBarLeft
                | SortedInventoryLocation::HotBarRight
                | SortedInventoryLocation::HotBar => {
                    // The hotbar is full, so pick an open slot
                    // somewhere in the backpack to place this.
                    empty_hidden_slot
                }

                SortedInventoryLocation::Hidden => {
                    show_in_hotbar.or(empty_hidden_slot).or(empty_hotbar_slot)
                }
            }
        } else {
            move_within_hotbar
                .or_else(|| {
                    hide_from_hotbar.filter(|_| empty_hidden_slot.is_some())
                })
                .or_else(|| {
                    iter_slots()
                        .enumerate()
                        .skip(12)
                        .find(|(_, opt_item)| match opt_item.map(func) {
                            Some(
                                SortedInventoryLocation::HotBarLeft
                                | SortedInventoryLocation::HotBarRight,
                            ) => num_hotbar_left + num_hotbar_right < 12,
                            Some(SortedInventoryLocation::HotBar) => {
                                empty_hotbar_slot.is_some()
                                    || hide_from_hotbar.is_some()
                            }

                            Some(SortedInventoryLocation::Hidden) | None => {
                                false
                            }
                        })
                        .map(|(i, _)| i)
                })
        }
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool
    where
        Func: Fn(&Item) -> SortedInventoryLocation,
    {
        self.plan_next_move(game_state).is_none()
    }
}

impl<Func> BotGoal for OrganizeInventoryGoal<Func>
where
    Func: 'static,
    Func: Fn(&Item) -> SortedInventoryLocation,
{
    fn description(&self) -> std::borrow::Cow<str> {
        "Sort inventory".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        do_action: &mut dyn FnMut(GameAction),
    ) -> Result<BotGoalResult, Error> {
        let Some(next_slot) = self.plan_next_move(game_state) else {
            // No further next step is required.  Therefore, Close the
            // pause menu and resume.
            if let Some(menu) = &game_state.pause_menu {
                do_action(GameAction::MouseOverPixel(menu.exit_button));
                do_action(GameAction::LeftClick);
                return Ok(BotGoalResult::InProgress);
            } else {
                return Ok(BotGoalResult::Completed);
            }
        };

        // Open the pause menu if needed
        let Some(menu) = &game_state.pause_menu else {
            do_action(GameAction::ExitMenu);
            return Ok(BotGoalResult::InProgress);
        };

        // Navigate to the inventory page if needed
        let Some(page) = menu.inventory_page() else {
            do_action(GameAction::MouseOverPixel(menu.tab_buttons[0]));
            do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        };

        // Left-click on the correct tile for the next organizational step.
        do_action(GameAction::MouseOverPixel(
            page.player_item_locations[next_slot],
        ));
        do_action(GameAction::LeftClick);
        Ok(BotGoalResult::InProgress)
    }
}

use crate::{
    bot_logic::SelectItemGoal,
    game_state::{FacingDirection, Item, Vector},
    Error, GameAction, GameState,
};

use super::{
    bot_logic::{
        ActionCollector, BotGoal, BotGoalResult, BotInterrupt, LogicStack,
    },
    FaceDirectionGoal,
};

#[derive(Clone)]
pub struct MaintainStaminaGoal {
    /// The target stamina to stay above.
    min_stamina: f32,

    /// The target health to stay above.
    min_health: i32,

    /// The maximum gold that should be spent in per energy restored.
    /// Any item that is outside of this threshold will not be eaten.
    max_gold_per_energy: f32,
}

impl MaintainStaminaGoal {
    pub fn new() -> Self {
        Self {
            min_stamina: 10.0,
            min_health: 50,
            max_gold_per_energy: 2.5,
        }
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        (game_state.player.current_stamina > self.min_stamina
            && game_state.player.current_health > self.min_health)
            || self.item_to_eat(game_state).is_none()
    }

    pub fn item_to_eat<'a>(
        &self,
        game_state: &'a GameState,
    ) -> Option<&'a Item> {
        let iter_items = || {
            game_state
                .player
                .inventory
                .iter_items()
                .filter_map(|item| {
                    item.gp_per_stamina().map(|ratio| (ratio, item))
                })
                .filter(|(gold_per_energy, _)| {
                    *gold_per_energy < self.max_gold_per_energy
                })
        };

        // If one of the eligible items has only a single instance
        // remaining, prioritize eating it in order to free up the
        // slot.
        let has_single_item_stack_of_food =
            iter_items().any(|(_, item)| item.count == 1);

        iter_items()
            .filter(|(_, item)| {
                item.count == 1 || !has_single_item_stack_of_food
            })
            .min_by(|(a, _), (b, _)| a.total_cmp(b))
            .map(|(_, item)| item)
    }
}

impl BotGoal for MaintainStaminaGoal {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Recover health/stamina".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        if self.is_completed(game_state) {
            return Ok(BotGoalResult::Completed);
        }

        if game_state.player.is_eating {
            // Avoid sending additional inputs until the previous
            // consumption has completed.
            return Ok(BotGoalResult::InProgress);
        }

        let Some(item_to_eat) = self.item_to_eat(game_state) else {
            unreachable!("Guarded by self.is_completed() check")
        };

        let select_item = SelectItemGoal::new(item_to_eat.id.clone());
        if !select_item.is_completed(game_state) {
            return Ok(select_item.into());
        }

        let loc = game_state.current_room()?;

        if game_state.dialogue_menu.is_some() {
            // The eat-food confirmation menu is open, so send the 'y'
            // keystroke to confirm.
            actions.do_action(GameAction::ConfirmMenu);
            return Ok(BotGoalResult::InProgress);
        }

        // If the mouse is hovering over the player, or over a tile
        // adjacent to the player, then the 'x' keystroke may activate
        // that tile rather than using the selected item.  Therefore,
        // must find a tile that is adjacent to the player, and which
        // can have the mouse hovering over it without clicking on
        // anything.
        let safe_dir = FacingDirection::iter()
            .find(|dir| {
                let facing_tile = game_state.player.tile() + dir.offset();
                loc.iter_activatable_tiles()
                    .all(|activatable_tile| activatable_tile != facing_tile)
            })
            .expect(
                "TODO: Handle case where all directions \
                 would trigger an action",
            );
        let goal = FaceDirectionGoal::new(safe_dir);
        if !goal.is_completed(game_state) {
            return Ok(goal.into());
        }

        actions.do_action(GameAction::ActivateTile);
        Ok(BotGoalResult::InProgress)
    }
}

impl BotInterrupt for MaintainStaminaGoal {
    fn description(&self) -> std::borrow::Cow<str> {
        "Recover health/stamina".into()
    }

    fn check(
        &mut self,
        game_state: &GameState,
    ) -> Result<Option<LogicStack>, Error> {
        if self.is_completed(game_state) || game_state.any_menu_open() {
            Ok(None)
        } else {
            Ok(Some(LogicStack::new().then(self.clone())))
        }
    }
}

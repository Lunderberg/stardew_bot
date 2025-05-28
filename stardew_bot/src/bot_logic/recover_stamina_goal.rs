use crate::{
    bot_logic::SelectItemGoal, game_state::Item, Error, GameAction, GameState,
};

use super::bot_logic::{BotGoal, BotGoalResult};

pub struct RecoverStaminaGoal {
    /// The stamina of the player when the goal started.  Used to
    /// determine whether stamina has been recovered.
    initial_stamina: Option<f32>,

    /// The maximum gold that should be spent in per energy restored.
    /// Any item that is outside of this threshold will not be eaten.
    max_gold_per_energy: f32,
}

impl RecoverStaminaGoal {
    pub fn new() -> Self {
        Self {
            initial_stamina: None,
            max_gold_per_energy: 2.5,
        }
    }

    pub fn item_to_eat<'a>(
        &self,
        game_state: &'a GameState,
    ) -> Option<&'a Item> {
        game_state
            .player
            .inventory
            .iter_items()
            .filter(|item| item.edibility > 0)
            .map(|item| {
                let stamina = item
                    .stamina_recovery()
                    .expect("Guarded by edibility check");
                let price = item.price as f32;
                (price / stamina, item)
            })
            .filter(|(gold_per_energy, _)| {
                *gold_per_energy < self.max_gold_per_energy
            })
            .min_by(|(a, _), (b, _)| a.total_cmp(b))
            .map(|(_, item)| item)
    }
}

impl BotGoal for RecoverStaminaGoal {
    fn description(&self) -> std::borrow::Cow<str> {
        "Recover stamina".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        do_action: &mut dyn FnMut(GameAction),
    ) -> Result<BotGoalResult, Error> {
        if let Some(initial_stamina) = self.initial_stamina {
            // Check if the stamina has increased.  If so, return
            // control to the parent goal.
            if game_state.player.current_stamina > initial_stamina {
                return Ok(BotGoalResult::Completed);
            }
        } else {
            // This is the first invocation, remember the current stamina
            self.initial_stamina = Some(game_state.player.current_stamina);
        }

        let Some(item_to_eat) = self.item_to_eat(game_state) else {
            return Ok(BotGoalResult::Completed);
        };

        let select_item = SelectItemGoal::new(item_to_eat.clone());
        if !select_item.is_completed(game_state) {
            return Ok(select_item.into());
        }

        if game_state.dialogue_menu.is_none() {
            do_action(GameAction::ActivateTile);
            return Ok(BotGoalResult::InProgress);
        };

        do_action(GameAction::ConfirmMenu);

        Ok(BotGoalResult::InProgress)
    }
}

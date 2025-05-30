use crate::{
    bot_logic::SelectItemGoal,
    game_state::{FacingDirection, Item},
    Error, GameAction, GameState,
};

use super::{
    bot_logic::{BotGoal, BotGoalResult},
    FaceDirectionGoal,
};

pub struct MaintainStaminaGoal {
    /// The target stamina to stay above.
    min_stamina: f32,

    /// The maximum gold that should be spent in per energy restored.
    /// Any item that is outside of this threshold will not be eaten.
    max_gold_per_energy: f32,
}

impl MaintainStaminaGoal {
    pub fn new() -> Self {
        Self {
            min_stamina: 10.0,
            max_gold_per_energy: 2.5,
        }
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        game_state.player.current_stamina > self.min_stamina
            || self.item_to_eat(game_state).is_none()
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

impl BotGoal for MaintainStaminaGoal {
    fn description(&self) -> std::borrow::Cow<str> {
        "Recover stamina".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        do_action: &mut dyn FnMut(GameAction),
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

        let select_item = SelectItemGoal::new(item_to_eat.clone());
        if !select_item.is_completed(game_state) {
            return Ok(select_item.into());
        }

        let player_tile = game_state.player.tile();
        let facing_tile = player_tile + game_state.player.facing.offset();

        let loc = game_state.current_room()?;

        let is_facing_action_tile = loc
            .action_tiles
            .iter()
            .any(|(action_tile, _)| (action_tile == &facing_tile));
        if is_facing_action_tile {
            let safe_facing_direction = FacingDirection::iter()
                .find(|dir| {
                    let facing_tile = player_tile + dir.offset();
                    loc.action_tiles
                        .iter()
                        .all(|(action_tile, _)| (action_tile != &facing_tile))
                })
                .expect("TODO: Handle case where all directions have action");
            let goal = FaceDirectionGoal(safe_facing_direction);
            return Ok(goal.into());
        }

        if game_state.dialogue_menu.is_none() {
            do_action(GameAction::ActivateTile);
            return Ok(BotGoalResult::InProgress);
        };

        do_action(GameAction::ConfirmMenu);

        Ok(BotGoalResult::InProgress)
    }
}

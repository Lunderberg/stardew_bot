use crate::{
    bot_logic::SelectItemGoal,
    game_state::{FacingDirection, Item, Vector},
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

        let loc = game_state.current_room()?;

        if game_state.dialogue_menu.is_some() {
            // The eat-food confirmation menu is open, so send the 'y'
            // keystroke to confirm.
            do_action(GameAction::ConfirmMenu);
            return Ok(BotGoalResult::InProgress);
        }

        let can_apply = game_state
            .inputs
            .mouse_tile_location
            .manhattan_dist(player_tile)
            == 1
            && loc.iter_activatable_tiles().all(|activatable_tile| {
                game_state.inputs.mouse_tile_location != activatable_tile
            });
        if can_apply {
            // The mouse is not currently over a tile that would be
            // activated.  Therefore, send the 'x' keystroke to use
            // the item.
            do_action(GameAction::ActivateTile);
            return Ok(BotGoalResult::InProgress);
        }

        // If the mouse is hovering over the player, or over a tile
        // adjacent to the player, then the 'x' keystroke may activate
        // that tile rather than using the selected item.  Therefore,
        // must find a tile that is adjacent to the player, and which
        // can have the mouse hovering over it without clicking on
        // anything.
        let safe_tile = player_tile
            .iter_adjacent()
            .find(|&hover_tile| {
                loc.iter_activatable_tiles()
                    .all(|activatable_tile| activatable_tile != hover_tile)
            })
            .expect(
                "TODO: Handle case where no nearby tile can be hovered over",
            );

        do_action(GameAction::MouseOverTile(safe_tile));

        Ok(BotGoalResult::InProgress)
    }
}

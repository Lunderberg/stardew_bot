use crate::{Error, GameAction, SelectItemGoal};
use game_state::{FacingDirection, GameState, Item};
use itertools::Itertools as _;

use super::{
    bot_logic::{
        ActionCollector, BotGoal, BotGoalResult, BotInterrupt, LogicStack,
    },
    FaceDirectionGoal, GameStateExt as _,
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

    pub fn is_completed(&self, game_state: &GameState) -> Result<bool, Error> {
        Ok((game_state.player.current_stamina > self.min_stamina
            && game_state.player.current_health > self.min_health)
            || self.item_to_eat(game_state)?.is_none())
    }

    pub fn item_to_eat<'a>(
        &self,
        game_state: &'a GameState,
    ) -> Result<Option<&'a Item>, Error> {
        let iter_items = || -> Result<_, Error> {
            let num_available = game_state
                .iter_accessible_items()?
                .map(|item| (&item.id, item.count))
                .into_grouping_map()
                .sum();
            let num_reserved =
                game_state.iter_reserved_items()?.into_grouping_map().sum();

            let iter = game_state
                .player
                .inventory
                .iter_items()
                .filter(move |item| {
                    let available =
                        num_available.get(&item.id).cloned().unwrap_or(0);
                    let reserved =
                        num_reserved.get(&item.id).cloned().unwrap_or(0);
                    available > reserved
                })
                .filter_map(|item| {
                    item.gp_per_stamina().map(|ratio| (ratio, item))
                })
                .filter(|(gold_per_energy, _)| {
                    *gold_per_energy < self.max_gold_per_energy
                });
            Ok(iter)
        };

        // If one of the eligible items has only a single instance
        // remaining, prioritize eating it in order to free up the
        // slot.
        let has_single_item_stack_of_food =
            iter_items()?.any(|(_, item)| item.count == 1);

        let opt_edible = iter_items()?
            .filter(|(_, item)| {
                item.count == 1 || !has_single_item_stack_of_food
            })
            .min_by(|(a, _), (b, _)| a.total_cmp(b))
            .map(|(_, item)| item);

        Ok(opt_edible)
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
        if self.is_completed(game_state)? {
            return Ok(BotGoalResult::Completed);
        }

        if game_state.player.is_eating {
            // Avoid sending additional inputs until the previous
            // consumption has completed.
            return Ok(BotGoalResult::InProgress);
        }

        let Some(item_to_eat) = self.item_to_eat(game_state)? else {
            unreachable!("Guarded by self.is_completed() check")
        };

        let select_item = SelectItemGoal::new(item_to_eat.id.clone());
        if !select_item.is_completed(game_state) {
            return Ok(select_item.into());
        }

        let loc = game_state.current_room()?;

        if game_state.dialogue_menu().is_some() {
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
        if self.is_completed(game_state)? || game_state.any_menu_open() {
            Ok(None)
        } else {
            Ok(Some(LogicStack::new().then(self.clone())))
        }
    }
}

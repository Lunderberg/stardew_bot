use crate::{
    bot_logic::{ActivateTile, GameStateExt as _},
    game_state::Vector,
    Error, GameAction, GameState,
};

use super::{
    bot_logic::{BotGoal, BotGoalResult},
    InventoryGoal,
};

pub struct CheckAllMail;

impl CheckAllMail {
    pub fn is_completed(&self, game_state: &GameState) -> bool {
        game_state.player.num_unread_mail == 0
    }
}

impl BotGoal for CheckAllMail {
    fn description(&self) -> std::borrow::Cow<str> {
        "Check all mail".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        do_action: &mut dyn FnMut(GameAction),
    ) -> Result<BotGoalResult, Error> {
        let farm_door = game_state.get_farm_door()?;
        let mailbox = farm_door + Vector::new(4, 2);

        if game_state.mail_menu.is_some() {
            do_action(GameAction::ExitMenu);
            return Ok(BotGoalResult::InProgress);
        }

        if self.is_completed(game_state) {
            return Ok(BotGoalResult::Completed);
        }

        if !game_state.player.inventory.has_empty_slot() {
            let goal = InventoryGoal::empty();
            return Ok(goal.into());
        }

        let goal = ActivateTile::new("Farm", mailbox);
        Ok(goal.into())
    }
}

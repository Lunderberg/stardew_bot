use game_state::GameState;
use geometry::Vector;

use crate::{ActivateTile, Error, GameAction, GameStateExt as _};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    InventoryGoal, MenuCloser,
};

pub struct CheckAllMail;

impl CheckAllMail {
    pub fn is_completed(&self, game_state: &GameState) -> bool {
        game_state.player.num_unread_mail == 0
    }
}

impl BotGoal for CheckAllMail {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Check all mail".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let farm_door = game_state.get_farm_door()?;
        let mailbox = farm_door + Vector::new(4, 2);

        let is_near_mailbox = game_state.player.room_name == "Farm"
            && game_state
                .player
                .tile()
                .iter_adjacent()
                .any(|adj| adj == mailbox);

        let cleanup = MenuCloser::new();
        if is_near_mailbox && !cleanup.is_completed(game_state) {
            return Ok(cleanup.into());
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

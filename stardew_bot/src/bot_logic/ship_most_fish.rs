use crate::{game_state::ItemCategory, Error, GameState};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    ShipItemGoal,
};

pub struct ShipMostFishGoal;

impl ShipMostFishGoal {
    pub fn new() -> Self {
        Self
    }
}

impl BotGoal for ShipMostFishGoal {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Ship most fish".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let iter_items = game_state
            .player
            .inventory
            .iter_items()
            .filter(|item| matches!(item.category, Some(ItemCategory::Fish)))
            .filter(|item| {
                let Some(stamina) = item.stamina_recovery() else {
                    return true;
                };
                (item.price as f32) > 1.9 * stamina
            })
            .map(|item| item.clone().with_count(1));

        let goal = ShipItemGoal::new(iter_items);
        if !goal.is_completed(game_state) {
            return Ok(goal.into());
        }

        Ok(BotGoalResult::Completed)
    }
}

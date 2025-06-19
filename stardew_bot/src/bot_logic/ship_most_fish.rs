use crate::{
    game_state::{ItemCategory, Quality},
    Error, GameState,
};

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
                item.gp_per_stamina().map(|gp| gp > 1.75).unwrap_or(false)
            })
            .map(|item| {
                let num_to_keep = match item.quality() {
                    Quality::Normal | Quality::Silver => 0,
                    Quality::Gold | Quality::Iridium => 1,
                };
                item.clone().with_count(num_to_keep)
            });

        let goal = ShipItemGoal::new(iter_items);
        if !goal.is_completed(game_state) {
            return Ok(goal.into());
        }

        Ok(BotGoalResult::Completed)
    }
}

use crate::{Error, GameAction, GameState};

use super::{
    bot_logic::{BotGoal, BotGoalResult, LogicStack},
    CheckAllMail, ExpandTreeFarm, FirstDay, FishingGoal, FishingLocation,
    InventoryGoal, OpportunisticForaging, PlantCropsGoal, WaterCropsGoal,
};

pub struct GenericDay;

impl BotGoal for GenericDay {
    fn description(&self) -> std::borrow::Cow<str> {
        "Generic Day".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _do_action: &mut dyn FnMut(GameAction),
    ) -> Result<BotGoalResult, Error> {
        let current_day = game_state.globals.get_stat("daysPlayed")?;
        if current_day == 1 {
            let goal = LogicStack::new()
                .then(FirstDay)
                .with_interrupt(OpportunisticForaging::new(10.0))
                .cancel_if(|game_state| {
                    game_state.globals.get_stat("daysPlayed").unwrap_or(0) != 1
                });
            return Ok(goal.into());
        }

        Ok(LogicStack::new()
            .then(CheckAllMail)
            .then(FishingGoal::new(FishingLocation::OceanByWilly))
            .then(WaterCropsGoal::new())
            .then(ExpandTreeFarm::new())
            .into())
    }
}

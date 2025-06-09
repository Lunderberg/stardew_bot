use crate::{Error, GameAction, GameState};

use super::{
    bot_logic::{BotGoal, BotGoalResult, LogicStack},
    CheckAllMail, ExpandTreeFarm, FirstDay, InventoryGoal, PlantCropsGoal,
    WaterCropsGoal,
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
            return Ok(FirstDay.into());
        }

        Ok(LogicStack::new()
            .then(CheckAllMail)
            .then(WaterCropsGoal::new())
            .then(ExpandTreeFarm::new())
            .into())
    }
}

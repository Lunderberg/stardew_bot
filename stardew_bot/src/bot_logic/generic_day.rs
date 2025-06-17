use crate::{game_state::ObjectKind, Error, GameAction, GameState};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult, LogicStack},
    CheckAllMail, ClearFarmGoal, CollectNearbyItems, ExpandTreeFarm, FirstDay,
    FishingGoal, FishingLocation, GameStateExt as _, InventoryGoal,
    OpportunisticForaging, PlantCropsGoal, ShipMostFishGoal, WaterCropsGoal,
};

pub struct GenericDay;

impl BotGoal for GenericDay {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Generic Day".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
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

        let has_fishing_rod = game_state
            .iter_accessible_items()?
            .any(|item| item.as_fishing_rod().is_some());

        let fishing_goal = if !has_fishing_rod {
            FishingGoal::new(FishingLocation::OceanByWilly)
        } else if game_state.daily.is_raining {
            FishingGoal::new(FishingLocation::River).stop_time(2400)
        } else {
            FishingGoal::new(FishingLocation::Lake)
        };

        Ok(LogicStack::new()
            .then(CheckAllMail)
            .then(WaterCropsGoal::new())
            .then(ExpandTreeFarm::new())
            .then(fishing_goal)
            .then(ShipMostFishGoal::new())
            .then(ClearFarmGoal::new())
            .with_interrupt(CollectNearbyItems::new())
            .into())
    }
}

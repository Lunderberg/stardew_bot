use crate::{
    game_state::{Item, ObjectKind},
    Error, GameAction, GameState,
};

use super::{
    bot_logic::{
        ActionCollector, BotGoal, BotGoalResult, BotInterrupt as _, LogicStack,
    },
    BuyFromMerchantGoal, CheckAllMail, ClearFarmGoal, CollectNearbyItems,
    ExpandStorageInterrupt, ExpandTreeFarm, FirstDay, FishingGoal,
    FishingLocation, ForagingGoal, GameStateExt as _, GeodeCrackingGoal,
    HarvestCropsGoal, InventoryGoal, MineDelvingGoal, OpportunisticForaging,
    PlantCropsGoal, ShipMostFishGoal, WaterCropsGoal,
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

        let current_day = game_state
            .globals
            .stats
            .get("daysPlayed")
            .cloned()
            .unwrap_or(0);

        let stack = LogicStack::new()
            .then(CheckAllMail)
            .then(HarvestCropsGoal::new())
            .then(WaterCropsGoal::new());

        let stack = if !has_fishing_rod {
            stack
                .then(ExpandTreeFarm::new())
                .then(FishingGoal::new(FishingLocation::OceanByWilly))
        } else if game_state.daily.is_raining {
            stack
                .then(ExpandTreeFarm::new())
                .then(FishingGoal::new(FishingLocation::River).stop_time(2400))
        } else if current_day == 4 {
            stack
                .then(ClearFarmGoal::new().stop_time(2000))
                .then(ExpandTreeFarm::new())
        } else if current_day >= 5
            && game_state.globals.lowest_mine_level_reached < 40
        {
            stack
                .then(ShipMostFishGoal::new())
                .then(MineDelvingGoal::new())
        } else if current_day >= 6 {
            stack
                .then(
                    PlantCropsGoal::new([Item::KALE_SEEDS.with_count(200)])
                        .buy_missing_seeds(false)
                        .stop_time(1100),
                )
                .then(
                    GeodeCrackingGoal::new()
                        .sell_gems(true)
                        .sell_minerals(true),
                )
                .then(
                    PlantCropsGoal::new([Item::KALE_SEEDS.with_count(200)])
                        .buy_missing_seeds(true),
                )
        } else {
            stack
                .then(ExpandTreeFarm::new())
                .then(FishingGoal::new(FishingLocation::Lake))
        };

        Ok(stack
            .then(ClearFarmGoal::new())
            .with_interrupt(OpportunisticForaging::new(5.0))
            .with_interrupt(CollectNearbyItems::new())
            .with_interrupt(ExpandStorageInterrupt::new())
            .into())
    }
}

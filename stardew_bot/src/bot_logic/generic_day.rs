use crate::{
    game_state::{Item, ObjectKind, Vector},
    Error, GameAction, GameState,
};

use super::{
    bot_logic::{
        ActionCollector, BotGoal, BotGoalResult, BotInterrupt as _, LogicStack,
    },
    BuyFromMerchantGoal, CheckAllMail, ClearFarmGoal, CollectNearbyItems,
    ExpandStorageInterrupt, ExpandTreeFarm, FirstDay, FishingGoal,
    FishingLocation, ForagingGoal, GameStateExt as _, GeodeCrackingGoal,
    HarvestCropsGoal, InventoryGoal, KeyEventInterrupt, MineDelvingGoal,
    MovementGoal, OpportunisticForaging, PlantCropsGoal, SellToMerchantGoal,
    ShipMostFishGoal, WaterCropsGoal,
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
            let crops = PlantCropsGoal::new([Item::KALE_SEEDS.with_count(200)]);
            stack
                .then(
                    crops
                        .clone()
                        .buy_missing_seeds(false)
                        .stop_time(1100)
                        .opportunistic_clay_farming(5),
                )
                .then(
                    InventoryGoal::empty()
                        .with(Item::HOE)
                        .with(Item::GEODE.with_count(1000))
                        .with(Item::FROZEN_GEODE.with_count(1000))
                        .with(Item::MAGMA_GEODE.with_count(1000))
                        .with(Item::OMNI_GEODE.with_count(1000))
                        .with(Item::CLAY.with_count(1000))
                        .stamina_recovery_slots(1),
                )
                .then(SellToMerchantGoal::new("Carpenter", Item::CLAY))
                .then(
                    BuyFromMerchantGoal::new(
                        "Carpenter",
                        Item::WOOD.with_count(350),
                    )
                    .include_stored_items("Farm"),
                )
                .then(
                    GeodeCrackingGoal::new()
                        .sell_gems(true)
                        .sell_minerals(true)
                        .sell_iridium_ore(true),
                )
                .then(crops.clone().only_buy_missing_seeds())
                .then(BuyFromMerchantGoal::new(
                    "Saloon",
                    Item::SALAD.with_count(10),
                ))
                .then(crops.clone())
                .then(
                    InventoryGoal::empty()
                        .with(Item::HOE)
                        .stamina_recovery_slots(2),
                )
                .then(ForagingGoal::new().location("Beach"))
                .then(ForagingGoal::new().location("Custom_Garden"))
                .then(ForagingGoal::new().location("Custom_ShearwaterBridge"))
                .then(ForagingGoal::new().location("Town"))
                .then(ForagingGoal::new().location("Mountain"))
                .then(ForagingGoal::new().location("Forest"))
                .then(ForagingGoal::new().location("Custom_ForestWest"))
        } else {
            stack
                .then(ExpandTreeFarm::new())
                .then(FishingGoal::new(FishingLocation::Lake))
        };

        Ok(stack
            .then(ClearFarmGoal::new())
            .with_interrupt(KeyEventInterrupt::new())
            .with_interrupt(OpportunisticForaging::new(5.0))
            .with_interrupt(CollectNearbyItems::new())
            .with_interrupt(ExpandStorageInterrupt::new())
            .into())
    }
}

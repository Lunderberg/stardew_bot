use crate::{Error, GiveGiftGoal};
use game_state::{GameState, ItemCategory, ItemId, Quality};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult, LogicStack},
    BuyFromMerchantGoal, CheckAllMail, ClearFarmGoal, CollectNearbyItems,
    ExpandStorageInterrupt, ExpandTreeFarm, FirstDay, FishingGoal,
    FishingLocation, ForagingGoal, GameStateExt as _, GeodeCrackingGoal,
    HarvestCropsGoal, InventoryGoal, KeyEventInterrupt, LocationExt as _,
    MineDelvingGoal, OpportunisticForaging, PlantCropsGoal, SellToMerchantGoal,
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
                .with_interrupt(
                    OpportunisticForaging::new(10.0).min_empty_slots(1),
                )
                .cancel_if(|game_state| {
                    game_state.globals.get_stat("daysPlayed").unwrap_or(0) != 1
                });
            return Ok(goal.into());
        }

        let farm = game_state.get_room("Farm")?;

        let has_fishing_rod = game_state
            .iter_accessible_items()?
            .any(|item| item.as_fishing_rod().is_some());

        let current_day = game_state
            .globals
            .stats
            .get("daysPlayed")
            .cloned()
            .unwrap_or(0);

        let any_kale_planted = farm
            .iter_planted_seeds()
            .any(|seed| seed == &ItemId::KALE_SEEDS);

        let stack = LogicStack::new().then(CheckAllMail);

        let stack = if current_day == 2 || !has_fishing_rod {
            stack
                .then(HarvestCropsGoal::new())
                .then(WaterCropsGoal::new())
                .then(ExpandTreeFarm::new())
                .then(FishingGoal::new(FishingLocation::OceanByWilly))
        } else if game_state.daily.is_raining {
            // TODO: Conditionally apply HarvestCropsGoal, based on
            // whether delaying the harvest will prevent a harvest at
            // the end of the season.
            stack
                .then(FishingGoal::new(FishingLocation::River).stop_time(2400))
                .then(HarvestCropsGoal::new())
        } else if current_day == 4 {
            stack
                .then(HarvestCropsGoal::new())
                .then(WaterCropsGoal::new())
                .then(ClearFarmGoal::new().stop_time(2000))
                .then(ExpandTreeFarm::new())
        } else if current_day == 5
            || (current_day > 5
                && game_state.globals.lowest_mine_level_reached < 40)
        {
            stack
                .then(HarvestCropsGoal::new())
                .then(WaterCropsGoal::new())
                .then(ShipMostFishGoal::new())
                .then(MineDelvingGoal::new())
        } else if current_day >= 6 && any_kale_planted {
            let crops =
                PlantCropsGoal::new([ItemId::KALE_SEEDS.with_count(200)]);
            stack
                .then(HarvestCropsGoal::new())
                .then(crops)
                .then(ExpandTreeFarm::new())
                .then(MineDelvingGoal::new())
        } else if current_day >= 6 {
            let crops =
                PlantCropsGoal::new([ItemId::KALE_SEEDS.with_count(200)]);
            stack
                .then(HarvestCropsGoal::new())
                .then(
                    crops
                        .clone()
                        .buy_missing_seeds(false)
                        .craft_missing(false)
                        .stop_time(1100)
                        .opportunistic_clay_farming(6),
                )
                .then(
                    InventoryGoal::empty()
                        .with(ItemId::HOE)
                        .take_if(|item| {
                            matches!(
                                item.category,
                                Some(ItemCategory::Gem | ItemCategory::Mineral)
                            )
                        })
                        .with(ItemId::GEODE.with_count(1000))
                        .with(ItemId::FROZEN_GEODE.with_count(1000))
                        .with(ItemId::MAGMA_GEODE.with_count(1000))
                        .with(ItemId::OMNI_GEODE.with_count(1000))
                        .with(ItemId::CLAY.with_count(1000))
                        .with_exactly(
                            ItemId::PARSNIP.with_quality(Quality::Gold),
                        )
                        .stamina_recovery_slots(1),
                )
                .then(
                    SellToMerchantGoal::new("Carpenter", ItemId::CLAY)
                        .min_to_sell(10),
                )
                .then(BuyFromMerchantGoal::new(
                    "Carpenter",
                    ItemId::WOOD.with_count(300),
                ))
                .then(GiveGiftGoal::new(
                    "Pam",
                    ItemId::PARSNIP.with_quality(Quality::Gold),
                ))
                .then(
                    GeodeCrackingGoal::new()
                        .sell_gems(true)
                        .sell_minerals(true)
                        .sell_iridium_ore(true),
                )
                .then(crops.clone().only_buy_missing_seeds())
                .then(BuyFromMerchantGoal::new(
                    "Saloon",
                    ItemId::SALAD.with_count(20),
                ))
                .then(ForagingGoal::new().location("Beach"))
                .then(crops.clone())
        } else {
            stack
                .then(HarvestCropsGoal::new())
                .then(WaterCropsGoal::new())
                .then(ExpandTreeFarm::new())
                .then(FishingGoal::new(FishingLocation::Lake))
        };

        let stack = if current_day % 7 == 6 {
            stack
                .then(
                    InventoryGoal::empty()
                        .with(ItemId::HOE)
                        .stamina_recovery_slots(2),
                )
                .then(ForagingGoal::new())
        } else {
            stack
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

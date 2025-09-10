use std::collections::HashMap;

use env_var_flag::env_var_flag;
use geometry::Vector;
use itertools::Itertools as _;

use crate::{bot_logic::BotInterrupt, Error, GameAction, GiveGiftGoal};
use game_state::{GameState, ItemCategory, ItemId, ObjectKind, Season};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult, LogicStack},
    BuyFromMerchantGoal, ClayFarmingGoal, ClearFarmGoal, CollectNearbyItems,
    ExpandStorageInterrupt, ForagingGoal, GameStateExt as _, GoToActionTile,
    MaintainStaminaGoal, MenuCloser, MovementGoal, PlantCropsGoal,
    SellToMerchantGoal, ShipItemGoal, UseItemOnTile,
};

pub struct FirstDay;

struct ScytheNearby;

impl BotGoal for FirstDay {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "First Day".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let current_day = game_state.globals.get_stat("daysPlayed")?;
        if current_day != 1 {
            return Ok(BotGoalResult::Completed);
        }

        let foraging =
            ForagingGoal::new().stop_at_time(700).stop_with_stamina(30);
        if !foraging.is_completed(game_state) {
            return Ok(foraging
                .with_interrupt(CollectNearbyItems::new())
                .into());
        }

        let farm = game_state.get_room("Farm")?;
        let goal = ClearFarmGoal::new()
            .use_priority_tiles(false)
            .use_stamina(false)
            .target_tile(
                farm.warps
                    .iter()
                    .find(|warp| warp.target_room == "BusStop")
                    .map(|warp| warp.location)
                    .map_or_else(|| game_state.get_farm_door(), Ok)?,
            )
            .relative_weights((4, 1))
            .stop_time(620);

        if !goal.is_completed(game_state)? {
            return Ok(goal.into());
        }

        let in_game_time = game_state.globals.in_game_time;
        let current_money = game_state.player.current_money;

        // If we are playing StardewValleyExpanded, pick up the
        // freebie ForestSword.  The `class PlacedItem:
        // StardewValley.Object` is added by mods, and is not
        // currently unpacked as I want to keep the bot compatible
        // with vanilla Stardew, so it shows up as an
        // `ObjectKind::Unknown`.
        let should_pick_up_forest_sword = env_var_flag("GRAB_FOREST_SWORD");
        let can_pick_up_forest_sword = game_state
            .locations
            .iter()
            .find(|loc| loc.name == "Custom_ForestWest")
            .into_iter()
            .flat_map(|loc| loc.objects.iter())
            .find(|obj| {
                matches!(obj.kind, ObjectKind::Unknown)
                    && obj.tile == Vector::new(60, 148)
            })
            .is_some();
        if should_pick_up_forest_sword && can_pick_up_forest_sword {
            let tile = Vector::<isize>::new(60, 148);
            let movement =
                MovementGoal::new("Custom_ForestWest", tile.map(|x| x as f32))
                    .with_tolerance(1.1);
            if !movement.is_completed(game_state) {
                return Ok(movement.into());
            }

            actions.do_action(GameAction::MouseOverTile(tile));
            if tile == game_state.inputs.mouse_tile_location {
                actions.do_action(GameAction::RightClick);
            }

            return Ok(BotGoalResult::InProgress);
        }

        let goal = MaintainStaminaGoal::new();
        if !goal.is_completed(game_state)? {
            return Ok(goal.into());
        }

        let clay_farming = ClayFarmingGoal::new()
            .stop_at_time(1130)
            .stop_at_stamina(4.0)
            .hard_stop();
        if !clay_farming.is_completed(game_state) {
            let stack = LogicStack::new()
                .then(clay_farming)
                .with_interrupt(ScytheNearby);
            return Ok(stack.into());
        }

        if !game_state
            .globals
            .queued_events
            .contains("copperFound%&NL&%")
        {
            let stack = LogicStack::new()
                .then(BuyFromMerchantGoal::new(
                    "Blacksmith",
                    ItemId::COPPER_ORE,
                ))
                .with_interrupt(ScytheNearby);
            return Ok(stack.into());
        }

        let gift_pam = GiveGiftGoal::new("Pam", ItemId::DAFFODIL);
        if !gift_pam.is_completed(game_state)? {
            return Ok(gift_pam.into());
        }

        let goal =
            SellToMerchantGoal::new("Carpenter", [ItemId::CLAY, ItemId::FIBER])
                .min_to_sell(100);
        if in_game_time < 1630 && !goal.is_completed(game_state) {
            let stack =
                LogicStack::new().then(goal).with_interrupt(ScytheNearby);
            return Ok(stack.into());
        }

        let goal =
            BuyFromMerchantGoal::new("Saloon", ItemId::SALAD.with_count(7));
        if goal.item_count(game_state)? == 0 && in_game_time < 1900 {
            return Ok(goal.into());
        }

        let clay_farming = ClayFarmingGoal::new().stop_at_time(1450);
        if !clay_farming.is_completed(game_state) {
            return Ok(clay_farming.into());
        }

        if game_state.player.inventory.items.len() < 24 && current_money > 2000
        {
            if let Some(menu) = game_state.dialogue_menu() {
                let Some(pixel) = menu.response_pixel("Purchase") else {
                    return Ok(MenuCloser::new().into());
                };

                actions.do_action(GameAction::MouseOverPixel(pixel));
                actions.do_action(GameAction::LeftClick);
                return Ok(BotGoalResult::InProgress);
            } else {
                let goal = GoToActionTile::new("BuyBackpack");
                return Ok(goal.into());
            }
        }
        if let Some(menu) = game_state.dialogue_menu() {
            if menu.responses.is_empty() {
                actions.do_action(GameAction::ExitMenu);
                return Ok(BotGoalResult::InProgress);
            }
        }

        // Includes buying seeds as necessary.  Which, given that
        // today is the first day, is quite necessary.
        let iter_seeds = {
            let mut seeds: HashMap<ItemId, usize> = HashMap::new();

            game_state
                .player
                .inventory
                .iter_items()
                .filter(|item| {
                    matches!(item.category, Some(ItemCategory::Seed))
                        && !item.id.is_tree_seed()
                })
                .for_each(|item| {
                    if let Some(count) = seeds.get_mut(&item.id) {
                        *count += item.count;
                    } else {
                        seeds.insert(item.id.clone(), item.count);
                    }
                });

            let crop_to_seed: HashMap<_, _> = game_state
                .statics
                .crop_data
                .iter()
                .filter(|(_, crop_data)| {
                    crop_data
                        .seasons
                        .iter()
                        .any(|season| matches!(season, Season::Spring))
                })
                .map(|(seed, crop_data)| (&crop_data.harvest_item, seed))
                .collect();

            let iter_bundle_seeds = game_state
                .iter_bundle_items()?
                .filter(|(name, _, _)| {
                    // While we could also buy the seeds for Quality
                    // Crops, the seeds for anything other than
                    // parsnips would be too expensive on Day1.  The
                    // seeds for Quality Crops should be
                    // purchased/planted later in spring, when money
                    // isn't as tight.
                    name == &"Spring Crops"
                })
                .filter_map(|(_, id, _)| -> Option<(&ItemId, usize)> {
                    // Buying two seeds of each type gives a spare in
                    // case the crows eat one.  Until Day6, we can't
                    // craft scarecrows to protect the crops.
                    crop_to_seed.get(id).map(|seed| (*seed, 2))
                });

            std::iter::empty::<(&ItemId, usize)>()
                .chain([(&ItemId::PARSNIP_SEEDS, 60)])
                .chain(iter_bundle_seeds)
                .for_each(|(seed, count)| {
                    if let Some(to_plant) = seeds.get_mut(seed) {
                        *to_plant = (*to_plant).max(count);
                    } else {
                        seeds.insert(seed.clone(), count);
                    }
                });

            seeds
                .into_iter()
                .filter(|(id, _)| id != &ItemId::MIXED_SEEDS)
                .map(|(id, count)| id.clone().with_count(count))
                .sorted_by_key(|item| item.count)
        };
        let mut plant_crops =
            PlantCropsGoal::new(iter_seeds).opportunistic_clay_farming(10);
        if !plant_crops.is_completed(game_state)? {
            return Ok(plant_crops.into());
        }

        let should_ship_clay = |game_state: &GameState| -> bool {
            let current_clay =
                game_state.player.inventory.count_item(&ItemId::CLAY);
            current_clay > 20 && game_state.globals.in_game_time > 2400
        };

        if should_ship_clay(game_state) {
            let goal = ShipItemGoal::new([ItemId::CLAY.with_count(0)]);
            if !goal.is_completed(game_state) {
                return Ok(goal.into());
            }
        }

        let goal = ClearFarmGoal::new().clear_stone(false);
        if !goal.is_completed(game_state)? {
            return Ok(goal
                .cancel_if(should_ship_clay)
                .with_interrupt(CollectNearbyItems::new())
                .with_interrupt(ExpandStorageInterrupt::new())
                .into());
        }

        Ok(BotGoalResult::InProgress)
    }
}

impl BotInterrupt for ScytheNearby {
    fn description(&self) -> std::borrow::Cow<str> {
        "Scythe nearby".into()
    }

    fn check(
        &mut self,
        game_state: &GameState,
    ) -> Result<Option<LogicStack>, Error> {
        let player_tile = game_state.player.tile();

        if !game_state.player.inventory.contains(ItemId::SCYTHE) {
            return Ok(None);
        }

        let Some(tile) = game_state
            .current_room()?
            .objects
            .iter()
            .filter(|obj| {
                matches!(obj.kind, ObjectKind::Fiber | ObjectKind::Grass)
            })
            .map(|obj| obj.tile)
            .find(|&tile| player_tile.manhattan_dist(tile) == 1)
        else {
            return Ok(None);
        };

        let current_room = game_state.player.room_name.clone();
        let stack = LogicStack::new()
            .then(UseItemOnTile::new(
                ItemId::SCYTHE,
                game_state.player.room_name.clone(),
                tile,
            ))
            .cancel_if(move |game_state| {
                game_state.player.room_name != current_room
                    || game_state.player.tile().manhattan_dist(tile) != 1
            });

        Ok(Some(stack))
    }
}

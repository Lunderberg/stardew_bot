use std::collections::{HashMap, HashSet};

use dotnet_debugger::env_var_flag;
use geometry::{Direction, Vector};
use itertools::Itertools as _;

use crate::{bot_logic::BotInterrupt, Error, GameAction};
use game_state::{GameState, Item, ItemCategory, ItemId, ObjectKind};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult, LogicStack},
    graph_search::GraphSearch as _,
    BuyFromMerchantGoal, ClayFarmingGoal, ClearFarmGoal, CollectNearbyItems,
    CraftItemGoal, ExpandStorageInterrupt, ForagingGoal, GameStateExt as _,
    GoToActionTile, LocationExt as _, MaintainStaminaGoal, MenuCloser,
    MovementGoal, PlantCropsGoal, SellToMerchantGoal, ShipItemGoal,
    UseItemOnTile,
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
            .relative_weights((3, 1))
            .stop_time(640);

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
        if !goal.is_completed(game_state) {
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
            if let Some(menu) = &game_state.dialogue_menu {
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
        if let Some(menu) = &game_state.dialogue_menu {
            if menu.responses.is_empty() {
                actions.do_action(GameAction::ExitMenu);
                return Ok(BotGoalResult::InProgress);
            }
        }

        // Includes buying seeds as necessary.  Which, given that
        // today is the first day, is quite necessary.
        let mut plant_crops = PlantCropsGoal::new(
            std::iter::once(ItemId::PARSNIP_SEEDS.with_count(60)).chain(
                game_state
                    .player
                    .inventory
                    .iter_items()
                    .filter(|item| {
                        item.id != ItemId::PARSNIP_SEEDS
                            && item.id != ItemId::MIXED_SEEDS
                    })
                    .filter(|item| {
                        matches!(item.category, Some(ItemCategory::Seed))
                    })
                    .filter(|item| !item.id.is_tree_seed())
                    .cloned(),
            ),
        )
        .opportunistic_clay_farming(10);
        if in_game_time < 1700 && !plant_crops.is_completed(game_state)? {
            return Ok(plant_crops.into());
        }

        let should_ship_clay = |game_state: &GameState| -> bool {
            let current_clay =
                game_state.player.inventory.count_item(&ItemId::CLAY);
            current_clay > 20 && game_state.globals.in_game_time > 2400
        };

        if should_ship_clay(game_state) {
            let goal = ShipItemGoal::new([
                ItemId::CLAY.with_count(0),
                ItemId::DAFFODIL.with_count(0),
            ]);
            if !goal.is_completed(game_state) {
                return Ok(goal.into());
            }
        }

        let goal = ClearFarmGoal::new().clear_stone(false);
        if !goal.is_completed(game_state)? {
            return Ok(goal
                .cancel_if(move |game_state| should_ship_clay(game_state))
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

        Ok(game_state
            .current_room()?
            .objects
            .iter()
            .find(|obj| {
                player_tile.manhattan_dist(obj.tile) == 1
                    && matches!(obj.kind, ObjectKind::Fiber | ObjectKind::Grass)
            })
            .map(|obj| {
                UseItemOnTile::new(
                    ItemId::SCYTHE,
                    game_state.player.room_name.clone(),
                    obj.tile,
                )
                .into()
            }))
    }
}

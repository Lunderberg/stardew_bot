use std::collections::{HashMap, HashSet};

use dotnet_debugger::env_var_flag;
use itertools::Itertools as _;

use crate::{
    game_state::{Item, ObjectKind, Vector},
    Direction, Error, GameAction, GameState,
};

use super::{
    bot_logic::{BotGoal, BotGoalResult},
    graph_search::GraphSearch as _,
    BuyFromMerchantGoal, ClayFarmingGoal, ClearFarmGoal, ForagingGoal,
    GameStateExt as _, GoToActionTile, MaintainStaminaGoal, MovementGoal,
    PlantCropsGoal, SellToMerchantGoal, UseItemOnTile,
};

pub struct FirstDay;

fn scythe_path_to_water(
    game_state: &GameState,
) -> Result<Option<BotGoalResult>, Error> {
    let farm = game_state.get_room("Farm")?;
    let farm_door = game_state.get_farm_door()?;

    let water_border: Vec<Vector<isize>> = (0..farm.shape.right)
        .cartesian_product(0..farm.shape.down)
        .map(|(i, j)| Vector::new(i as isize, j as isize))
        .filter(|&tile| !farm.is_water(tile))
        .filter(|&tile| {
            Direction::iter()
                .filter(|dir| dir.is_cardinal())
                .any(|dir| farm.is_water(tile + dir.offset()))
        })
        .collect();

    let pathfinding = farm
        .pathfinding()
        .allow_diagonal(false)
        .fiber_clearing_cost(0);

    let can_scythe: HashSet<_> = farm
        .objects
        .iter()
        .filter(|obj| matches!(obj.kind, ObjectKind::Grass | ObjectKind::Fiber))
        .map(|obj| obj.tile)
        .collect();

    let tile_to_scythe = pathfinding
        .path_between(farm_door, water_border.as_slice())?
        .into_iter()
        .find(|tile| can_scythe.contains(tile));

    let opt_action = tile_to_scythe
        .map(|tile| UseItemOnTile::new(Item::SCYTHE.clone(), "Farm", tile));

    Ok(opt_action.map(Into::into))
}

impl BotGoal for FirstDay {
    fn description(&self) -> std::borrow::Cow<str> {
        "First Day".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        do_action: &mut dyn FnMut(GameAction),
    ) -> Result<BotGoalResult, Error> {
        let current_day = game_state.globals.get_stat("daysPlayed")?;
        if current_day != 1 {
            return Ok(BotGoalResult::Completed);
        }

        if let Some(tile_to_scythe) = scythe_path_to_water(game_state)? {
            return Ok(tile_to_scythe.into());
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

            do_action(GameAction::MouseOverTile(tile));
            if tile == game_state.inputs.mouse_tile_location {
                do_action(GameAction::RightClick);
            }

            return Ok(BotGoalResult::InProgress);
        }

        let foraging =
            ForagingGoal::new().stop_at_time(700).stop_with_stamina(30);
        if !foraging.is_completed(game_state) {
            return Ok(foraging.into());
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
            return Ok(clay_farming.into());
        }

        let goal = SellToMerchantGoal::new("Carpenter", Item::CLAY);
        if goal.item_count(game_state) > 100 && in_game_time < 1630 {
            return Ok(goal.into());
        }

        let goal =
            BuyFromMerchantGoal::new("Saloon", Item::SALAD.with_count(10));
        if goal.item_count(game_state) == 0 && in_game_time < 1900 {
            return Ok(goal.into());
        }

        let clay_farming = ClayFarmingGoal::new().stop_at_time(1500);
        if !clay_farming.is_completed(game_state) {
            return Ok(clay_farming.into());
        }

        if game_state.player.inventory.items.len() < 24 && current_money > 2000
        {
            if let Some(menu) = &game_state.dialogue_menu {
                if let Some(pixel) = menu.responses.get(0) {
                    do_action(GameAction::MouseOverPixel(*pixel));
                    do_action(GameAction::LeftClick);
                }
                return Ok(BotGoalResult::InProgress);
            } else {
                let goal = GoToActionTile::new("BuyBackpack");
                return Ok(goal.into());
            }
        }
        if let Some(menu) = &game_state.dialogue_menu {
            if menu.responses.is_empty() {
                do_action(GameAction::ExitMenu);
                return Ok(BotGoalResult::InProgress);
            }
        }

        if game_state.player.inventory.count_item(&Item::PARSNIP_SEEDS) == 0
            && in_game_time < 1700
            && current_money > 1200
        {
            let goal = BuyFromMerchantGoal::new(
                "Buy General",
                Item::PARSNIP_SEEDS.with_count(60),
            );
            return Ok(goal.into());
        }

        let plant_crops = PlantCropsGoal::new();
        if !plant_crops.is_completed(game_state) {
            return Ok(plant_crops.into());
        }

        let goal = ClearFarmGoal::new();
        if !goal.is_completed(game_state)? {
            return Ok(goal.into());
        }

        Ok(BotGoalResult::InProgress)
    }
}

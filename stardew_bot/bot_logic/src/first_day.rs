use std::collections::{HashMap, HashSet};

use env_var_flag::env_var_flag;
use geometry::Vector;
use itertools::Itertools as _;

use crate::{
    ActivateTile, ClearTreeGoal, Error, GameAction, GiveGiftGoal, GoToRoomGoal,
    LocationExt as _, PickUpFurnitureGoal, bot_logic::BotInterrupt,
    predictors::FarmCoalRockPredictor,
};
use game_state::{
    CropPhase, GameState, ItemCategory, ItemId, ObjectKind, Season, TreeKind,
};

use super::{
    BuyFromMerchantGoal, ClayFarmingGoal, ClearFarmGoal, CollectNearbyItems,
    ExpandStorageInterrupt, GameStateExt as _, GoToActionTile,
    MaintainStaminaGoal, MenuCloser, MovementGoal, PlantCropsGoal,
    SellToMerchantGoal, ShipItemGoal, UseItemOnTile,
    bot_logic::{ActionCollector, BotGoal, BotGoalResult, LogicStack},
};

pub struct FirstDay;

struct ScytheNearby;

struct CollectCoal {
    goal: usize,
    tile: Option<Vector<isize>>,
}

impl BotGoal for FirstDay {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "First Day".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let player = &game_state.player;
        let current_day = game_state.globals.get_stat("daysPlayed")?;
        let in_game_time = game_state.globals.in_game_time;
        if current_day != 1 {
            return Ok(BotGoalResult::Completed);
        }

        let farm = game_state.get_room("Farm")?;
        let farmhouse = game_state.get_room("FarmHouse")?;
        if in_game_time < 630
            && let Some(chair_loc) = farmhouse
                .furniture
                .iter()
                .find(|furniture| furniture.is_chair())
                .map(|furniture| furniture.shape.top_left)
        {
            let goal = PickUpFurnitureGoal::new("FarmHouse", chair_loc);
            if !goal.is_completed(game_state)? {
                return Ok(goal.into());
            }
        }

        let mut goal = CollectCoal::new(5);
        if !goal.is_completed(game_state)? {
            return Ok(goal.into());
        }

        // TODO: Include a maximum travel distance for the foraging.
        //
        // let foraging =
        //     ForagingGoal::new().stop_at_time(700).stop_with_stamina(30);
        // if !foraging.is_completed(game_state) {
        //     return Ok(foraging
        //         .with_interrupt(CollectNearbyItems::new())
        //         .into());
        // }

        if let Some(spring_onion) = game_state
            .get_room("Forest")?
            .objects
            .iter()
            .find_map(|obj| {
                obj.kind
                    .as_hoe_dirt()
                    .and_then(|hoe_dirt| hoe_dirt.crop.as_ref())
                    .map(|crop| crop.seed == ItemId::SPRING_ONION)
                    .unwrap_or(false)
                    .then(|| obj.tile)
            })
        {
            let room_movement =
                GoToRoomGoal::new("Forest").clear_breakables(true);
            if !room_movement.is_completed(game_state) {
                return Ok(room_movement.into());
            }

            return Ok(ActivateTile::new("Forest", spring_onion).into());
        }

        let current_money = player.current_money;

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
            .stop_at_time(1430)
            .stop_at_stamina(4.0)
            .hard_stop();
        if !clay_farming.is_completed(game_state)? {
            let stack = LogicStack::new()
                .then(clay_farming)
                .with_interrupt(ScytheNearby);
            return Ok(stack.into());
        }

        let need_early_battery = game_state
            .statics
            .bundles
            .iter()
            .filter_map(|bundle| {
                let flags =
                    game_state.globals.bundles.get(&bundle.bundle_index)?;
                Some((bundle, flags))
            })
            .any(|(bundle, flags)| {
                bundle
                    .iter_items()
                    .zip(flags)
                    .any(|(item, done)| item.id == ItemId::BATTERY && !done)
            });
        if need_early_battery {
            let gift_pam = GiveGiftGoal::new("Pam", ItemId::DAFFODIL);
            if !gift_pam.is_completed(game_state)? {
                return Ok(gift_pam.into());
            }
        }

        let clay_farming = ClayFarmingGoal::new().stop_at_time(1610);
        if player.room_name == "Beach"
            && (player.current_stamina > 4.0 || in_game_time > 1330)
            && !clay_farming.is_completed(game_state)?
        {
            return Ok(clay_farming.into());
        }

        let have_chair = player
            .inventory
            .iter_items()
            .any(|item| item.as_chair().is_none());
        if !have_chair {
            let goal = BuyFromMerchantGoal::new("Carpenter", ItemId::OAK_CHAIR);
            if !goal.is_completed(game_state)? {
                return Ok(goal.into());
            }
        }

        // Sell enough clay to afford the Day1 purchases.  When
        // passing out, we lose 10% of all cash that we're holding.
        // Since we're planning to pass out at the end of pretty much
        // every day, it's better to get the money overnight to
        // minimize that penalty.
        //
        // TODO: Compute the exact amount needed for seeds, rather
        // than the hardcoded 1300 gp.
        let required_cash = 1300
            + 2000 * (player.inventory.items.len() < 24) as i32
            + 220
                * 10i32
                    .saturating_sub(
                        player.inventory.count_item(ItemId::SALAD) as i32
                    )
                    .max(0)
            + 75 * 45i32
                .saturating_sub(
                    player.inventory.count_item(ItemId::COPPER_ORE) as i32
                )
                .max(0);
        if current_money < required_cash && in_game_time < 1630 {
            let to_sell =
                ((required_cash - current_money) as usize).div_ceil(20);
            let to_keep = player
                .inventory
                .count_item(ItemId::CLAY)
                .checked_sub(to_sell)
                .unwrap_or(0);

            let goal = SellToMerchantGoal::new("Carpenter")
                .sell_down_to([
                    ItemId::CLAY.with_count(to_keep),
                    ItemId::FIBER.with_count(0),
                ])
                .min_to_sell(50);

            if !goal.is_completed(game_state) {
                return Ok(goal.into());
            }
        }

        let goal = BuyFromMerchantGoal::new(
            "HospitalShop",
            ItemId::ENERGY_TONIC.with_count(1),
        );
        if player.current_stamina < 10.0
            && in_game_time < 1030
            && !goal.is_completed(game_state)?
        {
            return Ok(goal
                .cancel_if(|game_state| {
                    game_state.player.current_stamina >= 10.0
                })
                .into());
        }

        // Buy copper ore with which to start a Day2 pickaxe upgrade.
        // We can't start it any earlier because (1) we don't have the
        // Day1 money and (2) Clint hasn't given us the furnace
        // recipe.  Technically, we don't need to buy all the ore on
        // Day1, since we'll have more funds available on Day2, but we
        // need at least one copper ore on Day1 so that Clint will
        // give us the recipe on Day2.  (Finding copper ore sets
        // `game_state.globals.queued_events.contains("copperFound%&NL&%")`,
        // which trigger's Clint's visit the next morning.)
        //
        // So long as we can route in the full purchase on Day1, we
        // only need to visit Clint once on Day2.
        let goal = BuyFromMerchantGoal::new(
            "Blacksmith",
            ItemId::COPPER_ORE.with_count(45),
        );
        if in_game_time < 1530
            && current_money >= 45 * 75
            && !goal.is_completed(game_state)?
        {
            return Ok(goal.into());
        }

        // Handles both purchases from the Saloon.  First purchase
        // occurs after 2nd round of clay-farming (around noon), and
        // is limited by available funds.  Second purchase occurs
        // after 3rd round of clay-farming (around 5 PM), and buys as
        // much as we can at that point.
        let goal =
            BuyFromMerchantGoal::new("Saloon", ItemId::SALAD.with_count(10));
        if goal.item_count(game_state)? < 5 && in_game_time < 1900 {
            return Ok(goal.into());
        }

        if !clay_farming.is_completed(game_state)? {
            return Ok(clay_farming.into());
        }

        if player.inventory.items.len() < 24 && current_money > 2000 {
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
        if let Some(menu) = game_state.dialogue_menu()
            && menu.responses.is_empty()
        {
            return Ok(MenuCloser::new().into());
        }

        // Includes buying seeds as necessary.  Which, given that
        // today is the first day, is quite necessary.
        let iter_seeds = {
            let mut seeds: HashMap<ItemId, usize> = HashMap::new();

            let iter_stored_items = [farm, farmhouse]
                .into_iter()
                .flat_map(|loc| {
                    loc.objects.iter().filter_map(|obj| obj.kind.as_chest())
                })
                .flat_map(|chest| {
                    chest.iter_filled_slots().map(|(_, item)| item)
                });

            let iter_held_items = player.inventory.iter_items();

            let iter_already_planted = farm.objects.iter().filter_map(|obj| {
                let crop = obj.kind.as_hoe_dirt()?.crop.as_ref()?;
                matches!(crop.phase, CropPhase::Seed).then(|| &crop.seed)
            });

            iter_stored_items
                .chain(iter_held_items)
                .filter(|item| {
                    matches!(item.category, Some(ItemCategory::Seed))
                        && !item.id.is_tree_seed()
                })
                .map(|item| (&item.id, item.count))
                .chain(iter_already_planted.map(|id| (id, 1)))
                .filter(|(id, _)| *id != &ItemId::MIXED_SEEDS)
                .for_each(|(id, count)| {
                    if let Some(total) = seeds.get_mut(id) {
                        *total += count;
                    } else {
                        seeds.insert(id.clone(), count);
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

            let mut remaining_gp = player.current_money;
            std::iter::empty::<(&ItemId, usize)>()
                .chain(iter_bundle_seeds)
                .chain([(&ItemId::PARSNIP_SEEDS, 60)])
                .for_each(|(seed, count)| {
                    // TODO: Filter on purchasable items.  This will
                    // handle carrot seeds, which can appear in the
                    // spring remixed bundle, but cannot be purchased.
                    let previously_planned =
                        seeds.get(seed).cloned().unwrap_or(0);
                    let additional =
                        count.max(previously_planned) - previously_planned;
                    let unit_price = seed.buy_price(game_state);
                    let num_to_buy =
                        additional.min((remaining_gp / unit_price) as usize);
                    remaining_gp -= (num_to_buy as i32) * unit_price;

                    if num_to_buy > 0 {
                        if let Some(to_plant) = seeds.get_mut(seed) {
                            *to_plant += num_to_buy;
                        } else {
                            seeds.insert(seed.clone(), num_to_buy);
                        }
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

        if player.skills.foraging_level() < 1 {
            // Cut down trees to ensure we reach Foraging Level 1 at
            // the end of the first day.
            let trees: HashSet<_> = farm
                .objects
                .iter()
                .filter(|obj| match &obj.kind {
                    ObjectKind::Tree(tree) => {
                        matches!(tree.kind, TreeKind::Maple | TreeKind::Pine)
                            && tree.health > 0.0
                            && tree.growth_stage >= 5
                    }
                    _ => false,
                })
                .map(|obj| obj.tile)
                .collect();
            let search_from = if player.room_name == "Farm" {
                player.tile()
            } else {
                game_state.get_farm_door()?
            };
            let opt_tree = farm
                .pathfinding(&game_state.statics)
                .include_border(true)
                .allow_chair_jump()
                .iter_dijkstra_forward(search_from)
                .map(|(tile, _)| tile)
                .find(|tile| trees.contains(tile));
            if let Some(tree) = opt_tree {
                return Ok(ClearTreeGoal::new(tree).into());
            }
        }

        let farm_clay_farming = ClayFarmingGoal::new()
            .room("Farm")
            .distance_for_clear_tile(3)
            .stop_at_time(2430)
            .hard_stop();
        if !farm_clay_farming.is_completed(game_state)? {
            return Ok(farm_clay_farming.into());
        }

        let should_ship_clay = |game_state: &GameState| -> bool {
            let current_clay =
                game_state.player.inventory.count_item(&ItemId::CLAY);
            current_clay > 20 && game_state.globals.in_game_time > 2400
        };

        if should_ship_clay(game_state) {
            let goal = ShipItemGoal::new(
                [ItemId::CLAY, ItemId::DAFFODIL, ItemId::CONCH]
                    .into_iter()
                    .map(|id| id.with_count(0)),
            );
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
    fn description(&self) -> std::borrow::Cow<'_, str> {
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
            .find(|&tile| player_tile.chessboard_dist(tile) == 1)
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
                    || game_state.player.tile().chessboard_dist(tile) != 1
            });

        Ok(Some(stack))
    }
}

impl CollectCoal {
    pub fn new(goal: usize) -> Self {
        Self { goal, tile: None }
    }

    pub fn is_completed(
        &mut self,
        game_state: &GameState,
    ) -> Result<bool, Error> {
        let has_enough =
            game_state.player.inventory.count_item(ItemId::COAL) >= self.goal;
        let next_pickaxe = self.next_pickaxe(game_state)?;
        let next_pickup = self.next_pickup(game_state)?;

        Ok(has_enough || (next_pickaxe.is_none() && next_pickup.is_none()))
    }

    fn find_coal_rock(
        game_state: &GameState,
    ) -> Result<Option<Vector<isize>>, Error> {
        let player = &game_state.player;
        let farm = game_state.get_room("Farm")?;
        let predictor = FarmCoalRockPredictor::new(game_state);

        let coal_rocks: HashSet<Vector<isize>> = farm
            .objects
            .iter()
            .filter(|obj| matches!(obj.kind, game_state::ObjectKind::Stone(_)))
            .map(|obj| obj.tile)
            .filter(|&tile| predictor.will_produce_coal(tile))
            .collect();

        let search_from = if player.room_name == "Farm" {
            player.tile()
        } else {
            game_state.get_farm_door()?
        };
        let opt_tile = farm
            .pathfinding(&game_state.statics)
            .include_border(true)
            .allow_chair_jump()
            .iter_dijkstra_forward(search_from)
            .map(|(tile, _)| tile)
            .find(|tile| coal_rocks.contains(tile));
        Ok(opt_tile)
    }

    fn get_coal_rock(
        &mut self,
        game_state: &GameState,
    ) -> Result<Option<Vector<isize>>, Error> {
        if self.tile.is_none() {
            self.tile = Self::find_coal_rock(game_state)?;
        }
        Ok(self.tile)
    }

    fn next_pickaxe(
        &mut self,
        game_state: &GameState,
    ) -> Result<Option<Vector<isize>>, Error> {
        let Some(tile) = self.get_coal_rock(game_state)? else {
            return Ok(None);
        };

        let still_exists =
            game_state.get_room("Farm")?.objects.iter().any(|obj| {
                obj.tile == tile && matches!(obj.kind, ObjectKind::Stone(_))
            });

        Ok(still_exists.then(|| tile))
    }

    fn next_pickup(
        &mut self,
        game_state: &GameState,
    ) -> Result<Option<Vector<f32>>, Error> {
        let farm = game_state.get_room("Farm")?;

        let find_near =
            |target_pos: Vector<f32>, radius: f32| -> Option<Vector<f32>> {
                let radius2 = radius * radius;
                farm.items
                    .iter()
                    .filter(|floating_item| floating_item.id == ItemId::COAL)
                    .map(|floating_item| floating_item.position / 64.0)
                    .find(|item_pos| item_pos.dist2(target_pos) < radius2)
            };

        if let Some(near_player) =
            find_near(game_state.player.center_pos(), 5.0)
        {
            return Ok(Some(near_player));
        }

        let Some(tile) = self.get_coal_rock(game_state)? else {
            return Ok(None);
        };

        let opt_coal_pos = find_near(tile.map(|x| x as f32), 2.0);

        Ok(opt_coal_pos)
    }
}

impl BotGoal for CollectCoal {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Gather coal".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        if let Some(pos) = self.next_pickup(game_state)? {
            let goal = MovementGoal::new("Farm", pos).with_tolerance(1.4);
            if goal.is_completed(game_state) {
                return Ok(BotGoalResult::InProgress.into());
            }

            let num_items = game_state.get_room("Farm")?.items.len();
            return Ok(goal
                .cancel_if(move |game_state| {
                    game_state
                        .get_room("Farm")
                        .map(|loc| loc.items.len())
                        .unwrap_or(0)
                        != num_items
                })
                .into());
        }

        if let Some(tile) = self.next_pickaxe(game_state)? {
            let goal = UseItemOnTile::new(ItemId::PICKAXE, "Farm", tile);
            return Ok(goal.into());
        }

        Ok(BotGoalResult::Completed.into())
    }
}

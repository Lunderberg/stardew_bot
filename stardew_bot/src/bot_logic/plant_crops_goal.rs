use std::collections::{HashMap, HashSet};

use itertools::{Either, Itertools as _};

use crate::{
    bot_logic::{
        graph_search::GraphSearch as _, CraftItemGoal, MaintainStaminaGoal,
        MovementGoal, ObjectKindExt as _, UseItemOnTile,
    },
    game_state::{
        HoeDirt, Item, ItemCategory, ItemId, ObjectKind, Sprinkler,
        StaticState, Vector,
    },
    Error, GameAction, GameState,
};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult, LogicStack},
    ActivateTile, BuyFromMerchantGoal, ClayPredictor, FarmPlan,
    FillWateringCan, GameStateExt as _, InventoryGoal, LocationExt as _,
    OrganizeInventoryGoal,
};

#[derive(Clone)]
pub struct PlantCropsGoal {
    /// The seeds that should be planted
    seeds: Vec<Item>,

    /// The plan on where to place seeds, sprinklers, and scarecrows.
    plan: Option<CropPlantingPlan>,

    /// The time at which this goal should stop early, if not already
    /// completed.
    stop_time: i32,

    /// If `Some(dist)`, the maximum distance in tiles that the bot
    /// may travel to dig up clay while preparing the field.  If
    /// `None` (default), do not prioritize digging up clay.
    opportunistic_clay_farming: Option<u64>,

    /// If true, and if there are insufficient seeds in the player's
    /// inventory and storage chests, buy extra seeds from Pierre.  If
    /// false, plant the seeds that are available and do not buy extra
    /// seeds.
    buy_missing_seeds: bool,

    /// If `stop_after_buying_seeds` is true, buy any seeds from
    /// Pierre but do not plant them.  This is mainly used to buy
    /// seeds when near Pierre, to be planted later.
    stop_after_buying_seeds: bool,

    /// A read may occur after the player's inventory has been updated
    /// but before the game location has been updated.  This will
    /// appear as if the player has insufficient seeds/materials, and
    /// may cause the bot to head to Pierre or the storage chests to
    /// buy/craft additional materials.  To avoid this, this field
    /// tracks the game tick on which the decision to buy/craft was
    /// made.  The decision is only applied if made again on the
    /// following frame.
    tick_decided_to_gather_items: Option<i32>,
}

#[derive(Clone)]
struct CropPlantingPlan {
    /// After planting, what should be located at each tile.
    final_state: HashMap<Vector<isize>, ItemId>,
}

impl PlantCropsGoal {
    pub fn new<Iter, T>(seeds: Iter) -> Self
    where
        Iter: IntoIterator<Item = T>,
        T: Into<Item>,
    {
        let seeds = seeds.into_iter().map(Into::into).collect();
        Self {
            seeds,
            plan: None,
            stop_time: 2600,
            opportunistic_clay_farming: None,
            buy_missing_seeds: true,
            stop_after_buying_seeds: false,
            tick_decided_to_gather_items: None,
        }
    }

    pub fn opportunistic_clay_farming(self, dist: u64) -> Self {
        Self {
            opportunistic_clay_farming: Some(dist),
            ..self
        }
    }

    pub fn buy_missing_seeds(self, buy_missing_seeds: bool) -> Self {
        Self {
            buy_missing_seeds,
            ..self
        }
    }

    pub fn only_buy_missing_seeds(self) -> Self {
        Self {
            buy_missing_seeds: true,
            stop_after_buying_seeds: true,
            ..self
        }
    }

    pub fn stop_time(self, stop_time: i32) -> Self {
        Self { stop_time, ..self }
    }

    /// Returns the next step required to prepare the tile.
    ///
    /// - None: The tile needs no additional processing
    /// - Some(None): The tile should be activated
    /// - Some(Some(item)): An item should be used on the tile
    fn next_step_of_tile(
        statics: &StaticState,
        opt_current: Option<&ObjectKind>,
        goal: &ItemId,

        // Exclude showing the watering step.  When using this
        // function to determine if additional items must be
        // bought/crafted, a tile with unwatered seeds should not
        // cause the bot to think the tile requires seeds.
        assume_watered: bool,
    ) -> Option<Option<Item>> {
        let is_seed = statics
            .item_data
            .get(goal)
            .map(|data| matches!(data.category, ItemCategory::Seed))
            .unwrap_or(false);

        if let Some(current) = opt_current {
            match current {
                ObjectKind::Sprinkler(sprinkler) if goal == &sprinkler.id() => {
                    None
                }
                ObjectKind::Scarecrow if goal == &Item::SCARECROW.id => None,

                ObjectKind::HoeDirt(HoeDirt {
                    is_watered: false, ..
                }) if is_seed && !assume_watered => {
                    Some(Some(Item::WATERING_CAN))
                }

                ObjectKind::HoeDirt(HoeDirt { crop: None, .. }) => {
                    Some(Some(goal.clone().into()))
                }

                ObjectKind::HoeDirt(HoeDirt {
                    crop: Some(crop),
                    is_watered,
                }) if &crop.seed == goal && (*is_watered || assume_watered) => {
                    None
                }

                ObjectKind::Other { .. } if current.is_forage() => Some(None),

                ObjectKind::Stone(_)
                | ObjectKind::Mineral(_)
                | ObjectKind::Wood
                | ObjectKind::Fiber
                | ObjectKind::Grass
                | ObjectKind::PotOfGold
                | ObjectKind::Tree(_)
                | ObjectKind::FruitTree(_)
                | ObjectKind::ArtifactSpot
                | ObjectKind::SeedSpot
                | ObjectKind::Torch
                | ObjectKind::HoeDirt(_)
                | ObjectKind::Sprinkler(_)
                | ObjectKind::Scarecrow => Some(current.get_tool()),

                ObjectKind::Chest(_)
                | ObjectKind::MineLadderUp
                | ObjectKind::MineLadderDown
                | ObjectKind::MineHoleDown
                | ObjectKind::MineElevator
                | ObjectKind::MineCartCoal
                | ObjectKind::MineBarrel
                | ObjectKind::Furnace(_)
                | ObjectKind::Other { .. }
                | ObjectKind::Unknown => {
                    panic!("Planned farming tile should not contain {current}.")
                }
            }
        } else {
            let item = if is_seed {
                Item::HOE
            } else {
                goal.clone().into()
            };
            Some(Some(item))
        }
    }

    /// Iterator through the available planting steps that may
    /// currently be applied.
    fn iter_steps<'a>(
        &'a self,
        game_state: &'a GameState,
    ) -> Result<impl Iterator<Item = (Vector<isize>, Option<Item>)> + 'a, Error>
    {
        if game_state.globals.in_game_time >= self.stop_time {
            return Ok(Either::Left(std::iter::empty()));
        }

        let farm = game_state.get_room("Farm")?;

        let plan = self.plan.as_ref().expect("Populated by self.fill_plan()");

        let current_state = farm.generate_tile_lookup();
        let inventory =
            InventoryGoal::current().total_stored_and_carried(game_state)?;

        let iter_steps = plan
            .final_state
            .iter()
            .filter_map(move |(tile, goal)| {
                let opt_current = current_state.get(tile).map(|&kind| kind);
                let opt_next_step = Self::next_step_of_tile(
                    &game_state.statics,
                    opt_current,
                    goal,
                    false,
                );
                let next_step = opt_next_step?;
                Some((*tile, next_step))
            })
            .filter(move |(_, opt_item)| {
                opt_item
                    .as_ref()
                    .map(|item| inventory.get(&item.id).is_some())
                    .unwrap_or(true)
            });

        Ok(Either::Right(iter_steps))
    }

    /// Check if the planting has been completed.
    pub fn is_completed(
        &mut self,
        game_state: &GameState,
    ) -> Result<bool, Error> {
        self.fill_plan(game_state)?;
        Ok(self.iter_steps(game_state)?.next().is_none())
    }

    /// Populate the planned state of farming tiles, after all
    /// planting has been completed.
    pub fn fill_plan(&mut self, game_state: &GameState) -> Result<(), Error> {
        if self.plan.is_some() {
            return Ok(());
        }
        let plan = FarmPlan::plan(game_state)?;

        let farm = game_state.get_room("Farm")?;

        let sprinkler_locations: HashSet<_> = farm
            .objects
            .iter()
            .filter(|obj| matches!(obj.kind, ObjectKind::Sprinkler(_)))
            .map(|obj| obj.tile)
            .collect();

        let current_sprinklers: Vec<_> = plan
            .iter_regular_sprinklers()
            .filter(|tile| sprinkler_locations.contains(tile))
            .collect();

        let iter_seeds = self.seeds.iter().flat_map(|to_plant| {
            std::iter::repeat_n(to_plant.id.clone(), to_plant.count)
        });

        let iter_seed_tiles = if game_state.globals.days_played() == 1 {
            Either::Left(plan.iter_initial_plot())
        } else {
            Either::Right(plan.iter_sprinkler_plot())
        }
        .zip(iter_seeds);

        let iter_sprinklers = current_sprinklers
            .into_iter()
            .chain(plan.iter_regular_sprinklers())
            .map(|tile| (tile, ItemId::SPRINKLER));

        let iter_scarecrows = plan
            .iter_scarecrows()
            .map(|tile| (tile, Item::SCARECROW.id.clone()));

        let final_state = iter_seed_tiles
            .chain(iter_sprinklers)
            .chain(iter_scarecrows)
            .collect();
        self.plan = Some(CropPlantingPlan { final_state });

        Ok(())
    }

    /// Count the number of each item is required to complete the
    /// planned planting of the farm.
    ///
    /// For each `(item,count)` pair, the player's inventory and
    /// storage should have at least `count` of `item` in order to
    /// complete the planned planting.
    fn iter_required_items(
        &self,
        game_state: &GameState,
    ) -> Result<impl Iterator<Item = (&ItemId, usize)> + '_, Error> {
        let farm = game_state.get_room("Farm")?;
        let plan = self.plan.as_ref().expect("Populated by self.fill_plan()");
        let current_state = farm.generate_tile_lookup();

        let iter_items_required = plan
            .final_state
            .iter()
            .filter(|(tile, goal_item)| {
                let opt_current = current_state.get(tile).map(|&kind| kind);
                let opt_next_step = Self::next_step_of_tile(
                    &game_state.statics,
                    opt_current,
                    goal_item,
                    true,
                );
                opt_next_step.is_some()
            })
            .map(|(_, id)| id)
            .counts()
            .into_iter();

        Ok(iter_items_required)
    }

    /// Iterate through items that are planned, but are not available.
    fn iter_missing_items<'a>(
        &'a self,
        game_state: &'a GameState,
    ) -> Result<impl Iterator<Item = Item> + 'a, Error> {
        let items_available =
            InventoryGoal::current().total_stored_and_carried(game_state)?;
        let iter = self
            .iter_required_items(game_state)?
            .map(move |(id, required)| {
                let available = items_available.get(id).cloned().unwrap_or(0);
                (id, required.saturating_sub(available))
            })
            .filter(|(_, missing)| *missing > 0)
            .map(|(id, count)| {
                game_state
                    .statics
                    .enrich_item(id.clone().into())
                    .with_count(count)
            });

        Ok(iter)
    }

    /// Attempt to buy enough seeds to meet the target number to farm.
    fn try_buy_missing_seeds(
        &self,
        game_state: &GameState,
    ) -> Result<Option<LogicStack>, Error> {
        if !self.buy_missing_seeds {
            return Ok(None);
        }

        let iter_purchase = self
            .iter_required_items(game_state)?
            .map(|(id, count)| {
                let item: Item = id.clone().into();
                let item = item.with_count(count);
                let item = game_state.statics.enrich_item(item);
                item
            })
            .filter(|item| matches!(item.category, Some(ItemCategory::Seed)))
            .map(|item| {
                BuyFromMerchantGoal::new("Buy General", item)
                    .include_stored_items("Farm")
            });

        let mut buy_items = LogicStack::new();
        for goal in iter_purchase {
            if !goal.is_completed(game_state)? {
                buy_items = buy_items.then(goal);
            }
        }

        if buy_items.len() > 0 {
            Ok(Some(buy_items))
        } else {
            Ok(None)
        }
    }

    /// Attempt to craft any items that are missing
    fn try_craft_missing_items(
        &self,
        game_state: &GameState,
    ) -> Result<Option<LogicStack>, Error> {
        let get_ingredients = |to_craft: &ItemId| -> Option<Vec<Item>> {
            if to_craft == &ItemId::SPRINKLER {
                Some(vec![Item::COPPER_BAR, Item::IRON_BAR])
            } else if to_craft == &Item::SCARECROW.id {
                Some(vec![
                    Item::WOOD.with_count(50),
                    Item::COAL,
                    Item::FIBER.with_count(20),
                ])
            } else {
                None
            }
        };

        let available =
            InventoryGoal::current().total_stored_and_carried(game_state)?;

        let items_to_craft: Vec<Item> = self
            .iter_missing_items(game_state)?
            .filter_map(|missing| {
                let ingredients = get_ingredients(&missing.id)?;
                let num_craftable = ingredients
                    .iter()
                    .map(|ingredient| {
                        let num_available =
                            available.get(&ingredient.id).cloned().unwrap_or(0);
                        num_available / ingredient.count
                    })
                    .min()
                    .filter(|&num| num > 0)?;
                let num_to_craft = num_craftable.min(missing.count);
                Some(missing.with_count(num_to_craft))
            })
            .collect();

        if items_to_craft.is_empty() {
            return Ok(None);
        }

        let prepare = items_to_craft
            .iter()
            .flat_map(|to_craft| {
                let ingredients = get_ingredients(&to_craft.id)
                    .expect("Craftables have known recipe");
                ingredients.into_iter().map(|ingredient| {
                    (ingredient.id, ingredient.count * to_craft.count)
                })
            })
            .into_grouping_map()
            .sum()
            .into_iter()
            .fold(InventoryGoal::current(), |goal, (id, count)| {
                let item: Item = id.into();
                let item = item.with_count(count);
                goal.with(item)
            });

        let stack = items_to_craft
            .into_iter()
            .map(CraftItemGoal::new)
            .fold(LogicStack::new().then(prepare), |stack, goal| {
                stack.then(goal)
            });

        Ok(Some(stack))
    }
}

impl BotGoal for PlantCropsGoal {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Plant crops".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        if game_state.globals.in_game_time > self.stop_time {
            return Ok(BotGoalResult::Completed);
        }

        self.fill_plan(game_state)?;

        if self.stop_after_buying_seeds {
            // Early handling in case we want to buy the seeds while
            // we're out, but plant them later.
            if let Some(buy_seeds) = self.try_buy_missing_seeds(game_state)? {
                return Ok(buy_seeds.into());
            } else {
                return Ok(BotGoalResult::Completed);
            }
        }

        let opt_gather = 'opt_gather: {
            let opt = self.try_buy_missing_seeds(game_state)?;
            if opt.is_some() {
                break 'opt_gather opt;
            }
            let opt = self.try_craft_missing_items(game_state)?;
            if opt.is_some() {
                break 'opt_gather opt;
            }

            None
        };

        if let Some(gather_items) = opt_gather {
            // In some cases, the update to the player's inventory can
            // occur before the update to the game location.  In that
            // case, the total number of seeds may appear to be less
            // than the required number of seeds.  To avoid running
            // off to the general store, wait until the choice to buy
            // seeds has been made for a few game ticks in a row.
            let tick = game_state.globals.game_tick;
            if let Some(prev_tick) = self.tick_decided_to_gather_items {
                if tick < prev_tick + 2 {
                    return Ok(BotGoalResult::InProgress);
                }
            } else {
                self.tick_decided_to_gather_items = Some(tick);
                return Ok(BotGoalResult::InProgress);
            }

            return Ok(gather_items.into());
        } else {
            self.tick_decided_to_gather_items = None;
        }

        let initial_tile = if game_state.player.room_name == "Farm" {
            game_state.player.tile()
        } else {
            game_state.closest_entrance("Farm")?
        };
        let farm = game_state.get_room("Farm")?;
        let distances = farm
            .pathfinding()
            .include_border(true)
            .distances(initial_tile);

        let iter_tiles_to_hoe = || -> Result<_, Error> {
            let iter = self
                .iter_steps(game_state)?
                .filter(|(_, opt_item)| {
                    opt_item
                        .as_ref()
                        .map(|item| item.is_same_item(&Item::HOE))
                        .unwrap_or(false)
                })
                .map(|(tile, _)| tile)
                .sorted_by_key(|tile| (tile.right, tile.down));
            Ok(iter)
        };

        let clay_tiles: HashSet<Vector<isize>> = 'clay_tiles: {
            let Some(max_clay_dist) = self.opportunistic_clay_farming else {
                // Not doing opporunistic clay farming.
                break 'clay_tiles Default::default();
            };

            let is_within_clay_range = |tile: &Vector<isize>| {
                distances
                    .get_opt(*tile)
                    .map(|&dist| dist <= max_clay_dist)
                    .unwrap_or(false)
            };

            // Returns an iterator of tiles that will be hoed in
            // preparation for planting, and are within the allowed
            // distance for opportunistic clay farming.
            let iter_potential_clay_tiles = || {
                iter_tiles_to_hoe()
                    .map(|iter| iter.filter(is_within_clay_range))
            };

            let clay_predictor = ClayPredictor::new(game_state);
            let clay_tiles: HashSet<Vector<isize>> =
                iter_potential_clay_tiles()?
                    .filter(|tile| clay_predictor.will_produce_clay(*tile))
                    .collect();

            if !clay_tiles.is_empty() {
                // There's at least one tile that would produce clay.
                break 'clay_tiles clay_tiles;
            }

            // We are deliberately restricting the tiles that may be
            // inspected for clay, to avoid spending time running
            // around the farm area.  Therefore, use an empty HashSet
            // for `clay_tiles`, allowing any nearby tile to be hoed.
            if iter_tiles_to_hoe()?.any(|tile| !is_within_clay_range(&tile)) {
                break 'clay_tiles Default::default();
            }

            // There are no tiles that will produce clay in the next
            // use of the hoe.  The hoe should be used on whichever
            // tile would take the longest before producing clay.
            // This maximizes the clay production for the last few
            // tiles, since it preserves tiles that are soon to
            // produce clay, even if they are adjacent to the player.
            let num_remaining = iter_tiles_to_hoe()?.count();
            let uses_until_clay = |tile: Vector<isize>| {
                clay_predictor
                    .iter_will_produce_clay(tile)
                    .take(num_remaining + 1)
                    .take_while(|will_be_clay| !will_be_clay)
                    .count()
            };
            let longest_until_clay = iter_potential_clay_tiles()?
                .map(|tile| uses_until_clay(tile))
                .max();

            iter_potential_clay_tiles()?
                .filter(|tile| {
                    Some(uses_until_clay(*tile)) == longest_until_clay
                })
                .collect()
        };

        let opt_next_step = self
            .iter_steps(game_state)?
            .filter(|(tile, opt_item)| {
                opt_item
                    .as_ref()
                    .map(|item| {
                        !item.is_same_item(&Item::HOE)
                            || clay_tiles.is_empty()
                            || clay_tiles.contains(tile)
                    })
                    .unwrap_or(true)
            })
            .filter(|(tile, _)| distances.is_some(*tile))
            .min_by_key(|(tile, opt_item)| {
                let need_clay_before_deadline =
                    self.opportunistic_clay_farming.is_some()
                        && self.stop_time < 2600;
                let dist = distances[*tile].unwrap();
                (
                    need_clay_before_deadline
                        && opt_item
                            .as_ref()
                            .map(|item| item.is_same_item(&Item::WATERING_CAN))
                            .unwrap_or(true),
                    dist != 1,
                    dist,
                )
            });

        let Some((tile, opt_item)) = opt_next_step else {
            return Ok(BotGoalResult::Completed);
        };

        let get_tools = self
            .iter_required_items(game_state)?
            .fold(InventoryGoal::current(), |goal, (id, count)| {
                let item: Item = id.clone().into();
                let item = item.with_count(count);
                goal.with(item)
            })
            .with(Item::WATERING_CAN)
            .with(Item::HOE)
            .with(Item::PICKAXE)
            .with(Item::AXE)
            .stamina_recovery_slots(1);
        if !get_tools.is_completed(game_state)? {
            let get_tools = get_tools
                .stamina_recovery_slots(3)
                .keep_if(|item| {
                    matches!(item.category, Some(ItemCategory::Seed))
                })
                .otherwise_empty();
            return Ok(get_tools.into());
        }

        let goal = MaintainStaminaGoal::new();
        if !goal.is_completed(game_state) {
            return Ok(goal.into());
        }
        if game_state.player.current_stamina < 2.0 {
            return Ok(BotGoalResult::Completed);
        }

        let sort_inventory = OrganizeInventoryGoal::new(|item| {
            use crate::bot_logic::SortedInventoryLocation as SIL;
            if item.id.item_id.starts_with("(T)")
                || item.id.item_id.starts_with("(W)")
                || (matches!(item.category, Some(ItemCategory::Seed))
                    && !item.id.is_tree_seed()
                    && !item.id.is_fruit_sapling())
                || item.is_same_item(&ItemId::SPRINKLER)
                || item.is_same_item(&Item::SCARECROW)
            {
                SIL::HotBarLeft
            } else if item.gp_per_stamina().is_some() {
                SIL::HotBarRight
            } else if item.is_same_item(&Item::CLAY)
                || item.is_same_item(&Item::WOOD)
            {
                SIL::HotBar
            } else {
                SIL::Hidden
            }
        });
        if !sort_inventory.is_completed(game_state) {
            return Ok(sort_inventory.into());
        }

        if let Some(item) = opt_item {
            if item.is_same_item(&Item::WATERING_CAN) {
                let goal = FillWateringCan::if_empty();
                if !goal.is_completed(game_state) {
                    return Ok(goal.into());
                }
            }

            let action = UseItemOnTile::new(item.clone(), "Farm", tile);
            Ok(action.into())
        } else {
            let action = ActivateTile::new("Farm", tile);
            Ok(action.into())
        }
    }
}

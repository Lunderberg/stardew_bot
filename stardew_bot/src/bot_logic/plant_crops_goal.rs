use std::collections::{HashMap, HashSet};

use itertools::{Either, Itertools as _};

use crate::{
    bot_logic::{
        graph_search::GraphSearch as _, MaintainStaminaGoal, MovementGoal,
        ObjectKindExt as _, UseItemOnTile,
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
};

#[derive(Clone)]
pub struct PlantCropsGoal {
    seeds: Vec<Item>,
    plan: Option<CropPlantingPlan>,
    stop_time: i32,
    opportunistic_clay_farming: bool,
    buy_missing_seeds: bool,
    stop_after_buying_seeds: bool,
    tick_decided_to_buy_seeds: Option<i32>,
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
            opportunistic_clay_farming: false,
            buy_missing_seeds: true,
            stop_after_buying_seeds: false,
            tick_decided_to_buy_seeds: None,
        }
    }

    pub fn opportunistic_clay_farming(
        self,
        opportunistic_clay_farming: bool,
    ) -> Self {
        Self {
            opportunistic_clay_farming,
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
    ) -> Option<Option<Item>> {
        if let Some(current) = opt_current {
            match current {
                ObjectKind::Sprinkler(sprinkler) if goal == &sprinkler.id() => {
                    None
                }
                ObjectKind::Scarecrow if goal == &Item::SCARECROW.id => None,

                ObjectKind::HoeDirt(HoeDirt {
                    is_watered: false, ..
                }) => Some(Some(Item::WATERING_CAN)),

                ObjectKind::HoeDirt(HoeDirt { crop: None, .. }) => {
                    Some(Some(goal.clone().into()))
                }

                ObjectKind::HoeDirt(HoeDirt {
                    crop: Some(crop),
                    is_watered: true,
                }) if &crop.seed == goal => None,

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
            let is_seed = statics
                .item_data
                .get(goal)
                .map(|data| matches!(data.category, ItemCategory::Seed))
                .unwrap_or(false);
            let item = if is_seed {
                Item::HOE
            } else {
                goal.clone().into()
            };
            Some(Some(item))
        }
    }

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
        let inventory = game_state.player.inventory.to_hash_map();

        let iter_steps = plan
            .final_state
            .iter()
            .filter_map(move |(tile, goal)| {
                let opt_current = current_state.get(tile).map(|&kind| kind);
                let opt_next_step = Self::next_step_of_tile(
                    &game_state.statics,
                    opt_current,
                    goal,
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

    pub fn is_completed(
        &mut self,
        game_state: &GameState,
    ) -> Result<bool, Error> {
        self.fill_plan(game_state)?;
        Ok(self.iter_steps(game_state)?.next().is_none())
    }

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

        self.plan = Some(CropPlantingPlan {
            final_state: iter_seed_tiles.chain(iter_sprinklers).collect(),
        });

        Ok(())
    }

    fn try_buy_missing_seeds(
        &self,
        game_state: &GameState,
    ) -> Result<Option<LogicStack>, Error> {
        if !self.buy_missing_seeds {
            return Ok(None);
        }

        let farm = game_state.get_room("Farm")?;

        let mut missing: HashMap<_, _> = self
            .seeds
            .iter()
            .map(|item| (&item.id, item.count))
            .collect();

        let iter_item = InventoryGoal::current()
            .iter_stored_and_carried(game_state)?
            .map(|item| (&item.id, item.count));

        let iter_planted = farm.iter_planted_seeds().map(|seed| (seed, 1));

        iter_item
            .chain(iter_planted)
            .for_each(|(item_id, item_count)| {
                if let Some(count) = missing.get_mut(item_id) {
                    *count = count.saturating_sub(item_count);
                }
            });

        let inventory = game_state.player.inventory.to_hash_map();

        let buy_items = self
            .seeds
            .iter()
            .filter_map(|item| {
                let num_missing = missing
                    .get(&item.id)
                    .cloned()
                    .filter(|&count| count > 0)?;
                let current = inventory.get(&item.id).cloned().unwrap_or(0);
                let goal = BuyFromMerchantGoal::new(
                    "Buy General",
                    item.clone().with_count(current + num_missing),
                );
                Some(goal)
            })
            .fold(LogicStack::new(), |stack, goal| stack.then(goal));

        if buy_items.len() > 0 {
            Ok(Some(buy_items))
        } else {
            Ok(None)
        }
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

        if self.stop_after_buying_seeds {
            // Early handling in case we want to buy the seeds while
            // we're out, but plant them later.
            if let Some(buy_seeds) = self.try_buy_missing_seeds(game_state)? {
                return Ok(buy_seeds.into());
            } else {
                return Ok(BotGoalResult::Completed);
            }
        }

        if let Some(buy_seeds) = self.try_buy_missing_seeds(game_state)? {
            // In some cases, the update to the player's inventory can
            // occur before the update to the game location.  In that
            // case, the total number of seeds may appear to be less
            // than the required number of seeds.  To avoid running
            // off to the general store, wait until the choice to buy
            // seeds has been made for a few game ticks in a row.
            let tick = game_state.globals.game_tick;
            if let Some(prev_tick) = self.tick_decided_to_buy_seeds {
                if tick < prev_tick + 2 {
                    return Ok(BotGoalResult::InProgress);
                }
            } else {
                self.tick_decided_to_buy_seeds = Some(tick);
                return Ok(BotGoalResult::InProgress);
            }

            return Ok(buy_seeds.into());
        } else {
            self.tick_decided_to_buy_seeds = None;
        }

        self.fill_plan(game_state)?;

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
            if !self.opportunistic_clay_farming {
                // Not doing opporunistic clay farming.
                break 'clay_tiles Default::default();
            }

            let clay_predictor = ClayPredictor::new(game_state);
            let clay_tiles: HashSet<Vector<isize>> = iter_tiles_to_hoe()?
                .filter(|tile| clay_predictor.will_produce_clay(*tile))
                .collect();

            if !clay_tiles.is_empty() {
                // There's at least once tile that would produce clay.
                break 'clay_tiles clay_tiles;
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
            let longest_until_clay =
                iter_tiles_to_hoe()?.map(|tile| uses_until_clay(tile)).max();

            iter_tiles_to_hoe()?
                .filter(|tile| {
                    Some(uses_until_clay(*tile)) == longest_until_clay
                })
                .collect()
        };

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
            .min_by_key(|(tile, _)| {
                let dist = distances[*tile].unwrap();
                (dist != 1, dist)
            });

        let Some((tile, opt_item)) = opt_next_step else {
            return Ok(BotGoalResult::Completed);
        };

        let planted_seeds = farm.iter_planted_seeds().counts();

        let get_tools = self
            .seeds
            .iter()
            .filter_map(|item| {
                let currently_planted =
                    planted_seeds.get(&item.id).cloned().unwrap_or(0);
                let to_plant = item.count.saturating_sub(currently_planted);
                (to_plant > 0).then(|| item.clone().with_count(to_plant))
            })
            .fold(InventoryGoal::current(), |goal, item| goal.with(item))
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

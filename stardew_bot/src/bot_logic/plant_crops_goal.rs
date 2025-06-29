use std::collections::{HashMap, HashSet};

use itertools::{Either, Itertools as _};

use crate::{
    bot_logic::{
        graph_search::GraphSearch as _, MaintainStaminaGoal, MovementGoal,
        UseItemOnTile,
    },
    game_state::{Item, ItemCategory, ObjectKind, Vector},
    Error, GameAction, GameState,
};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    ActivateTile, ClayPredictor, FarmPlan, FillWateringCan, GameStateExt as _,
    InventoryGoal,
};

pub struct PlantCropsGoal {
    plan: Option<CropPlantingPlan>,
    expected_seeds: Option<usize>,
    stop_time: i32,
    opportunistic_clay_farming: bool,
}

struct CropPlantingPlan {
    /// Which tiles should be prepared for planting
    to_plant: HashSet<Vector<isize>>,

    /// Which tiles should be cleared out (e.g. for sprinklers and
    /// scarecrows)
    to_clear: HashSet<Vector<isize>>,
}

impl PlantCropsGoal {
    pub fn new() -> Self {
        Self {
            plan: None,
            expected_seeds: None,
            stop_time: 2600,
            opportunistic_clay_farming: false,
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

    pub fn with_expected_seeds(self, expected_seeds: usize) -> Self {
        Self {
            expected_seeds: Some(expected_seeds),
            ..self
        }
    }

    pub fn stop_time(self, stop_time: i32) -> Self {
        Self { stop_time, ..self }
    }

    pub fn is_completed(
        &mut self,
        game_state: &GameState,
    ) -> Result<bool, Error> {
        if game_state.globals.in_game_time >= self.stop_time {
            return Ok(true);
        }

        if self.expected_seeds.is_some() {
            // Prepare based on the number of spaces specified.
            let fully_prepared: HashSet<Vector<isize>> = game_state
                .get_room("Farm")?
                .objects
                .iter()
                .filter(|obj| {
                    obj.kind
                        .as_hoe_dirt()
                        .map(|hoe_dirt| hoe_dirt.is_watered)
                        .unwrap_or(false)
                })
                .map(|obj| obj.tile)
                .collect();

            self.fill_plan(game_state)?;
            let plan =
                self.plan.as_ref().expect("Populated by self.fill_plan()");

            let ready_for_seed = plan
                .to_plant
                .iter()
                .all(|tile| fully_prepared.contains(tile));
            if !ready_for_seed {
                return Ok(false);
            }

            let blocked = CropPlantingPlan::blocked_from_planting(game_state)?;
            let ready_for_sprinklers =
                plan.to_clear.iter().all(|tile| !blocked.contains(tile));
            if !ready_for_sprinklers {
                return Ok(false);
            }
        }

        // Plant based on the number of seeds in player's
        // inventory.  Finished if the inventory no longer
        // contains any seeds.
        let num_seeds = num_seeds(game_state);
        Ok(num_seeds == 0)
    }

    pub fn fill_plan(&mut self, game_state: &GameState) -> Result<(), Error> {
        if self.plan.is_some() {
            return Ok(());
        }
        let plan = FarmPlan::plan(game_state)?;
        let num_seeds =
            self.expected_seeds.unwrap_or_else(|| num_seeds(game_state));

        let blocked = CropPlantingPlan::blocked_from_planting(game_state)?;

        let crop_planting_plan = if game_state.globals.days_played() == 1 {
            let to_plant: HashSet<_> = plan
                .iter_initial_plot()
                .filter(|tile| !blocked.contains(tile))
                .take(num_seeds)
                .collect();
            CropPlantingPlan {
                to_plant,
                to_clear: Default::default(),
            }
        } else {
            let to_plant: HashSet<_> =
                plan.iter_sprinkler_plot().take(num_seeds).collect();
            let to_clear: HashSet<_> = plan.iter_regular_sprinklers().collect();
            CropPlantingPlan { to_plant, to_clear }
        };
        self.plan = Some(crop_planting_plan);

        Ok(())
    }
}

fn iter_seeds(game_state: &GameState) -> impl Iterator<Item = &Item> + '_ {
    game_state
        .player
        .inventory
        .iter_items()
        .filter(|item| matches!(item.category, Some(ItemCategory::Seed)))
        .filter(|item| {
            let id = &item.id;
            !id.is_tree_seed() && !id.is_fruit_sapling()
        })
}

fn num_seeds(game_state: &GameState) -> usize {
    iter_seeds(game_state).map(|item| item.count).sum::<usize>()
}

impl CropPlantingPlan {
    fn iter_blocked_from_planting(
        game_state: &GameState,
    ) -> Result<impl Iterator<Item = Vector<isize>> + '_, Error> {
        let farm = game_state.get_room("Farm")?;

        let iter_blocked = std::iter::empty()
            .chain(farm.iter_water_tiles())
            .chain(farm.iter_bush_tiles())
            .chain(farm.iter_building_tiles())
            .chain(
                farm.objects
                    .iter()
                    .filter(|obj| match &obj.kind {
                        ObjectKind::Stone(_)
                        | ObjectKind::Mineral(_)
                        | ObjectKind::Wood
                        | ObjectKind::Fiber
                        | ObjectKind::Grass
                        | ObjectKind::PotOfGold
                        | ObjectKind::ArtifactSpot
                        | ObjectKind::SeedSpot
                        | ObjectKind::Tree(_) => false,

                        ObjectKind::HoeDirt(hoe_dirt) => hoe_dirt.has_crop(),

                        ObjectKind::Torch
                        | ObjectKind::MineBarrel
                        | ObjectKind::MineLadderUp
                        | ObjectKind::MineLadderDown
                        | ObjectKind::MineHoleDown
                        | ObjectKind::MineElevator
                        | ObjectKind::MineCartCoal
                        | ObjectKind::FruitTree(_)
                        | ObjectKind::Chest(_)
                        | ObjectKind::Furnace(_)
                        | ObjectKind::Other { .. }
                        | ObjectKind::Unknown => true,
                    })
                    .map(|obj| obj.tile),
            )
            .chain(
                farm.resource_clumps
                    .iter()
                    .flat_map(|clump| clump.shape.iter_points()),
            );

        Ok(iter_blocked)
    }

    fn blocked_from_planting(
        game_state: &GameState,
    ) -> Result<HashSet<Vector<isize>>, Error> {
        let iter_blocked = Self::iter_blocked_from_planting(game_state)?;

        Ok(iter_blocked.collect())
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
        let plan = self.plan.as_ref().unwrap();

        let goal = MaintainStaminaGoal::new();
        if !goal.is_completed(game_state) {
            return Ok(goal.into());
        }
        if game_state.player.current_stamina < 2.0 {
            return Ok(BotGoalResult::Completed);
        }

        let farm_door = game_state.get_farm_door()?;
        let goal =
            MovementGoal::new("Farm", farm_door.into()).with_tolerance(1000.0);
        if !goal.is_completed(game_state) {
            return Ok(goal.into());
        }

        let get_tools = InventoryGoal::current()
            .with(Item::WATERING_CAN)
            .with(Item::HOE)
            .with(Item::PICKAXE)
            .with(Item::AXE);
        if !get_tools.is_completed(game_state)? {
            let get_tools = get_tools
                .stamina_recovery_slots(3)
                .keep_if(|item| {
                    matches!(item.category, Some(ItemCategory::Seed))
                })
                .otherwise_empty();
            return Ok(get_tools.into());
        }

        let farm = game_state.get_room("Farm")?;
        let player_tile = game_state.player.tile();

        let current_contents: HashMap<Vector<isize>, &ObjectKind> = farm
            .objects
            .iter()
            .filter(|obj| plan.to_plant.contains(&obj.tile))
            .map(|obj| (obj.tile, &obj.kind))
            .collect();

        let iter_tiles_to_hoe = || {
            plan.to_plant.iter().cloned().filter(|tile| {
                !matches!(
                    current_contents.get(tile),
                    Some(ObjectKind::HoeDirt(_))
                )
            })
        };

        let clay_tiles: HashSet<Vector<isize>> = 'clay_tiles: {
            if !self.opportunistic_clay_farming {
                // Not doing opporunistic clay farming.
                break 'clay_tiles Default::default();
            }

            let clay_predictor = ClayPredictor::new(game_state);
            let clay_tiles: HashSet<Vector<isize>> = iter_tiles_to_hoe()
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
            let num_remaining = iter_tiles_to_hoe().count();
            let uses_until_clay = |tile: Vector<isize>| {
                clay_predictor
                    .iter_will_produce_clay(tile)
                    .take(num_remaining + 1)
                    .take_while(|will_be_clay| !will_be_clay)
                    .count()
            };
            let longest_until_clay =
                iter_tiles_to_hoe().map(|tile| uses_until_clay(tile)).max();

            iter_tiles_to_hoe()
                .filter(|tile| {
                    Some(uses_until_clay(*tile)) == longest_until_clay
                })
                .collect()
        };

        let mut iter_next_clear = plan
            .to_plant
            .iter()
            .chain(plan.to_clear.iter())
            .cloned()
            .filter_map(|tile| {
                let opt_action = match current_contents.get(&tile) {
                    Some(ObjectKind::Stone(_)) => Some(Item::PICKAXE),
                    Some(ObjectKind::Mineral(_)) => None,
                    Some(ObjectKind::Fiber | ObjectKind::Grass) => {
                        Some(Item::SCYTHE)
                    }
                    Some(ObjectKind::Wood | ObjectKind::Tree(_)) => {
                        Some(Item::AXE)
                    }
                    Some(ObjectKind::ArtifactSpot | ObjectKind::SeedSpot) => {
                        Some(Item::HOE)
                    }
                    Some(ObjectKind::PotOfGold) => None,

                    _ => {
                        return None;
                    }
                };
                Some((tile, opt_action))
            });

        let iter_next_plant =
            plan.to_plant.iter().cloned().filter_map(|tile| {
                let action = match current_contents.get(&tile) {
                    Some(ObjectKind::HoeDirt(hoe_dirt))
                        if !hoe_dirt.is_watered =>
                    {
                        Some(Item::WATERING_CAN)
                    }
                    Some(ObjectKind::HoeDirt(hoe_dirt))
                        if hoe_dirt.is_empty() =>
                    {
                        iter_seeds(game_state).next().cloned()
                    }
                    None => {
                        // If any of the tiles that still need to be
                        // hoed will produce clay, then only allow the
                        // hoe to be used on those tiles.  If there
                        // are no tiles remaining that would produce
                        // clay, then hoe whichever tile would take
                        // the longest before producing clay.
                        (clay_tiles.is_empty() || clay_tiles.contains(&tile))
                            .then(|| Item::HOE)
                    }

                    _ => None,
                }?;
                Some((tile, Some(action)))
            });

        let next_steps: HashMap<Vector<isize>, Option<Item>> = {
            if let Some(first_clear) = iter_next_clear.next() {
                std::iter::once(first_clear)
                    .chain(iter_next_clear)
                    .collect()
            } else {
                iter_next_plant.collect()
            }
        };

        let opt_next_step =
            // If any adjacent tiles can be improved upon, do so.
            player_tile.iter_adjacent().find_map(|tile| {
                next_steps.get(&tile).map(|opt_item| (tile,opt_item))
            })
            .or_else(|| {
                next_steps
                    .iter()
                    .min_by_key(|(tile,_)| tile.dist2(player_tile))
                    .map(|(tile,opt_item)| (*tile,opt_item))
            });

        let Some((tile, opt_item)) = opt_next_step else {
            return Ok(BotGoalResult::Completed);
        };

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

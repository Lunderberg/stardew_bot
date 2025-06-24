use std::collections::{HashMap, HashSet};

use itertools::Itertools as _;

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
    ClayPredictor, FillWateringCan, GameStateExt as _,
};

pub struct PlantCropsGoal {
    plan: Option<CropPlantingPlan>,
}

struct CropPlantingPlan {
    /// Which tiles should be prepared for planting
    to_plant: HashSet<Vector<isize>>,
    farm_door: Vector<isize>,
}

impl PlantCropsGoal {
    pub fn new() -> Self {
        Self { plan: None }
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        let num_seeds = num_seeds(game_state);
        num_seeds == 0
    }
}

fn iter_seeds(game_state: &GameState) -> impl Iterator<Item = &Item> + '_ {
    game_state
        .player
        .inventory
        .iter_items()
        .filter(|item| matches!(item.category, Some(ItemCategory::Seed)))
}

fn num_seeds(game_state: &GameState) -> usize {
    iter_seeds(game_state).map(|item| item.count).sum::<usize>()
}

impl CropPlantingPlan {
    pub fn new(game_state: &GameState) -> Result<Self, Error> {
        let farm = game_state.get_room("Farm").unwrap();
        let farm_door = game_state.get_farm_door()?;

        let blocked: HashSet<Vector<isize>> = std::iter::empty()
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

                        ObjectKind::MineBarrel
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
            .collect();

        let plot_top_right = farm_door + Vector::new(3, 5);

        let num_seeds = num_seeds(game_state);

        let num_rows = 6;
        let to_plant = (0..)
            .flat_map(|di| {
                (0..num_rows).map(move |dj| Vector::<isize>::new(-di, dj))
            })
            .map(|offset| plot_top_right + offset)
            .filter(|tile| !blocked.contains(tile))
            .take(num_seeds)
            .collect();

        Ok(CropPlantingPlan {
            to_plant,
            farm_door,
        })
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
        if self.plan.is_none() {
            self.plan = Some(CropPlantingPlan::new(game_state)?);
        }
        let plan = self.plan.as_ref().unwrap();

        let goal = MaintainStaminaGoal::new();
        if !goal.is_completed(game_state) {
            return Ok(goal.into());
        }
        if game_state.player.current_stamina < 2.0 {
            return Ok(BotGoalResult::Completed);
        }

        let goal = MovementGoal::new("Farm", plan.farm_door.map(|x| x as f32))
            .with_tolerance(1000.0);
        if !goal.is_completed(game_state) {
            return Ok(goal.into());
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
        let clay_predictor = ClayPredictor::new(game_state);
        let clay_tiles: HashSet<Vector<isize>> = iter_tiles_to_hoe()
            .filter(|tile| clay_predictor.will_produce_clay(*tile))
            .collect();

        let clay_tiles = if clay_tiles.is_empty() {
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
        } else {
            clay_tiles
        };

        let next_steps: HashMap<Vector<isize>, Item> = plan
            .to_plant
            .iter()
            .filter_map(|tile| {
                let opt_action = match current_contents.get(tile) {
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
                    Some(ObjectKind::HoeDirt(_)) => None,
                    Some(ObjectKind::PotOfGold) => None,
                    None => {
                        // If any of the tiles that still need to be
                        // hoed will produce clay, then only allow the
                        // hoe to be used on those tiles.  If there
                        // are no tiles remaining that would produce
                        // clay, then hoe whichever tile would take
                        // the longest before producing clay.
                        clay_tiles.contains(tile).then(|| Item::HOE)
                    }

                    Some(
                        ObjectKind::MineBarrel
                        | ObjectKind::MineLadderUp
                        | ObjectKind::MineLadderDown
                        | ObjectKind::MineHoleDown
                        | ObjectKind::MineElevator
                        | ObjectKind::MineCartCoal
                        | ObjectKind::Other { .. }
                        | ObjectKind::Unknown
                        | ObjectKind::FruitTree(_)
                        | ObjectKind::Chest(_)
                        | ObjectKind::Furnace(_),
                    ) => unreachable!(
                        "These tiles were excluded from the planning step"
                    ),
                };
                opt_action.map(|action| (*tile, action))
            })
            .collect();

        let opt_tile_item =
            // If any adjacent tiles can be improved upon, do so.
            player_tile.iter_adjacent().find_map(|tile| {
                next_steps.get(&tile).map(|item| (tile,item))
            })
            .or_else(|| {
                next_steps
                    .iter()
                    .min_by_key(|(tile,_)| tile.dist2(player_tile))
                    .map(|(tile,item)| (*tile,item))
            });

        let Some((tile, item)) = opt_tile_item else {
            return Ok(BotGoalResult::Completed);
        };

        if item.is_same_item(&Item::WATERING_CAN) {
            let goal = FillWateringCan::if_empty();
            if !goal.is_completed(game_state) {
                return Ok(goal.into());
            }
        }

        let action = UseItemOnTile::new(item.clone(), "Farm", tile);
        Ok(action.into())
    }
}

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
    bot_logic::{BotGoal, BotGoalResult},
    FillWateringCan, GameStateExt as _,
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
                        ObjectKind::Stone
                        | ObjectKind::Wood
                        | ObjectKind::Fiber
                        | ObjectKind::Grass
                        | ObjectKind::PotOfGold
                        | ObjectKind::ArtifactSpot
                        | ObjectKind::SeedSpot
                        | ObjectKind::Tree(_) => false,
                        ObjectKind::HoeDirt(hoe_dirt) => hoe_dirt.has_crop,
                        ObjectKind::FruitTree(_)
                        | ObjectKind::Chest(_)
                        | ObjectKind::Other(_)
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
    fn description(&self) -> std::borrow::Cow<str> {
        "Plant crops".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _do_action: &mut dyn FnMut(GameAction),
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

        let next_steps: HashMap<Vector<isize>, Item> = plan
            .to_plant
            .iter()
            .filter_map(|tile| {
                let opt_action = match current_contents.get(tile) {
                    Some(ObjectKind::Stone) => Some(Item::PICKAXE),
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
                        if !hoe_dirt.has_crop =>
                    {
                        iter_seeds(game_state).next().cloned()
                    }
                    Some(ObjectKind::HoeDirt(_)) => None,
                    Some(ObjectKind::PotOfGold) => None,
                    None => Some(Item::HOE),

                    Some(
                        ObjectKind::Other(_)
                        | ObjectKind::Unknown
                        | ObjectKind::FruitTree(_)
                        | ObjectKind::Chest(_),
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

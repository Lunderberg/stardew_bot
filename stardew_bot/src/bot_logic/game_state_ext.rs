use std::collections::HashMap;

use itertools::Itertools as _;

use crate::{
    game_state::{Item, ItemId, Location, ObjectKind, Vector},
    Error, GameState,
};

use super::{BotError, MovementGoal};

pub trait GameStateExt {
    fn get_farm_door(&self) -> Result<Vector<isize>, Error>;

    fn get_mine_elevator(&self) -> Result<Vector<isize>, Error>;

    fn iter_accessible_items(
        &self,
    ) -> Result<impl Iterator<Item = &Item> + '_, Error>;

    fn closest_entrance(
        &self,
        target_room: &str,
    ) -> Result<Vector<isize>, Error>;
}

impl GameStateExt for GameState {
    fn get_farm_door(&self) -> Result<Vector<isize>, Error> {
        let farm = self.get_room("Farm")?;
        let farm_door = farm
            .buildings
            .iter()
            .find_map(|building| {
                building
                    .door
                    .as_ref()
                    .filter(|door| door.inside_name == "FarmHouse")
                    .map(|door| {
                        building.shape.top_left + door.relative_location
                    })
            })
            .ok_or_else(|| BotError::FarmhouseDoorNotFound)?;

        Ok(farm_door)
    }

    fn get_mine_elevator(&self) -> Result<Vector<isize>, Error> {
        let mine = self.get_room("Mine")?;
        let elevator = mine
            .action_tiles
            .iter()
            .find(|(_, action)| action == "MineElevator")
            .map(|(tile, _)| *tile)
            .ok_or_else(|| BotError::MineElevatorNotFound)?;

        Ok(elevator)
    }

    fn iter_accessible_items(
        &self,
    ) -> Result<impl Iterator<Item = &Item> + '_, Error> {
        let iter = std::iter::once(&self.player.inventory)
            .chain(self.get_room("Farm")?.objects.iter().filter_map(|obj| {
                match &obj.kind {
                    ObjectKind::Chest(chest) => Some(&chest.inventory),
                    _ => None,
                }
            }))
            .flat_map(|inventory| inventory.iter_items());

        Ok(iter)
    }

    fn closest_entrance(
        &self,
        target_room: &str,
    ) -> Result<Vector<isize>, Error> {
        MovementGoal::closest_entrance(self, target_room)
    }
}

pub trait ObjectKindExt {
    fn get_tool(&self) -> Option<Item>;
}

impl ObjectKindExt for ObjectKind {
    fn get_tool(&self) -> Option<Item> {
        match self {
            ObjectKind::Stone(_)
            | ObjectKind::Torch
            | ObjectKind::Sprinkler(_)
            | ObjectKind::Scarecrow => Some(Item::PICKAXE),

            ObjectKind::Mineral(_) => None,
            ObjectKind::Wood => Some(Item::AXE),
            ObjectKind::Tree(tree) => (tree.health > 0.0).then(|| Item::AXE),

            ObjectKind::MineBarrel | ObjectKind::Fiber | ObjectKind::Grass => {
                Some(Item::SCYTHE)
            }

            ObjectKind::ArtifactSpot | ObjectKind::SeedSpot => Some(Item::HOE),

            ObjectKind::MineLadderUp
            | ObjectKind::MineLadderDown
            | ObjectKind::MineHoleDown
            | ObjectKind::MineElevator
            | ObjectKind::MineCartCoal
            | ObjectKind::PotOfGold
            | ObjectKind::FruitTree(_)
            | ObjectKind::HoeDirt(_)
            | ObjectKind::Chest(_)
            | ObjectKind::Furnace(_)
            | ObjectKind::Other { .. }
            | ObjectKind::Unknown => None,
        }
    }
}

pub trait LocationExt {
    fn generate_tile_lookup(&self) -> HashMap<Vector<isize>, &ObjectKind>;

    #[allow(dead_code)]
    fn iter_planted_seeds(&self) -> impl Iterator<Item = &ItemId>;
}
impl LocationExt for Location {
    fn generate_tile_lookup(&self) -> HashMap<Vector<isize>, &ObjectKind> {
        self.objects
            .iter()
            .sorted_by_key(|obj| {
                // Since some objects may be placed on top of hoe dirt
                // or grass, collect these object types first.  The
                // object overtop will then overwrite them in the
                // HashMap.
                match &obj.kind {
                    ObjectKind::Grass => 0,
                    ObjectKind::HoeDirt(_) => 1,
                    _ => 2,
                }
            })
            .map(|obj| (obj.tile, &obj.kind))
            .collect()
    }

    fn iter_planted_seeds(&self) -> impl Iterator<Item = &ItemId> {
        self.objects
            .iter()
            .filter_map(|obj| obj.kind.as_hoe_dirt())
            .filter_map(|hoe_dirt| hoe_dirt.crop.as_ref())
            .map(|crop| &crop.seed)
    }
}

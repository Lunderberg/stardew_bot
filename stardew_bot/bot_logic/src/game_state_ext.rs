use std::collections::HashMap;

use geometry::Vector;
use itertools::Itertools as _;

use game_state::{
    GameState, Item, ItemId, Location, ObjectKind, ResourceClumpKind,
};

use crate::{best_weapon, Error, MovementGoal, Pathfinding};

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

    fn collect_clearable_tiles(
        &self,
    ) -> Result<HashMap<Vector<isize>, Option<ItemId>>, Error>;
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
            .ok_or_else(|| Error::FarmhouseDoorNotFound)?;

        Ok(farm_door)
    }

    fn get_mine_elevator(&self) -> Result<Vector<isize>, Error> {
        let mine = self.get_room("Mine")?;
        let elevator = mine
            .action_tiles
            .iter()
            .find(|(_, action)| action == "MineElevator")
            .map(|(tile, _)| *tile)
            .ok_or_else(|| Error::MineElevatorNotFound)?;

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
        // Workaround since pathfinding doesn't currently know how
        // to return from the underground mines.
        if target_room == "Farm"
            && self.player.room_name.starts_with("UndergroundMine")
        {
            return self.get_farm_door();
        }
        MovementGoal::closest_entrance(self, target_room)
    }

    fn collect_clearable_tiles(
        &self,
    ) -> Result<HashMap<Vector<isize>, Option<ItemId>>, Error> {
        let current_room = self.current_room()?;
        let opt_weapon = best_weapon(self.player.inventory.iter_items())
            .map(|item| &item.id);

        current_room.collect_clearable_tiles(opt_weapon)
    }
}

pub trait ObjectKindExt {
    fn get_tool(&self) -> Option<ItemId>;
}

impl ObjectKindExt for ObjectKind {
    fn get_tool(&self) -> Option<ItemId> {
        match self {
            ObjectKind::Stone(_)
            | ObjectKind::Torch
            | ObjectKind::Sprinkler(_)
            | ObjectKind::Scarecrow => Some(ItemId::PICKAXE),

            ObjectKind::Mineral(_) => None,
            ObjectKind::Wood => Some(ItemId::AXE),
            ObjectKind::Tree(tree) => (tree.health > 0.0).then(|| ItemId::AXE),

            ObjectKind::MineBarrel | ObjectKind::Fiber | ObjectKind::Grass => {
                Some(ItemId::SCYTHE)
            }

            ObjectKind::ArtifactSpot | ObjectKind::SeedSpot => {
                Some(ItemId::HOE)
            }

            ObjectKind::MineLadderUp
            | ObjectKind::MineLadderDown
            | ObjectKind::MineHoleDown
            | ObjectKind::MineElevator
            | ObjectKind::MineCartCoal
            | ObjectKind::PotOfGold
            | ObjectKind::FruitTree(_)
            | ObjectKind::HoeDirt(_)
            | ObjectKind::Chest(_)
            | ObjectKind::CraftingMachine(_)
            | ObjectKind::Other { .. }
            | ObjectKind::Unknown => None,
        }
    }
}

pub trait LocationExt {
    fn generate_tile_lookup(&self) -> HashMap<Vector<isize>, &ObjectKind>;

    #[allow(dead_code)]
    fn iter_planted_seeds(&self) -> impl Iterator<Item = &ItemId>;

    fn iter_stored_items(&self) -> impl Iterator<Item = &Item>;

    fn collect_clearable_tiles(
        &self,
        opt_weapon: Option<&ItemId>,
    ) -> Result<HashMap<Vector<isize>, Option<ItemId>>, Error>;

    fn pathfinding(&self) -> Pathfinding<'_>;
}
impl LocationExt for Location {
    fn pathfinding(&self) -> Pathfinding<'_> {
        Pathfinding::new(self)
    }

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

    fn iter_stored_items(&self) -> impl Iterator<Item = &Item> {
        self.objects
            .iter()
            .filter_map(|obj| obj.kind.as_chest())
            .flat_map(|chest| chest.iter_items())
    }

    fn collect_clearable_tiles(
        &self,
        opt_weapon: Option<&ItemId>,
    ) -> Result<HashMap<Vector<isize>, Option<ItemId>>, Error> {
        let iter_clearable_obj = self.objects.iter().filter_map(|obj| {
            let opt_tool = match &obj.kind {
                ObjectKind::Stone(_) => Some(ItemId::PICKAXE),
                ObjectKind::Wood => Some(ItemId::AXE),

                ObjectKind::Fiber
                | ObjectKind::Grass
                | ObjectKind::MineBarrel
                    if opt_weapon.is_some() =>
                {
                    opt_weapon.cloned()
                }
                ObjectKind::Mineral(_) => None,

                other if other.is_forage() => None,

                _ => {
                    return None;
                }
            };
            Some((obj.tile, opt_tool))
        });
        let iter_clearable_clump = self
            .resource_clumps
            .iter()
            .filter_map(|clump| {
                let tool = match &clump.kind {
                    ResourceClumpKind::MineBoulder => Some(ItemId::PICKAXE),
                    _ => None,
                }?;
                Some(
                    clump
                        .shape
                        .iter_points()
                        .map(move |tile| (tile, Some(tool.clone()))),
                )
            })
            .flatten();

        let clearable_tiles =
            iter_clearable_obj.chain(iter_clearable_clump).collect();

        Ok(clearable_tiles)
    }
}

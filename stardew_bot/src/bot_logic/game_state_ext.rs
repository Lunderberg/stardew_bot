use crate::{
    game_state::{Item, ObjectKind, Vector},
    Error, GameState,
};

use super::BotError;

pub trait GameStateExt {
    fn get_farm_door(&self) -> Result<Vector<isize>, Error>;
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
}

pub trait ObjectKindExt {
    fn get_tool(&self) -> Option<Item>;
}

impl ObjectKindExt for ObjectKind {
    fn get_tool(&self) -> Option<Item> {
        match self {
            ObjectKind::Stone => Some(Item::PICKAXE),
            ObjectKind::Wood => Some(Item::AXE),
            ObjectKind::Tree(tree) if tree.health > 0.0 => Some(Item::AXE),
            ObjectKind::Fiber | ObjectKind::Grass => Some(Item::SCYTHE),
            _ => None,
        }
    }
}

use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};

use crate::{Direction, Error};

use super::Vector;

#[derive(RustNativeObject, Debug, Clone)]
pub struct PlayerState {
    pub position: Vector<f32>,
    pub facing: FacingDirection,
    pub movement: Option<Direction>,
    pub room_name: String,
    pub skills: PlayerSkills,
    pub fade_to_black: bool,
}

#[derive(RustNativeObject, Debug, Clone)]
pub struct PlayerSkills {
    pub farming_xp: usize,
    pub fishing_xp: usize,
    pub foraging_xp: usize,
    pub mining_xp: usize,
    pub combat_xp: usize,
}

#[derive(RustNativeObject, Debug, Clone, Copy, PartialEq, Eq)]
pub enum FacingDirection {
    North,
    East,
    South,
    West,
}

impl PlayerState {
    pub(crate) fn read_all(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function(
            "new_f32_vector",
            |right: f32, down: f32| Vector::<f32> { right, down },
        )?;

        graph.named_native_function(
            "new_direction",
            FacingDirection::from_value,
        )?;

        graph.named_native_function(
            "new_movement_direction",
            |directions: &Vec<i32>| -> Option<Direction> {
                let to_dir = |value: i32| -> Option<Direction> {
                    match value {
                        0 => Some(Direction::North),
                        1 => Some(Direction::East),
                        2 => Some(Direction::South),
                        3 => Some(Direction::West),
                        _ => None,
                    }
                };

                let dir_0 = directions.get(0).cloned().and_then(to_dir);
                let dir_1 = directions.get(1).cloned().and_then(to_dir);
                match (dir_0, dir_1) {
                    (d, None) => d,
                    (Some(Direction::North), Some(Direction::East))
                    | (Some(Direction::East), Some(Direction::North)) => {
                        Some(Direction::NorthEast)
                    }
                    (Some(Direction::North), Some(Direction::West))
                    | (Some(Direction::West), Some(Direction::North)) => {
                        Some(Direction::NorthWest)
                    }
                    (Some(Direction::South), Some(Direction::East))
                    | (Some(Direction::East), Some(Direction::South)) => {
                        Some(Direction::SouthEast)
                    }
                    (Some(Direction::South), Some(Direction::West))
                    | (Some(Direction::West), Some(Direction::South)) => {
                        Some(Direction::SouthWest)
                    }
                    _ => None,
                }
            },
        )?;

        graph.named_native_function(
            "new_skills",
            |farming_xp: usize,
             fishing_xp: usize,
             foraging_xp: usize,
             mining_xp: usize,
             combat_xp: usize| PlayerSkills {
                farming_xp,
                fishing_xp,
                foraging_xp,
                mining_xp,
                combat_xp,
            },
        )?;

        graph.named_native_function(
            "new_player",
            |position: &Vector<f32>,
             facing: &FacingDirection,
             movement: Option<&Direction>,
             room_name: &str,
             skills: &PlayerSkills,
             fade_to_black: bool| {
                PlayerState {
                    position: position.clone(),
                    facing: *facing,
                    movement: movement.cloned(),
                    room_name: room_name.into(),
                    skills: skills.clone(),
                    fade_to_black,
                }
            },
        )?;

        let player = graph.parse(stringify! {
            fn read_player() {
                let player = StardewValley.Game1._player;

                let position = {
                    let pos = player
                        .position
                        .Field
                        .value;

                    new_f32_vector(pos.X, pos.Y)
                };

                let facing = new_direction(
                    player
                        .facingDirection
                        .value
                );

                let num_movement_directions = player
                    .movementDirections
                    ._size
                    .prim_cast::<usize>();
                let directions = (0..num_movement_directions)
                    .map(|i| player
                        .movementDirections
                        ._items[i])
                    .collect();
                let movement = new_movement_direction(directions);

                let room_name = player
                    .currentLocationRef
                    .locationName
                    .value
                    .read_string();

                let skills = {
                    let skill_list = player.experiencePoints.elements._items;
                    let farming = skill_list[0].value;
                    let fishing = skill_list[1].value;
                    let foraging = skill_list[2].value;
                    let mining = skill_list[3].value;
                    let combat = skill_list[4].value;

                    new_skills(farming, fishing, foraging, mining, combat)
                };

                let fade_to_black = StardewValley.Game1
                    .screenFade
                    .fadeToBlack;

                new_player(
                    position,
                    facing,
                    movement,
                    room_name,
                    skills,
                    fade_to_black
                )
            }
        })?;

        Ok(player)
    }

    pub fn tile(&self) -> Vector<isize> {
        self.position
            .map(|x| x / 64.0)
            .map(|x| x.round())
            .map(|x| x as isize)
    }
}

impl FacingDirection {
    fn from_value(value: usize) -> Option<FacingDirection> {
        match value {
            0 => Some(FacingDirection::North),
            1 => Some(FacingDirection::East),
            2 => Some(FacingDirection::South),
            3 => Some(FacingDirection::West),
            _ => None,
        }
    }
}

impl std::fmt::Display for FacingDirection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FacingDirection::North => write!(f, "North"),
            FacingDirection::East => write!(f, "East"),
            FacingDirection::South => write!(f, "South"),
            FacingDirection::West => write!(f, "West"),
        }
    }
}

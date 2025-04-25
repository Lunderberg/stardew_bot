use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};

use crate::Error;

use super::Vector;

#[derive(RustNativeObject, Debug, Clone)]
pub struct PlayerState {
    pub position: Vector<f32>,
    pub facing: FacingDirection,
    pub room_name: String,
    pub skills: PlayerSkills,
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
             room_name: &str,
             skills: &PlayerSkills| {
                PlayerState {
                    position: position.clone(),
                    facing: *facing,
                    room_name: room_name.into(),
                    skills: skills.clone(),
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

                new_player(position, facing, room_name, skills)
            }
        })?;

        Ok(player)
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

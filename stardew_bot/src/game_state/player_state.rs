use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};

use crate::{Direction, Error};

use super::{Inventory, Item, Vector};

#[derive(RustNativeObject, Debug, Clone)]
pub struct PlayerState {
    // Position-related info
    pub position: Vector<f32>,
    pub facing: FacingDirection,
    pub movement: Option<Direction>,
    pub room_name: String,

    // Player's progression
    pub skills: PlayerSkills,
    pub current_stamina: f32,
    pub max_stamina: i32,
    pub current_health: i32,
    pub max_health: i32,
    pub is_eating: bool,

    // Inventory-related info
    pub inventory: Inventory,
    pub active_hotbar_index: usize,
    pub current_money: i32,

    // Info related to the current action being performed
    pub using_tool: bool,
    pub melee_animation_frame: Option<i32>,
    pub can_move: bool,
    pub can_release_tool: bool,
    pub last_click: Vector<isize>,

    // Per-player game state
    pub num_unread_mail: usize,
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
    pub(crate) fn def_read_player(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function(
            "new_f32_vector",
            |right: f32, down: f32| Vector::<f32> { right, down },
        )?;

        graph.named_native_function(
            "new_isize_vector",
            |right: isize, down: isize| Vector::<isize> { right, down },
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
             inventory: &Inventory,
             active_hotbar_index: usize,
             current_money: i32,
             using_tool: bool,
             melee_animation_frame: Option<i32>,
             can_move: bool,
             can_release_tool: bool,
             last_click: &Vector<isize>,
             current_stamina: f32,
             max_stamina: i32,
             current_health: i32,
             max_health: i32,
             is_eating: bool,
             num_unread_mail: usize| {
                PlayerState {
                    position: position.clone(),
                    facing: *facing,
                    movement: movement.cloned(),
                    room_name: room_name.into(),
                    skills: skills.clone(),
                    inventory: inventory.clone(),
                    active_hotbar_index,
                    using_tool,
                    melee_animation_frame,
                    can_move,
                    can_release_tool,
                    current_money,
                    last_click: *last_click,
                    current_stamina,
                    max_stamina,
                    current_health,
                    max_health,
                    is_eating,
                    num_unread_mail,
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

                let inventory = read_inventory(player.netItems.value);

                let active_hotbar_index = player
                    .currentToolIndex
                    .value
                    .prim_cast::<usize>();

                let using_tool = player.usingTool.value;
                let can_move = player.canMove;
                let can_release_tool = player.netCanReleaseTool.value;
                let last_click = {
                    let pos = StardewValley.Game1._player.lastClick;
                    new_isize_vector(pos.X, pos.Y)
                };

                let current_stamina = player.netStamina.value;
                let max_stamina = player.maxStamina.value;
                let current_health = player.health;
                let max_health = player.maxHealth;
                let is_eating = player.isEating;

                let current_money = player.teamRoot.value.money.value;

                let num_unread_mail = player
                    .mailbox
                    .count
                    .value
                    .prim_cast::<usize>();

                let sprite = player.sprite.value.as::<StardewValley.FarmerSprite>();
                let is_swinging_melee_weapon = (
                    sprite.currentSingleAnimation == 232i32 ||
                        sprite.currentSingleAnimation == 240i32 ||
                        sprite.currentSingleAnimation == 248i32 ||
                        sprite.currentSingleAnimation == 256i32
                );
                let melee_animation_frame = if is_swinging_melee_weapon {
                    sprite.currentAnimationIndex
                } else {
                    None
                };

                new_player(
                    position,
                    facing,
                    movement,
                    room_name,
                    skills,
                    inventory,
                    active_hotbar_index,
                    current_money,
                    using_tool,
                    melee_animation_frame,
                    can_move,
                    can_release_tool,
                    last_click,
                    current_stamina,
                    max_stamina,
                    current_health,
                    max_health,
                    is_eating,
                    num_unread_mail,
                )
            }
        })?;

        Ok(player)
    }

    fn center_pixel(&self) -> Vector<isize> {
        let offset = Vector::<isize>::new(8, 0);
        let bounding_box = Vector::<isize>::new(48, 32);

        let position = self.position.map(|x| x as isize);
        position + offset + bounding_box / 2
    }

    /// The tile that contains the center of the player's bounding
    /// box.  This is the tile used to determine tool reach (may only
    /// target tiles at or adjacent to the player's tile).
    pub fn tile(&self) -> Vector<isize> {
        self.center_pixel().map(|x| x / 64)
    }

    /// The center of the player's bounding box, adjusted to be
    /// directly comparable to tile coordinates.
    pub fn center_pos(&self) -> Vector<f32> {
        self.center_pixel().map(|x| {
            let x = x - 32;
            let x = x as f32;
            x / 64.0
        })
    }

    pub fn selected_item(&self) -> Option<&Item> {
        self.inventory
            .items
            .get(self.active_hotbar_index)
            .map(|opt_item| opt_item.as_ref())
            .flatten()
    }
}

impl PlayerSkills {
    fn xp_to_level(xp: usize) -> u8 {
        if xp > 15000 {
            10
        } else if xp > 10000 {
            9
        } else if xp > 6900 {
            8
        } else if xp > 4800 {
            7
        } else if xp > 3300 {
            6
        } else if xp > 2150 {
            5
        } else if xp > 1300 {
            4
        } else if xp > 770 {
            3
        } else if xp > 380 {
            2
        } else if xp > 100 {
            1
        } else {
            0
        }
    }

    pub fn farming_level(&self) -> u8 {
        Self::xp_to_level(self.farming_xp)
    }
    pub fn fishing_level(&self) -> u8 {
        Self::xp_to_level(self.fishing_xp)
    }
    pub fn foraging_level(&self) -> u8 {
        Self::xp_to_level(self.foraging_xp)
    }
    pub fn mining_level(&self) -> u8 {
        Self::xp_to_level(self.mining_xp)
    }
    pub fn combat_level(&self) -> u8 {
        Self::xp_to_level(self.combat_xp)
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

    pub fn offset(self) -> Vector<isize> {
        match self {
            FacingDirection::North => Vector::new(0, -1),
            FacingDirection::East => Vector::new(1, 0),
            FacingDirection::South => Vector::new(0, 1),
            FacingDirection::West => Vector::new(-1, 0),
        }
    }

    pub fn iter() -> impl Iterator<Item = Self> {
        [
            FacingDirection::South,
            FacingDirection::North,
            FacingDirection::East,
            FacingDirection::West,
        ]
        .into_iter()
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

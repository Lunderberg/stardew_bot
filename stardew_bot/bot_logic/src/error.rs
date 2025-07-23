use geometry::{Rectangle, Vector};
use itertools::Itertools as _;
use thiserror::Error;

use game_state::ItemId;

#[derive(Error)]
pub enum Error {
    #[error("game_state::Error( {0} )")]
    GameState(#[from] game_state::Error),

    #[error("The bot has achieved all of its goals.")]
    NoRemainingGoals,

    #[error("No route to room '{from_room}' to room '{to_room}'")]
    NoRouteToRoom { from_room: String, to_room: String },

    #[error("No route from tile {start} to {goal} in {room}")]
    NoRouteToTarget {
        room: String,
        start: Vector<isize>,
        goal: Vector<isize>,
    },

    #[error(
        "Within {room}, no route from {start} to any of [{goal_fmt}]",
        goal_fmt = .goals.iter().format(", "),
    )]
    NoRouteToTargets {
        room: String,
        start: Vector<isize>,
        goals: Vec<Vector<isize>>,
    },

    #[error("Could not find room named '{0}'")]
    UnknownRoom(String),

    #[error("Could not find any tile with action '{0}'")]
    NoTileWithAction(String),

    #[error("Cannot buy {item} from {merchant}")]
    ItemNotSold { merchant: String, item: ItemId },

    #[error("Could not locate the farmhouse door")]
    FarmhouseDoorNotFound,

    #[error("Could not locate the mine's elevator")]
    MineElevatorNotFound,

    #[error("Could not locate the mine ladder")]
    MineLadderNotFound,

    #[error("Expected empty inventory slot to be available")]
    ExpectedEmptySlot,

    #[error("Expected item {0} to be in the inventory")]
    ExpectedItemInInventory(ItemId),

    #[error(
        "Bridge repair must refresh Beach to complete, \
         which should only happen when the player is at the Beach, \
         but the player is not currently at the Beach."
    )]
    InconsistentBridgeRepairState,

    #[error(
        "Cannot plan local movement within room {expected_room} \
         while player is located in {current_room}."
    )]
    IncorrectRoomToGenerateLocalMovementPlan {
        current_room: String,
        expected_room: String,
    },

    #[error(
        "Player's position {pos} is out of bounds \
         for room of size {room_bounds}"
    )]
    PlayerIsOutOfBounds {
        pos: Vector<f32>,
        room_bounds: Rectangle<isize>,
    },

    #[error("Could not find any remaining monster")]
    NoRemainingMonsterFound,

    #[error("Could not find villager '{0}'")]
    VillagerNotFound(String),

    #[error("The MineNearbyOre was used when not in the mines")]
    MineNearbyOreUsedOutsideOfMines,
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

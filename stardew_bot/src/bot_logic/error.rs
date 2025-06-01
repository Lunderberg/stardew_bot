use itertools::Itertools as _;
use thiserror::Error;

use crate::game_state::{Item, Vector};

#[derive(Error)]
pub enum BotError {
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
    ItemNotSold { merchant: String, item: Item },
}

impl std::fmt::Debug for BotError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

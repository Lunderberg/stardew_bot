use thiserror::Error;

use crate::game_state::Vector;

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

    #[error("Could not find room named '{0}'")]
    UnknownRoom(String),

    #[error("Could not find any tile with action '{0}'")]
    NoTileWithAction(String),
}

impl std::fmt::Debug for BotError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

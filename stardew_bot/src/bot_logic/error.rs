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
}

impl std::fmt::Debug for BotError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

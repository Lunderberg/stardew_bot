use thiserror::Error;

use crate::game_state::Vector;

#[derive(Error)]
pub enum BotError {
    #[error("No route to room '{from_room}' to room '{to_room}'")]
    NoRouteToRoom { from_room: String, to_room: String },

    #[error("No route to tile {position} in {room}")]
    NoRouteToTarget { room: String, position: Vector<f32> },
}

impl std::fmt::Debug for BotError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

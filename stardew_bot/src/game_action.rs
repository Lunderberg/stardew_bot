use enigo::{Direction, Enigo, Key, Keyboard as _};

use crate::Error;

pub enum GameAction {
    UseTool,
    HoldTool,
    ReleaseTool,
}

impl GameAction {
    pub fn apply(&self, enigo: &mut Enigo) -> Result<(), Error> {
        let tool_key = Key::Unicode('c');
        match self {
            GameAction::UseTool => enigo.key(tool_key, Direction::Click)?,
            GameAction::HoldTool => enigo.key(tool_key, Direction::Press)?,
            GameAction::ReleaseTool => {
                enigo.key(tool_key, Direction::Release)?
            }
        }
        Ok(())
    }
}

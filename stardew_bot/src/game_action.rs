use x11rb::protocol::xproto::{ButtonIndex, Keycode as X11KeyCode};

use crate::{game_state::Vector, Direction, Error, GameState, X11Handler};

#[derive(Debug)]
pub enum GameAction {
    Wait,
    HoldTool,
    ReleaseTool,
    Move(Direction),
    StopMoving,

    ExitMenu,
    StopExitingMenu,

    /// Send the 'x' keystroke, to activate a tile.
    // TODO: Send a mouse event instead.  This will allow the bot to
    // activate doors from a diagonal.
    ActivateTile,
    StopActivatingTile,

    LeftClickPixel(Vector<isize>),
    LeftClickTile(Vector<isize>),
    ReleaseLeftClick,
    RightClickPixel(Vector<isize>),
    RightClickTile(Vector<isize>),
    ReleaseRightClick,
}

impl GameAction {
    // TODO: Actually look up the X11KeyCode using xkbcommon
    //
    // Example:
    // https://github.com/rust-x-bindings/toy_xcb/blob/master/src/keyboard.rs
    const KEY_W: X11KeyCode = 25;
    const KEY_A: X11KeyCode = 38;
    const KEY_S: X11KeyCode = 39;
    const KEY_D: X11KeyCode = 40;

    const KEY_X: X11KeyCode = 53;
    const KEY_C: X11KeyCode = 54;

    const KEY_ESCAPE: X11KeyCode = 9;

    const MOUSE_LEFT: ButtonIndex = ButtonIndex::M1;
    const MOUSE_RIGHT: ButtonIndex = ButtonIndex::M3;

    pub fn apply(
        &self,
        handler: &mut X11Handler,
        window: x11rb::protocol::xproto::Window,
        game_state: &GameState,
    ) -> Result<(), Error> {
        match self {
            GameAction::Wait => {}
            GameAction::HoldTool => {
                handler.send_keystroke(true, Self::KEY_C)?;
            }
            GameAction::ReleaseTool => {
                handler.send_keystroke(false, Self::KEY_C)?;
            }
            GameAction::Move(direction) => {
                let keystate = match direction {
                    Direction::North => [true, false, false, false],
                    Direction::NorthEast => [true, false, false, true],
                    Direction::East => [false, false, false, true],
                    Direction::SouthEast => [false, false, true, true],
                    Direction::South => [false, false, true, false],
                    Direction::SouthWest => [false, true, true, false],
                    Direction::West => [false, true, false, false],
                    Direction::NorthWest => [true, true, false, false],
                };
                keystate
                    .into_iter()
                    .zip([Self::KEY_W, Self::KEY_A, Self::KEY_S, Self::KEY_D])
                    .try_for_each(|(state, key)| {
                        handler.send_keystroke(state, key)
                    })?;
            }
            GameAction::StopMoving => {
                handler.send_keystroke(false, Self::KEY_W)?;
                handler.send_keystroke(false, Self::KEY_A)?;
                handler.send_keystroke(false, Self::KEY_S)?;
                handler.send_keystroke(false, Self::KEY_D)?;
            }
            GameAction::ActivateTile => {
                handler.send_keystroke(true, Self::KEY_X)?
            }
            GameAction::StopActivatingTile => {
                handler.send_keystroke(false, Self::KEY_X)?
            }
            &GameAction::LeftClickPixel(pixel) => {
                handler.move_mouse(pixel)?;
                handler.send_click(true, Self::MOUSE_LEFT)?
            }
            &GameAction::LeftClickTile(tile) => {
                let pixel = game_state.display.center_pixel_of_tile(tile);
                handler.move_mouse(pixel)?;
                handler.send_click(true, Self::MOUSE_LEFT)?
            }
            GameAction::ReleaseLeftClick => {
                handler.send_click(false, Self::MOUSE_LEFT)?
            }
            &GameAction::RightClickPixel(pixel) => {
                handler.move_mouse(pixel)?;
                handler.send_click(true, Self::MOUSE_RIGHT)?
            }
            &GameAction::RightClickTile(tile) => {
                let pixel = game_state.display.center_pixel_of_tile(tile);
                handler.move_mouse(pixel)?;
                handler.send_click(true, Self::MOUSE_RIGHT)?
            }
            GameAction::ReleaseRightClick => {
                handler.send_click(false, Self::MOUSE_RIGHT)?
            }

            GameAction::ExitMenu => {
                handler.send_keystroke(true, Self::KEY_ESCAPE)?
            }
            GameAction::StopExitingMenu => {
                handler.send_keystroke(false, Self::KEY_ESCAPE)?
            }
        }

        Ok(())
    }
}

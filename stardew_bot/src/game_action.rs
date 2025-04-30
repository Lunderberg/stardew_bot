use x11rb::protocol::xproto::{
    ConnectionExt as _, KeyPressEvent, Keycode as X11KeyCode, KEY_PRESS_EVENT,
    KEY_RELEASE_EVENT,
};

use crate::{Direction, Error, X11Error, X11Handler};

pub enum GameAction {
    Wait,
    HoldTool,
    ReleaseTool,
    Move(Direction),
    StopMoving,

    /// Send the 'x' keystroke, to activate a tile.
    // TODO: Send a mouse event instead.  This will allow the bot to
    // activate doors from a diagonal.
    ActivateTile,
    StopActivatingTile,
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

    pub fn apply(
        &self,
        handler: &mut X11Handler,
        window: x11rb::protocol::xproto::Window,
    ) -> Result<(), Error> {
        let root = handler.get_root()?;

        let send_event =
            |press: bool, keycode: X11KeyCode| -> Result<(), X11Error> {
                let response_type = if press {
                    KEY_PRESS_EVENT
                } else {
                    KEY_RELEASE_EVENT
                };

                let event = KeyPressEvent {
                    response_type,
                    detail: keycode,
                    sequence: 0,
                    time: x11rb::CURRENT_TIME,
                    root,
                    event: window,
                    child: window,
                    root_x: 0,
                    root_y: 0,
                    event_x: 0,
                    event_y: 0,
                    state: x11rb::protocol::xproto::KeyButMask::default(),
                    same_screen: true,
                };
                handler.conn.send_event(
                    /* propagate = */ false,
                    window,
                    x11rb::protocol::xproto::EventMask::STRUCTURE_NOTIFY,
                    &event,
                )?;

                Ok(())
            };

        match self {
            GameAction::Wait => {}
            GameAction::HoldTool => {
                send_event(true, Self::KEY_C)?;
            }
            GameAction::ReleaseTool => {
                send_event(false, Self::KEY_C)?;
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
                    .try_for_each(|(state, key)| send_event(state, key))?;
            }
            GameAction::StopMoving => {
                send_event(false, Self::KEY_W)?;
                send_event(false, Self::KEY_A)?;
                send_event(false, Self::KEY_S)?;
                send_event(false, Self::KEY_D)?;
            }
            GameAction::ActivateTile => send_event(true, Self::KEY_X)?,
            GameAction::StopActivatingTile => send_event(false, Self::KEY_X)?,
        }

        Ok(())
    }
}

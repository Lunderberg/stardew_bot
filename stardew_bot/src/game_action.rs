use std::collections::HashSet;

use x11rb::protocol::xproto::{
    ConnectionExt as _, KeyPressEvent, KeyReleaseEvent, Keycode,
    KEY_PRESS_EVENT, KEY_RELEASE_EVENT,
};

use crate::{Direction, Error, X11Error, X11Handler};

#[derive(Default)]
pub struct InputState {
    pressed_keys: HashSet<Keycode>,
}

pub enum GameAction {
    UseTool,
    HoldTool,
    ReleaseTool,
    Move(Direction),
    StopMoving,
}

impl GameAction {
    // TODO: Actually look up the Keycode using xkbcommon
    //
    // Example:
    // https://github.com/rust-x-bindings/toy_xcb/blob/master/src/keyboard.rs
    const KEY_C: Keycode = 54;
    const KEY_W: Keycode = 25;
    const KEY_A: Keycode = 38;
    const KEY_S: Keycode = 39;
    const KEY_D: Keycode = 40;

    pub fn apply(
        &self,
        state: &mut InputState,
        handler: &mut X11Handler,
        window: x11rb::protocol::xproto::Window,
    ) -> Result<(), Error> {
        let time = x11rb::CURRENT_TIME;
        let root = handler.get_root()?;
        let event: x11rb::protocol::xproto::Window = window;
        let child: x11rb::protocol::xproto::Window = window;
        let root_x: i16 = 0;
        let root_y: i16 = 0;
        let event_x: i16 = 0;
        let event_y: i16 = 0;
        let modifier_keys: x11rb::protocol::xproto::KeyButMask =
            x11rb::protocol::xproto::KeyButMask::default();
        let same_screen: bool = true;

        let mut send_event =
            |press: bool, keycode: Keycode| -> Result<(), X11Error> {
                if press {
                    state.pressed_keys.insert(keycode);
                } else {
                    state.pressed_keys.remove(&keycode);
                }

                if press {
                    let event = KeyPressEvent {
                        response_type: KEY_PRESS_EVENT,
                        detail: keycode,
                        sequence: 0,
                        time,
                        root,
                        event,
                        child,
                        root_x,
                        root_y,
                        event_x,
                        event_y,
                        state: modifier_keys,
                        same_screen,
                    };
                    handler.conn.send_event(
                        /* propagate = */ false,
                        window,
                        x11rb::protocol::xproto::EventMask::STRUCTURE_NOTIFY,
                        &event,
                    )?;
                } else {
                    let event = KeyReleaseEvent {
                        response_type: KEY_RELEASE_EVENT,
                        detail: keycode,
                        sequence: 0,
                        time,
                        root,
                        event,
                        child,
                        root_x,
                        root_y,
                        event_x,
                        event_y,
                        state: modifier_keys,
                        same_screen,
                    };
                    handler.conn.send_event(
                        /* propagate = */ false,
                        window,
                        x11rb::protocol::xproto::EventMask::STRUCTURE_NOTIFY,
                        &event,
                    )?;
                };
                Ok(())
            };

        match self {
            GameAction::UseTool => {
                send_event(true, Self::KEY_C)?;
                send_event(false, Self::KEY_C)?;
            }
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
        }

        Ok(())
    }
}

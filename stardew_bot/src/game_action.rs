use std::collections::HashSet;

use x11rb::protocol::xproto::{
    ConnectionExt as _, KeyPressEvent, KeyReleaseEvent, KEY_PRESS_EVENT,
    KEY_RELEASE_EVENT,
};

use crate::{Error, X11Error, X11Handler};

#[derive(Default)]
pub struct InputState {
    pressed_keys: HashSet<x11rb::protocol::xproto::Keycode>,
}

pub enum GameAction {
    UseTool,
    HoldTool,
    ReleaseTool,
}

impl GameAction {
    pub fn apply(
        &self,
        state: &mut InputState,
        handler: &mut X11Handler,
        window: x11rb::protocol::xproto::Window,
    ) -> Result<(), Error> {
        // TODO: Actually look up the Keycode using xkbcommon
        //
        // Example:
        // https://github.com/rust-x-bindings/toy_xcb/blob/master/src/keyboard.rs
        let keycode: x11rb::protocol::xproto::Keycode = 54;
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

        let mut send_event = |press: bool| -> Result<(), X11Error> {
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
                send_event(true)?;
                send_event(false)?;
            }
            GameAction::HoldTool => {
                send_event(true)?;
            }
            GameAction::ReleaseTool => {
                send_event(false)?;
            }
        }

        Ok(())
    }
}

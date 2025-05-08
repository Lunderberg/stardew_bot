use thiserror::Error;
use x11rb::{
    connection::Connection as _,
    protocol::{
        xproto::{
            AtomEnum, ButtonIndex, ConnectionExt as _,
            EventMask as X11EventMask, KeyPressEvent, Keycode as X11KeyCode,
            Window, BUTTON_PRESS_EVENT, BUTTON_RELEASE_EVENT, KEY_PRESS_EVENT,
            KEY_RELEASE_EVENT, MOTION_NOTIFY_EVENT,
        },
        xtest::ConnectionExt as _,
    },
    rust_connection::RustConnection,
    x11_utils::X11Error,
};

use crate::game_state::Vector;

#[derive(Error)]
pub enum Error {
    #[error("X11 utility error: {0:?}")]
    UtilityError(x11rb::x11_utils::X11Error),

    #[error("X11 connection error: {0}")]
    ConnectionError(#[from] x11rb::errors::ConnectionError),

    #[error("X11 connect error: {0}")]
    ConnectError(#[from] x11rb::errors::ConnectError),

    #[error("X11 error: {0}")]
    X11Error(#[from] x11rb::errors::ReplyError),

    #[error("No root X11 window found")]
    MissingRootWindow,

    #[error("TryFromSliceError {0}")]
    TryFromSliceError(#[from] std::array::TryFromSliceError),

    #[error("Could not find window named {0}")]
    NoSuchWindow(String),
}
impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl From<x11rb::x11_utils::X11Error> for Error {
    fn from(err: x11rb::x11_utils::X11Error) -> Self {
        Error::UtilityError(err)
    }
}

pub struct X11Handler {
    pub(crate) conn: RustConnection,
    name_atoms: [x11rb::protocol::xproto::Atom; 2],
    pid_atom: x11rb::protocol::xproto::Atom,
    active_window_atom: x11rb::protocol::xproto::Atom,
    root: Window,
    main_window: Option<Window>,
    main_window_pos: Option<Vector<isize>>,
}

impl X11Handler {
    pub fn new() -> Result<Self, Error> {
        let (conn, _screen) = x11rb::connect(None)?;

        let get_atom =
            |name: &str| -> Result<x11rb::protocol::xproto::Atom, Error> {
                let cookie = conn.intern_atom(false, name.as_bytes())?;
                let reply = cookie.reply()?;
                Ok(reply.atom)
            };

        let net_wm_name = get_atom("_NET_WM_NAME")?;
        let name_atoms = [net_wm_name, AtomEnum::WM_NAME.into()];

        let pid_atom = get_atom("_NET_WM_PID")?;
        let active_window_atom = get_atom("_NET_ACTIVE_WINDOW")?;

        let root = conn
            .setup()
            .roots
            .iter()
            .next()
            .ok_or(Error::MissingRootWindow)?
            .root;

        let handler = Self {
            conn,
            name_atoms,
            pid_atom,
            active_window_atom,
            root,
            main_window: None,
            main_window_pos: None,
        };

        Ok(handler)
    }

    pub(crate) fn query_active_window(&self) -> Result<Window, Error> {
        let cookie = self.conn.get_property(
            false,
            self.root,
            self.active_window_atom,
            AtomEnum::WINDOW,
            /* long_offset = */ 0,
            /* long_length = */ 1,
        )?;
        let reply = cookie.reply()?;

        let window = Window::from_ne_bytes(reply.value.as_slice().try_into()?);
        Ok(window)
    }

    pub fn main_window_is_active(&self) -> Result<bool, Error> {
        let active_window = self.query_active_window()?;
        Ok(Some(active_window) == self.main_window)
    }

    pub fn get_title_blocking(&self, window: Window) -> Result<String, Error> {
        let property = self
            .name_atoms
            .iter()
            .cloned()
            .find(|&property| property != AtomEnum::NONE.into())
            .expect("Will always include x::ATOM_WM_CLASS");

        let cookie = self.conn.get_property(
            false,
            window,
            property,
            AtomEnum::ANY,
            /* long_offset = */ 0,
            /* long_length = */ 1024,
        )?;
        let reply = cookie.reply()?;
        let title = String::from_utf8_lossy(&reply.value).into_owned();

        Ok(title)
    }

    pub fn get_pid_blocking(&self, window: Window) -> Result<u32, Error> {
        let cookie = self.conn.get_property(
            false,
            window,
            self.pid_atom,
            AtomEnum::ANY,
            /* long_offset = */ 0,
            /* long_length = */ 1,
        )?;
        let reply = cookie.reply()?;
        let pid = u32::from_ne_bytes(reply.value.as_slice().try_into()?);

        Ok(pid)
    }

    pub fn find_window_blocking(&self, title: &str) -> Result<Window, Error> {
        let mut stack: Vec<Window> = self
            .conn
            .setup()
            .roots
            .iter()
            .map(|screen| screen.root)
            .collect();

        while let Some(window) = stack.pop() {
            if self.get_title_blocking(window)?.starts_with(title) {
                return Ok(window);
            }

            let cookie = self.conn.query_tree(window)?;
            let reply = cookie.reply()?;
            let children = &reply.children;
            for child in children {
                stack.push(*child);
            }
        }

        Err(Error::NoSuchWindow(title.to_string()))
    }

    pub fn set_main_window(&mut self, window: Window) {
        self.main_window = Some(window);
    }

    pub fn update_window_location(&mut self) -> Result<(), Error> {
        let window = self
            .main_window
            .expect("Should set the main window before updating its location");

        let query_pointer = self.conn.query_pointer(window)?.reply()?;

        let window_location = Vector::<isize>::new(
            (query_pointer.root_x - query_pointer.win_x) as isize,
            (query_pointer.root_y - query_pointer.win_y) as isize,
        );

        self.main_window_pos = Some(window_location);

        Ok(())
    }

    pub fn send_keystroke(
        &self,
        press: bool,
        keycode: X11KeyCode,
    ) -> Result<(), Error> {
        let window = self
            .main_window
            .expect("Must set main window before sending keystrokes");

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
            root: self.root,
            event: window,
            child: window,
            root_x: 0,
            root_y: 0,
            event_x: 0,
            event_y: 0,
            state: x11rb::protocol::xproto::KeyButMask::default(),
            same_screen: true,
        };
        self.conn.send_event(
            /* propagate = */ false,
            window,
            X11EventMask::STRUCTURE_NOTIFY,
            &event,
        )?;

        Ok(())
    }

    pub fn move_mouse(
        &self,
        window_pixel: Vector<isize>,
    ) -> Result<(), X11Error> {
        let offset = self.main_window_pos.expect(
            "Must set main window position \
             before moving the mouse",
        );
        let global_pixel = window_pixel + offset;

        self.conn
            .xtest_fake_input(
                MOTION_NOTIFY_EVENT,
                0,
                0,
                self.root,
                global_pixel.right as i16,
                global_pixel.down as i16,
                0,
            )
            .unwrap();

        Ok(())
    }

    pub fn send_click(
        &self,
        press: bool,
        button: ButtonIndex,
    ) -> Result<(), X11Error> {
        let response_type = if press {
            BUTTON_PRESS_EVENT
        } else {
            BUTTON_RELEASE_EVENT
        };

        self.conn
            .xtest_fake_input(
                response_type,
                button.into(),
                0,
                self.root,
                0,
                0,
                0,
            )
            .unwrap();

        Ok(())
    }

    pub fn flush(&self) -> Result<(), Error> {
        self.conn.flush()?;

        Ok(())
    }
}

use thiserror::Error;
use x11rb::{
    connection::Connection as _,
    protocol::xproto::{AtomEnum, ConnectionExt as _, Window},
    rust_connection::RustConnection,
};

#[derive(Error)]
pub enum Error {
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

pub struct X11Handler {
    pub(crate) conn: RustConnection,
    name_atoms: [x11rb::protocol::xproto::Atom; 2],
    pid_atom: x11rb::protocol::xproto::Atom,
    active_window_atom: x11rb::protocol::xproto::Atom,
}

impl X11Handler {
    pub fn new() -> Result<Self, Error> {
        let (conn, _screen) = x11rb::connect(None)?;

        // let setup = conn.setup();
        // let root: x11rb::protocol::xproto::Window = setup
        //     .roots
        //     .iter()
        //     .next()
        //     .ok_or(Error::MissingRootWindow)?
        //     .root;

        // println!("Root window: {root:?}");

        // let cookie = conn.query_tree(root)?;
        // let reply = cookie.reply()?;

        // let root = reply.root;
        // let parent = reply.parent;
        // let children = reply.children;

        // println!("Root: {root:?}");
        // println!("Parent: {parent:?}");
        // println!("Children: {children:?}");

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

        // let active_window = {
        //     let cookie = conn.get_property(
        //         false,
        //         root,
        //         active_window_atom,
        //         AtomEnum::WINDOW,
        //         /*long_offset = */ 0,
        //         /*long_length = */ 1,
        //     )?;
        //     let reply = cookie.reply()?;
        //     Window::from_ne_bytes(reply.value.as_slice().try_into()?)
        // };

        let handler = Self {
            conn,
            name_atoms,
            pid_atom,
            active_window_atom,
        };

        // // for child in children {
        // //     let title = handler.get_title_blocking(*child)?;
        // //     let pid = handler.get_pid_blocking(*child)?;
        // //     println!("Child: {child:?}, PID {pid}, {title}");
        // }

        // {
        //     let title = handler.get_title_blocking(active_window)?;
        //     let pid = handler.get_pid_blocking(active_window)?;
        //     println!("Active window: {active_window:?}, PID {pid}, {title}");
        // }

        // let sd_window = handler.find_window_blocking("Stardew Valley")?;
        // {
        //     let title = handler.get_title_blocking(sd_window)?;
        //     let pid = handler.get_pid_blocking(sd_window)?;
        //     println!("SD window: {sd_window:?}, PID {pid}, {title}");
        // }

        Ok(handler)
    }

    pub(crate) fn get_root(
        &self,
    ) -> Result<x11rb::protocol::xproto::Window, Error> {
        Ok(self
            .conn
            .setup()
            .roots
            .iter()
            .next()
            .ok_or(Error::MissingRootWindow)?
            .root)
    }

    pub(crate) fn query_active_window(&self) -> Result<Window, Error> {
        let root = self.get_root()?;
        let cookie = self.conn.get_property(
            false,
            root,
            self.active_window_atom,
            AtomEnum::WINDOW,
            /* long_offset = */ 0,
            /* long_length = */ 1,
        )?;
        let reply = cookie.reply()?;

        let window = Window::from_ne_bytes(reply.value.as_slice().try_into()?);
        Ok(window)
    }

    pub fn get_title_blocking(
        &self,
        window: x11rb::protocol::xproto::Window,
    ) -> Result<String, Error> {
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

    pub fn get_pid_blocking(
        &self,
        window: x11rb::protocol::xproto::Window,
    ) -> Result<u32, Error> {
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

    pub fn find_window_blocking(
        &self,
        title: &str,
    ) -> Result<x11rb::protocol::xproto::Window, Error> {
        let mut stack: Vec<x11rb::protocol::xproto::Window> = self
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
}

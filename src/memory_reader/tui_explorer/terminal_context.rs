use std::{
    fmt::Write,
    panic::PanicInfo,
    sync::{Arc, Mutex},
};

use crate::memory_reader::{Error, Result};

use ratatui::{backend, Terminal};

pub struct TerminalContext {
    terminal: Terminal<backend::CrosstermBackend<std::io::Stdout>>,
    panic_info: Arc<Mutex<Option<String>>>,
}

fn make_panic_hook() -> (
    Arc<Mutex<Option<String>>>,
    impl Fn(&PanicInfo<'_>) + 'static + Sync + Send,
) {
    let message_arc: Arc<Mutex<Option<String>>> = Arc::new(Mutex::new(None));
    let message_output = message_arc.clone();

    let callback = move |info: &PanicInfo| {
        let payload = info.payload();

        // Unfortunately, capturing the output of the default hook is
        // behind the `internal_output_capture` feature, and not
        // available for use.  Therefore, replicating the output of
        // the default hook (`std/src/panicking.rs::default_hook`).
        let payload = None
            .or_else(|| payload.downcast_ref::<&'static str>().map(|s| *s))
            .or_else(|| payload.downcast_ref::<String>().map(|s| &s[..]))
            .unwrap_or("Box<dyn Any>");

        let location = info.location().unwrap();
        let thread = std::thread::current();
        let thread_name = thread.name().unwrap_or("<unnamed>");

        let mut message = String::new();

        let _ = writeln!(
            message,
            "thread '{thread_name}' panicked at {location}:\n\
             {payload}"
        );

        {
            let env =
                std::env::var("RUST_BACKTRACE").unwrap_or("0".to_string());
            match env.as_str() {
                "full" => {
                    let backtrace = std::backtrace::Backtrace::capture();
                    let _ = write!(message, "\n{backtrace:#}");
                }
                "1" => {
                    let backtrace = std::backtrace::Backtrace::capture();
                    let _ = write!(message, "\n{backtrace}");
                }
                _ => {
                    let _ = writeln!(
                        message,
                        "\n\
                         note: run with `RUST_BACKTRACE=1` environment variable \
                         to display a backtrace"
                    );
                }
            }
        }

        let _ = message_arc.lock().unwrap().insert(message);
    };

    return (message_output, callback);
}

impl TerminalContext {
    pub fn new() -> Result<Self> {
        use crossterm::{event, execute, terminal};

        terminal::enable_raw_mode().map_err(|err| Error::TuiIo { err })?;
        let mut stdout = std::io::stdout();
        execute!(
            stdout,
            terminal::EnterAlternateScreen,
            event::EnableMouseCapture,
        )
        .map_err(|err| Error::TuiIo { err })?;
        let backend = backend::CrosstermBackend::new(stdout);
        let terminal =
            Terminal::new(backend).map_err(|err| Error::TuiIo { err })?;

        let (panic_info, panic_hook) = make_panic_hook();
        std::panic::set_hook(Box::new(panic_hook));

        Ok(Self {
            terminal,
            panic_info,
        })
    }
}

impl std::ops::Drop for TerminalContext {
    fn drop(&mut self) {
        use crossterm::{event, execute, terminal};

        terminal::disable_raw_mode().unwrap();
        execute!(
            self.terminal.backend_mut(),
            terminal::LeaveAlternateScreen,
            event::DisableMouseCapture,
        )
        .unwrap();
        self.terminal.show_cursor().unwrap();

        if let Some(message) = self.panic_info.lock().unwrap().as_deref() {
            // No need to call std::panic::resume_unwind, as we
            // haven't actually caught the unwinding.  By storing the
            // message and printing it here in `Drop`, it gets printed
            // to the regular terminal screen instead of the alternate
            // screen.
            print!("{message}");
        }
    }
}

impl std::ops::Deref for TerminalContext {
    type Target = Terminal<backend::CrosstermBackend<std::io::Stdout>>;

    fn deref(&self) -> &Self::Target {
        &self.terminal
    }
}

impl std::ops::DerefMut for TerminalContext {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.terminal
    }
}

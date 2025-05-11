use std::{
    fmt::Write,
    panic::PanicHookInfo,
    sync::{Arc, Mutex},
};

use crate::Error;

use ratatui::{backend, Terminal};

pub struct TerminalContext {
    terminal: Terminal<backend::CrosstermBackend<std::io::Stdout>>,
    panic_info: Arc<Mutex<Option<String>>>,
    is_disposed: bool,
}

fn make_panic_hook() -> (
    Arc<Mutex<Option<String>>>,
    impl Fn(&PanicHookInfo<'_>) + 'static + Sync + Send,
) {
    let message_arc: Arc<Mutex<Option<String>>> = Arc::new(Mutex::new(None));
    let message_output = message_arc.clone();

    let callback = move |info: &PanicHookInfo| {
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

        let _ = {
            let env =
                std::env::var("RUST_BACKTRACE").unwrap_or("0".to_string());
            match env.as_str() {
                "full" => {
                    let backtrace = std::backtrace::Backtrace::capture();
                    write!(message, "\n{backtrace:#}")
                }
                "1" => {
                    let backtrace = std::backtrace::Backtrace::capture();
                    // TODO: When `backtrace_frames` is stabilized,
                    // filter out stack frames before formatting.  As
                    // it is, a stack trace can be formatted, but not
                    // much else.
                    //
                    // https://github.com/rust-lang/rust/issues/79676

                    let backtrace_str = format!("{backtrace}");
                    let mut backtrace: &str = &backtrace_str;

                    if let Some(end) =
                        backtrace.find("__rust_begin_short_backtrace")
                    {
                        backtrace = &backtrace[..end];
                        let newline = backtrace.rfind('\n').unwrap_or(0);
                        backtrace = &backtrace[..=newline];
                    }

                    // The default formatter includes the column,
                    // which may be different between the stack trace
                    // and the panic location.
                    let location =
                        format!("{}:{}", location.file(), location.line());
                    if let Some(begin) = backtrace.find(&location) {
                        let begin = backtrace[..begin].rfind('\n').unwrap_or(0);
                        let begin = backtrace[..begin]
                            .rfind('\n')
                            .map(|i| i + 1)
                            .unwrap_or(0);
                        backtrace = &backtrace[begin..];
                    }

                    write!(message, "\n{backtrace}")
                }
                _ => {
                    writeln!(
                        message,
                        "\n\
                         note: run with `RUST_BACKTRACE=1` environment variable \
                         to display a backtrace"
                    )
                }
            }
        };

        let _ = message_arc.lock().unwrap().insert(message);
    };

    return (message_output, callback);
}

impl TerminalContext {
    pub fn new() -> Result<Self, Error> {
        use crossterm::{event, execute, terminal};

        terminal::enable_raw_mode()?;
        let mut stdout = std::io::stdout();
        execute!(
            stdout,
            terminal::EnterAlternateScreen,
            event::EnableMouseCapture,
        )?;
        let backend = backend::CrosstermBackend::new(stdout);
        let terminal = Terminal::new(backend)?;

        let (panic_info, panic_hook) = make_panic_hook();
        std::panic::set_hook(Box::new(panic_hook));

        Ok(Self {
            terminal,
            panic_info,
            is_disposed: false,
        })
    }

    pub fn dispose(&mut self) -> Result<(), Error> {
        if !self.is_disposed {
            use crossterm::{event, execute, terminal};

            terminal::disable_raw_mode()?;
            execute!(
                self.terminal.backend_mut(),
                terminal::LeaveAlternateScreen,
                event::DisableMouseCapture,
            )?;
            self.terminal.show_cursor()?;

            self.is_disposed = true;
        }

        Ok(())
    }
}

impl std::ops::Drop for TerminalContext {
    fn drop(&mut self) {
        self.dispose().unwrap();

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

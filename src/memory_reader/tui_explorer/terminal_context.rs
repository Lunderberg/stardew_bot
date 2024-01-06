use crate::memory_reader::{Error, Result};

use ratatui::{backend, Terminal};

pub struct TerminalContext {
    terminal: Terminal<backend::CrosstermBackend<std::io::Stdout>>,
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
        Ok(Self { terminal })
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

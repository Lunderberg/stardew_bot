use crate::memory_reader::{
    Error, MemoryReader, MemoryRegion, Result, SigintHandler,
};

use super::{DetailView, MemoryTable, RunningLog, TerminalContext};

use crossterm::event::Event;
use tui::{
    backend::Backend,
    layout::{Constraint, Direction, Layout},
    Frame,
};

pub struct TuiExplorer {
    // Application state
    _pid: u32,
    _reader: MemoryReader,
    _memory_region: MemoryRegion,
    // Display widgets
    running_log: RunningLog,
    memory_table: MemoryTable,
    detail_view: DetailView,
}

impl TuiExplorer {
    pub fn new(pid: u32) -> Result<Self> {
        let reader = MemoryReader::new(pid)?;
        let memory_region = reader.stack()?.read()?;
        let out = Self {
            running_log: RunningLog::new(100),
            memory_table: MemoryTable::new(memory_region.clone()),
            detail_view: DetailView::new(),
            _pid: pid,
            _reader: reader,
            _memory_region: memory_region,
        };
        Ok(out)
    }

    pub fn run(&mut self) -> Result<()> {
        use crossterm::event;

        let mut context = TerminalContext::new()?;
        let handler = SigintHandler::new();

        while !handler.received() {
            context
                .draw(|frame| self.draw(frame))
                .map_err(|err| Error::TuiIoError { err })?;

            let timeout = std::time::Duration::from_millis(100);
            let poll = event::poll(timeout)
                .map_err(|err| Error::TuiIoError { err })?;

            if poll {
                let event_received =
                    event::read().map_err(|err| Error::TuiIoError { err })?;
                let should_exit = self.update_state(event_received)?;
                if should_exit {
                    break;
                }
            }
        }
        Ok(())
    }

    fn draw<B: Backend>(&mut self, frame: &mut Frame<B>) {
        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .margin(1)
            .constraints(
                [
                    Constraint::Min(25),
                    Constraint::Min(45),
                    Constraint::Percentage(50),
                ]
                .as_ref(),
            )
            .split(frame.size());

        self.running_log.draw(frame, chunks[0]);
        self.memory_table.draw(frame, chunks[1]);
        self.detail_view.draw(frame, chunks[2]);
    }

    fn update_state(&mut self, event: Event) -> Result<bool> {
        use crossterm::event::{KeyCode, KeyModifiers};
        if let Event::Key(key) = event {
            match (key.code, key.modifiers) {
                (KeyCode::Char('c'), KeyModifiers::CONTROL) => return Ok(true),
                (KeyCode::Down, _)
                | (KeyCode::Char('n'), KeyModifiers::CONTROL) => {
                    self.memory_table.move_selection_down();
                    self.update_details();
                }
                (KeyCode::Up, _)
                | (KeyCode::Char('p'), KeyModifiers::CONTROL) => {
                    self.memory_table.move_selection_up();
                    self.update_details();
                }
                (KeyCode::PageUp, _)
                | (KeyCode::Char('v'), KeyModifiers::CONTROL) => {
                    self.memory_table.move_selection_page_up();
                    self.update_details();
                }
                (KeyCode::PageDown, _)
                | (KeyCode::Char('v'), KeyModifiers::ALT) => {
                    self.memory_table.move_selection_page_down();
                    self.update_details();
                }
                (KeyCode::Home, KeyModifiers::CONTROL)
                | (
                    KeyCode::Char('<'),
                    KeyModifiers::ALT | KeyModifiers::SHIFT,
                ) => self.memory_table.move_selection_start(),
                (KeyCode::End, KeyModifiers::CONTROL)
                | (
                    KeyCode::Char('>'),
                    KeyModifiers::ALT | KeyModifiers::SHIFT,
                ) => self.memory_table.move_selection_end(),
                _ => {
                    self.running_log.add_log(format!(
                        "{:?}, {:?}",
                        key.code, key.modifiers
                    ));
                }
            }
        }
        Ok(false)
    }

    fn update_details(&mut self) {
        let selected_value = self.memory_table.selected_value();
        let details = [
            (
                "Location".to_string(),
                format!("{}", selected_value.location),
            ),
            ("Value".to_string(), format!("{}", selected_value.value)),
        ];
        self.detail_view.load_details(details.into_iter());
    }
}

use crate::memory_reader::{
    Error, MemoryReader, MemoryRegion, Pointer, Result, SigintHandler,
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
    reader: MemoryReader,
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
            reader,
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
                    Constraint::Min(48),
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

                (KeyCode::Char('s'), KeyModifiers::CONTROL) => {
                    self.memory_table.search_forward()
                }

                (KeyCode::Char('g'), KeyModifiers::CONTROL)
                    if self.memory_table.search_is_active() =>
                {
                    self.memory_table.cancel_search()
                }

                (KeyCode::Backspace, _)
                    if self.memory_table.search_is_active() =>
                {
                    self.memory_table.backspace_search_character()
                }

                (KeyCode::Char(c), _)
                    if self.memory_table.search_is_active() =>
                {
                    self.memory_table.add_search_character(c)
                }

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
        let selection = self.memory_table.selected_value();
        let value: usize = usize::from_ne_bytes(selection.value);
        let mut details = vec![
            ("Location".to_string(), format!("{}", selection.location)),
            ("Hex Value".to_string(), format!("{:x}", value)),
            ("Dec Value".to_string(), format!("{}", value)),
        ];

        let as_pointer: Pointer = value.into();
        let pointed_region = self.reader.find_containing_region(as_pointer);
        if let Some(pointed_region) = pointed_region {
            use std::path::Path;

            let region_name: String = pointed_region.name.as_ref().map_or_else(
                || "anonymous".to_string(),
                |name: &String| {
                    Path::new(name)
                        .file_name()
                        .unwrap()
                        .to_str()
                        .unwrap()
                        .to_string()
                },
            );

            details.push(("".to_string(), "".to_string()));
            details.push(("Points to: ".to_string(), region_name));
        }

        self.detail_view.load_details(details.into_iter());
    }
}

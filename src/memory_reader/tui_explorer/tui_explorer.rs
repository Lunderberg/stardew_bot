use crate::memory_reader::{
    Error, MemoryReader, Pointer, Result, SigintHandler,
};

use super::{DetailView, MemoryTable, RunningLog, TerminalContext};

use crossterm::event::Event;
use ratatui::{
    layout::{Constraint, Direction, Layout},
    Frame,
};

pub struct TuiExplorer {
    // Application state
    _pid: u32,
    reader: MemoryReader,
    // Display widgets
    running_log: RunningLog,
    memory_table: MemoryTable,
    detail_view: DetailView,
}

impl TuiExplorer {
    pub fn new(pid: u32) -> Result<Self> {
        let reader = MemoryReader::new(pid)?;
        let memory_region = reader.stack()?.read()?;
        let stack_entry_point = memory_region.end();
        let out = Self {
            running_log: RunningLog::new(100),
            memory_table: MemoryTable::new(memory_region, stack_entry_point),
            detail_view: DetailView::new(),
            _pid: pid,
            reader,
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

    fn draw(&mut self, frame: &mut Frame) {
        let h_chunks = Layout::default()
            .direction(Direction::Horizontal)
            .margin(1)
            .constraints(
                [Constraint::Min(60), Constraint::Percentage(40)].as_ref(),
            )
            .split(frame.size());

        let v_chunks = Layout::default()
            .direction(Direction::Vertical)
            .margin(0)
            .constraints(
                [Constraint::Min(20), Constraint::Percentage(30)].as_ref(),
            )
            .split(h_chunks[1]);

        self.memory_table.draw(frame, h_chunks[0]);
        self.detail_view.draw(frame, v_chunks[0]);
        self.running_log.draw(frame, v_chunks[1]);
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

                (KeyCode::Char('r'), KeyModifiers::CONTROL) => {
                    self.memory_table.search_backward()
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

                (KeyCode::Enter, _) => {
                    let selection = self.memory_table.selected_value();
                    let as_pointer: Pointer = selection.value.into();
                    let pointed_map_region =
                        self.reader.find_containing_region(as_pointer);
                    if let Some(pointed_map_region) = pointed_map_region {
                        let pointed_region = pointed_map_region.read();
                        match pointed_region {
                            Ok(region) => {
                                self.memory_table.push_view(region, as_pointer);
                                self.update_details();
                            }
                            Err(_) => self
                                .running_log
                                .add_log("Error reading region".to_string()),
                        }
                    } else {
                        self.running_log.add_log(
                            "Value does not point to any memory region"
                                .to_string(),
                        );
                    }
                }

                (KeyCode::Char('g'), KeyModifiers::CONTROL)
                    if !self.memory_table.search_is_active() =>
                {
                    self.memory_table.pop_view()
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
        use crate::memory_reader::value_unpacker::*;

        let selection = self.memory_table.selected_value();
        let mut details = Vec::new();

        macro_rules! add_details {
            ($name:expr => $formatter:expr) => {
                if let Some(display) = $formatter.format(
                    &self.reader,
                    self.memory_table.current_region(),
                    selection.location,
                ) {
                    details.push(($name, format!("{display}")));
                }
            };

            ($($name:expr => $formatter:expr,)*
            ) => {
                $(
                    add_details!{ $name => $formatter }
                )*
            };
        }

        add_details! {
            "Location" => FormatLocation,
            "Hex Value" => FormatHexValue::<u8>::new(),
            "Dec Value" => FormatDecValue::<u64>::new(),
            "UTF-8" => FormatNullTerminatedString,
            "" => FormatSpacer,
            "Points to" => FormatRegionPointedTo,
            "Offset" => FormatPointerOffset,
        }

        self.detail_view.load_details(details.into_iter());
    }
}

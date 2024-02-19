use crate::extensions::*;
use crate::{Error, SigintHandler};

use memory_reader::{MemoryReader, Pointer, Symbol};

use super::{
    DetailView, MemoryTable, RunningLog, StackFrameTable, TerminalContext,
};

use crossterm::event::Event;
use ratatui::{
    layout::{Constraint, Direction, Layout},
    Frame,
};

pub struct TuiExplorer {
    // Application state
    _pid: u32,
    reader: MemoryReader,
    _symbols: Vec<Symbol>,
    // Display widgets
    stack_frame_table: StackFrameTable,
    running_log: RunningLog,
    memory_table: MemoryTable,
    detail_view: DetailView,
}

impl TuiExplorer {
    pub fn new(pid: u32) -> Result<Self, Error> {
        let reader = MemoryReader::new(pid)?;
        let memory_region = reader.stack()?.read()?;

        let symbols: Vec<Symbol> = reader
            .regions
            .iter()
            .flat_map(Symbol::iter_symbols)
            .collect();

        let bottom_stack_frame = memory_region
            .stack_pointers(reader.libc_address_ranges())
            .map(|p| p.location)
            .next();
        let x86_64_tag = memory_region.rfind_pattern(
            &"x86_64".chars().map(|c| (c as u8)).collect::<Vec<_>>(),
        );
        let stack_entry_point = bottom_stack_frame
            .or(x86_64_tag)
            .unwrap_or_else(|| memory_region.end());

        let detail_view = {
            use super::info_formatter::*;

            DetailView::new(vec![
                Box::new(FormatLocation),
                Box::new(FormatHexValue::<u64>::new()),
                Box::new(FormatDecValue::<u64>::new()),
                Box::new(FormatNullTerminatedString),
                Box::new(FormatSpacer),
                Box::new(FormatRegionPointedTo),
                Box::new(FormatSymbolPointedTo(symbols.clone())),
                Box::new(FormatPointerOffset),
                Box::new(FormatStringPointerWithLength),
                Box::new(FormatStringPointerNullTerminated),
            ])
        };

        let mut out = Self {
            stack_frame_table: StackFrameTable::new(&reader, &memory_region),
            running_log: RunningLog::new(100),
            memory_table: MemoryTable::new(memory_region, stack_entry_point),
            _symbols: symbols,
            detail_view,
            _pid: pid,
            reader,
        };
        out.update_details();

        Ok(out)
    }

    pub fn run(&mut self) -> Result<(), Error> {
        use crossterm::event;

        let mut context = TerminalContext::new()?;
        let handler = SigintHandler::new();

        while !handler.received() {
            context.draw(|frame| self.draw(frame))?;

            let timeout = std::time::Duration::from_millis(100);
            let poll = event::poll(timeout)?;

            if poll {
                let event_received = event::read()?;
                let should_exit = self.update_state(event_received)?;
                if should_exit {
                    break;
                }
            }
        }
        Ok(())
    }

    fn draw(&mut self, frame: &mut Frame) {
        let (stack_detail_column, mem_view, log_view) = Layout::default()
            .direction(Direction::Horizontal)
            .margin(1)
            .constraints([
                Constraint::Percentage(100),
                Constraint::Min(65),
                Constraint::Min(40),
            ])
            .split_tuple(frame.size());

        let (stack_view, detail_view) = Layout::default()
            .direction(Direction::Vertical)
            .margin(0)
            .constraints([Constraint::Min(15), Constraint::Percentage(70)])
            .split_tuple(stack_detail_column);

        self.stack_frame_table.draw(frame, stack_view, &self.reader);
        self.memory_table.draw(frame, mem_view, &self.reader);
        self.detail_view.draw(frame, detail_view);
        self.running_log.draw(frame, log_view);
    }

    fn update_state(&mut self, event: Event) -> Result<bool, Error> {
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
                ) => {
                    self.memory_table.move_selection_start();
                    self.update_details();
                }

                (KeyCode::End, KeyModifiers::CONTROL)
                | (
                    KeyCode::Char('>'),
                    KeyModifiers::ALT | KeyModifiers::SHIFT,
                ) => {
                    self.memory_table.move_selection_end();
                    self.update_details();
                }

                (KeyCode::Char('s'), KeyModifiers::CONTROL) => {
                    self.memory_table.search_forward(&self.reader);
                    self.update_details();
                }

                (KeyCode::Char('r'), KeyModifiers::CONTROL) => {
                    self.memory_table.search_backward(&self.reader);
                    self.update_details();
                }

                (KeyCode::Char('g'), KeyModifiers::CONTROL)
                    if self.memory_table.search_is_active() =>
                {
                    self.memory_table.cancel_search();
                    self.update_details();
                }

                (KeyCode::Backspace, _)
                    if self.memory_table.search_is_active() =>
                {
                    self.memory_table.backspace_search_character();
                    self.update_details();
                }

                (KeyCode::Char(c), _)
                    if self.memory_table.search_is_active() =>
                {
                    self.memory_table.add_search_character(c, &self.reader);
                    self.update_details();
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
                    self.memory_table.pop_view();
                    self.update_details();
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

        // TODO: Move this to a better location?  Should the cursor be
        // able to be moved up/down directly in the stack frames?
        self.stack_frame_table.select_address(selection.location);

        self.detail_view.update_details(
            &self.reader,
            self.memory_table.current_region(),
            selection.location,
        );
    }
}

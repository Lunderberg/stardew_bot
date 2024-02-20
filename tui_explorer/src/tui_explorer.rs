use crate::extensions::*;
use crate::{Error, SigintHandler};

use memory_reader::{MemoryReader, Symbol};
use ratatui::style::Stylize as _;

use super::{
    DetailView, MemoryTable, RunningLog, StackFrameTable, TerminalContext,
};

use crossterm::event::{Event, KeyEvent};
use ratatui::{
    layout::{Constraint, Direction, Layout},
    prelude::Style,
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
    // Display state
    selected_region: SelectableRegion,
    key_sequence: Vec<KeyEvent>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum SelectableRegion {
    MemoryTable,
    Log,
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
            selected_region: SelectableRegion::MemoryTable,
            key_sequence: Vec::new(),
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
                let should_exit = self.handle_event(event_received);
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

        let get_border_style = {
            let normal_border_style = Style::new();
            let selected_border_style = Style::new().light_green();
            let selected_region = self.selected_region;

            move |region: SelectableRegion| {
                if region == selected_region {
                    selected_border_style
                } else {
                    normal_border_style
                }
            }
        };

        self.stack_frame_table.draw(frame, stack_view, &self.reader);
        self.memory_table.draw(
            frame,
            mem_view,
            &self.reader,
            get_border_style(SelectableRegion::MemoryTable),
        );
        self.detail_view.draw(frame, detail_view);
        self.running_log.draw(
            frame,
            log_view,
            get_border_style(SelectableRegion::Log),
        );
    }

    fn handle_event(&mut self, event: Event) -> bool {
        let mut should_exit = false;
        let mut clear_sequence = true;

        use crossterm::event::{KeyCode, KeyModifiers};
        if let Event::Key(key) = event {
            match (key.code, key.modifiers) {
                (KeyCode::Char('c'), KeyModifiers::CONTROL) => {
                    should_exit = true;
                }

                // TODO: Have a better way to handle sequences of
                // keystrokes.
                (KeyCode::Char('x'), KeyModifiers::CONTROL) => {
                    self.key_sequence.push(key);
                    clear_sequence = false;
                }
                (KeyCode::Char('o'), KeyModifiers::NONE)
                    if self.key_sequence.len() == 1 =>
                {
                    self.selected_region = match self.selected_region {
                        SelectableRegion::MemoryTable => SelectableRegion::Log,
                        SelectableRegion::Log => SelectableRegion::MemoryTable,
                    };
                }

                _ => self.memory_table.handle_event(
                    key,
                    &self.reader,
                    &mut self.running_log,
                ),
            }

            self.update_details()
        }

        if clear_sequence {
            self.key_sequence.clear();
        }

        should_exit
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

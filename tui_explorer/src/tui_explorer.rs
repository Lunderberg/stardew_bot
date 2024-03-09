use crate::{extensions::*, KeyBindingMatch, KeySequence};
use crate::{Error, SigintHandler};
use memory_reader::extensions::*;

use memory_reader::{MemoryReader, Symbol};
use ratatui::style::Stylize as _;

use super::{
    DetailView, MemoryTable, RunningLog, StackFrameTable, TerminalContext,
};

use crossterm::event::Event;
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
    should_exit: bool,
    selected_region: SelectableRegion,
    keystrokes: KeySequence,
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

        let symbols: Vec<Symbol> = reader.iter_symbols().collect();

        let bottom_stack_frame = memory_region
            .stack_pointers(&reader)
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
            should_exit: false,
            selected_region: SelectableRegion::MemoryTable,
            keystrokes: KeySequence::default(),
        };
        out.update_details();

        Ok(out)
    }

    pub fn run(&mut self) -> Result<(), Error> {
        use crossterm::event;

        let mut context = TerminalContext::new()?;
        let handler = SigintHandler::new();

        let posix_clone = self
            ._symbols
            .iter()
            .find(|symbol| symbol.name == "clone")
            .map(|symbol| symbol.location.clone())
            .unwrap();

        use memory_reader::{MemoryValue, Pointer};
        self.reader
            .regions
            .iter()
            .filter(|region| {
                region.name.is_none()
                    && !region.is_executable
                    && region.is_readable
                    && region.is_writable
            })
            .flat_map(|region| {
                region
                    .read()
                    .unwrap()
                    .into_iter()
                    .iter_as::<MemoryValue<Pointer>>()
                    .rev()
            })
            .filter(|mem_pointer| posix_clone.contains(&mem_pointer.value))
            .for_each(|mem_pointer| {
                self.running_log.add_log(format!("{mem_pointer}"))
            });

        while !handler.received() && !self.should_exit {
            context.draw(|frame| self.draw(frame))?;

            let timeout = std::time::Duration::from_millis(100);
            let poll = event::poll(timeout)?;

            if poll {
                let event_received = event::read()?;
                self.handle_event(event_received);
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

    fn handle_event(&mut self, event: Event) {
        if let Event::Key(key) = event {
            self.keystrokes.push(key);

            match self.apply_key_binding() {
                KeyBindingMatch::Full => {
                    self.update_details();
                    self.keystrokes.clear();
                }
                KeyBindingMatch::Partial => {}
                KeyBindingMatch::Mismatch => {
                    self.running_log.add_log(format!("{:?}", self.keystrokes));
                    self.keystrokes.clear();
                }
            }
        }
    }

    fn apply_key_binding(&mut self) -> KeyBindingMatch {
        let keystrokes = &self.keystrokes;
        KeyBindingMatch::Mismatch
            .or_try_binding("C-c", keystrokes, || {
                self.should_exit = true;
            })
            .or_try_binding("C-x o", keystrokes, || {
                self.selected_region = match self.selected_region {
                    SelectableRegion::MemoryTable => SelectableRegion::Log,
                    SelectableRegion::Log => SelectableRegion::MemoryTable,
                };
            })
            .or_else(|| match self.selected_region {
                SelectableRegion::MemoryTable => {
                    self.memory_table.apply_key_binding(
                        keystrokes,
                        &self.reader,
                        &mut self.running_log,
                    )
                }
                SelectableRegion::Log => {
                    self.running_log.apply_key_binding(keystrokes)
                }
            })
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

use std::ops::Range;

use crate::{extensions::*, KeyBindingMatch, KeySequence};
use crate::{Error, SigintHandler};
use memory_reader::Pointer;

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
    annotations: Vec<Annotation>,
    // Display state
    should_exit: bool,
    selected_region: SelectableRegion,
    keystrokes: KeySequence,
}

pub struct Annotation {
    pub range: std::ops::Range<Pointer>,
    pub name: String,
    pub value: String,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum SelectableRegion {
    MemoryTable,
    Log,
}

impl TuiExplorer {
    pub fn new(pid: u32) -> Result<Self, Error> {
        let reader = MemoryReader::new(pid)?;
        let stack_memory = reader.stack()?.read()?;

        let symbols: Vec<Symbol> = reader.iter_symbols().collect();

        let stack_frame_table = StackFrameTable::new(&reader, &stack_memory);

        let detail_view = {
            use super::info_formatter::*;

            DetailView::new(vec![
                Box::new(FormatLocation),
                Box::new(FormatHexValue::<u64>::new()),
                Box::new(FormatDecValue::<u64>::new()),
                Box::new(FormatNullTerminatedString),
                Box::new(FormatUTF16String),
                Box::new(FormatSpacer),
                Box::new(FormatRegionPointedTo),
                Box::new(FormatSymbolPointedTo(symbols.clone())),
                Box::new(FormatPointerOffset),
                Box::new(FormatStringPointerWithLength),
                Box::new(FormatStringPointerNullTerminated),
            ])
        };

        let mut annotations = Vec::new();

        let dll_region = reader
            .find_region(|reg| reg.short_name() == "Stardew Valley.dll")
            .ok_or(memory_reader::Error::MissingMemoryMapSection(
                "Stardew Valley.dll".to_string(),
            ))?
            .read()?;

        let dll_info = dll_unpacker::Unpacker::new(&dll_region);

        dll_info.collect_annotations(
            |range: Range<Pointer>, name: &'static str, value: String| {
                annotations.push(Annotation {
                    range,
                    name: name.to_string(),
                    value,
                });
            },
        )?;

        let memory_table = {
            let start = dll_region.start() + dll_info.offset_so_far;
            MemoryTable::new(dll_region, start)
        };

        // let memory_table = {
        //     let bottom_stack_frame = stack_memory
        //         .stack_pointers(&reader)
        //         .map(|p| p.location)
        //         .next();
        //     let x86_64_tag = stack_memory.rfind_pattern(
        //         &"x86_64".chars().map(|c| (c as u8)).collect::<Vec<_>>(),
        //     );
        //     let stack_entry_point = bottom_stack_frame
        //         .or(x86_64_tag)
        //         .unwrap_or_else(|| stack_memory.end());
        //     MemoryTable::new(stack_memory, stack_entry_point)
        // };

        let mut out = Self {
            stack_frame_table,
            running_log: RunningLog::new(100),
            memory_table,
            annotations,
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
                Constraint::Min(60),
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
            &self.annotations,
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
                        &mut self.stack_frame_table,
                    )
                }
                SelectableRegion::Log => self.running_log.apply_key_binding(
                    keystrokes,
                    &self.reader,
                    &mut self.memory_table,
                    &mut self.stack_frame_table,
                ),
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
            &self.annotations,
            selection.location,
        );
    }
}

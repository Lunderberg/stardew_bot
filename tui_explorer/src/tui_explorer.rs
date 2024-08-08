use std::collections::HashSet;
use std::ops::Range;

use crate::{
    extensions::*, ColumnFormatter, InfoFormatter, KeyBindingMatch, KeySequence,
};
use crate::{Error, SigintHandler};
use itertools::Itertools as _;
use memory_reader::{extensions::*, MemoryValue};
use memory_reader::{MemoryMapRegion, Pointer};

use memory_reader::{MemoryReader, Symbol};
use ratatui::style::Stylize as _;
use stardew_utils::stardew_valley_pid;

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

    /// If true, highlight this annotation in the MemoryTable.  If
    /// false, show the annotation in the DetailView, but do not
    /// highlight.  This allows hierarchical annotations while only
    /// the inner-most annotation is highlighted.
    pub highlight_range: bool,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum SelectableRegion {
    MemoryTable,
    Log,
}

pub struct TuiExplorerBuilder {
    pid: u32,
    reader: MemoryReader,
    symbols: Vec<Symbol>,
    detail_formatters: Vec<Box<dyn InfoFormatter>>,
    column_formatters: Vec<Box<dyn ColumnFormatter>>,
    initial_pointer: Pointer,
    annotations: Vec<Annotation>,
}

fn stardew_valley_dll(
    reader: &MemoryReader,
) -> Result<&MemoryMapRegion, Error> {
    reader
        .find_region(|reg| reg.short_name() == "Stardew Valley.dll")
        .ok_or(memory_reader::Error::MissingMemoryMapSection(
            "Stardew Valley.dll".to_string(),
        ))
        .map_err(Into::into)
}

impl TuiExplorerBuilder {
    pub fn new() -> Result<Self, Error> {
        let pid = stardew_valley_pid()?;
        Ok(Self {
            pid,
            reader: MemoryReader::new(pid)?,
            symbols: Vec::new(),
            detail_formatters: Vec::new(),
            column_formatters: Vec::new(),
            initial_pointer: Pointer::null(),
            annotations: Vec::new(),
        })
    }

    pub fn init_symbols(self) -> Self {
        Self {
            symbols: self.reader.iter_symbols().collect(),
            ..self
        }
    }

    pub fn default_detail_formatters(self) -> Self {
        use super::info_formatter::*;
        Self {
            detail_formatters: vec![
                Box::new(FormatLocation),
                Box::new(FormatHexValue::<u64>::new()),
                Box::new(FormatDecValue::<u64>::new()),
                Box::new(FormatNullTerminatedString),
                Box::new(FormatUTF16String),
                Box::new(FormatSymbolContainingCursor(self.symbols.clone())),
                Box::new(FormatSpacer),
                Box::new(FormatRegionPointedTo),
                Box::new(FormatSymbolPointedTo(self.symbols.clone())),
                Box::new(FormatPointerOffset),
                Box::new(FormatStringPointerWithLength),
                Box::new(FormatStringPointerNullTerminated),
            ],
            ..self
        }
    }

    pub fn default_column_formatters(self) -> Self {
        use super::column_formatter::*;
        Self {
            column_formatters: vec![
                Box::new(AddressColumn),
                Box::new(HexColumn),
                Box::new(AsciiColumn),
                Box::new(PointsToColumn),
            ],
            ..self
        }
    }

    fn stardew_valley_dll(&self) -> Result<&MemoryMapRegion, Error> {
        stardew_valley_dll(&self.reader)
    }

    pub fn initialize_view_to_stardew_dll(self) -> Result<Self, Error> {
        let region = self.stardew_valley_dll()?.read()?;
        let dll_info = dll_unpacker::Unpacker::new(&region);

        Ok(Self {
            initial_pointer: dll_info.unpacked_so_far()?,
            ..self
        })
    }

    pub fn initialize_view_to_stack(self) -> Result<Self, Error> {
        let stack_memory = self.reader.stack()?.read()?;

        let bottom_stack_frame = stack_memory
            .stack_pointers(&self.reader)
            .map(|p| p.location)
            .next();
        let x86_64_tag = stack_memory.rfind_pattern(
            &"x86_64".chars().map(|c| (c as u8)).collect::<Vec<_>>(),
        );
        let stack_entry_point = bottom_stack_frame
            .or(x86_64_tag)
            .unwrap_or_else(|| stack_memory.end());

        Ok(Self {
            initial_pointer: stack_entry_point,
            ..self
        })
    }

    pub fn initialize_annotations(self) -> Result<Self, Error> {
        let region = self.stardew_valley_dll()?.read()?;
        let dll_info = dll_unpacker::Unpacker::new(&region);

        let mut annotator = DLLAnnotator {
            annotations: Vec::new(),
        };

        dll_info.collect_annotations(&mut annotator)?;

        let mut annotations = annotator.annotations;

        annotations.sort_by_key(|ann| {
            (ann.highlight_range, ann.range.end, ann.range.start)
        });

        return Ok(Self {
            annotations,
            ..self
        });

        struct DLLAnnotator {
            annotations: Vec<Annotation>,
        }

        impl dll_unpacker::Annotator for DLLAnnotator {
            fn range(
                &mut self,
                range: Range<Pointer>,
            ) -> &mut impl dll_unpacker::Annotation {
                self.annotations.push(Annotation {
                    range,
                    name: String::default(),
                    value: String::default(),
                    highlight_range: true,
                });
                self.annotations.last_mut().unwrap()
            }
        }

        impl dll_unpacker::Annotation for Annotation {
            fn name(&mut self, name: impl Into<String>) -> &mut Self {
                self.name = name.into();
                self
            }

            fn current_value(&self) -> &str {
                self.value.as_str()
            }

            fn value(&mut self, value: impl std::fmt::Display) -> &mut Self {
                self.value = format!("{value}");
                self
            }

            fn disable_highlight(&mut self) -> &mut Self {
                self.highlight_range = false;
                self
            }
        }
    }

    pub fn build(self) -> Result<TuiExplorer, Error> {
        let reader = MemoryReader::new(self.pid)?;
        let stack_memory = reader.stack()?.read()?;

        let stack_frame_table = StackFrameTable::new(&reader, &stack_memory);

        let detail_view = DetailView::new(self.detail_formatters);

        let memory_table = MemoryTable::new(
            &self.reader,
            self.initial_pointer,
            self.column_formatters,
        )?;

        let mut out = TuiExplorer {
            stack_frame_table,
            running_log: RunningLog::new(100),
            memory_table,
            annotations: self.annotations,
            _symbols: self.symbols,
            detail_view,
            _pid: self.pid,
            reader,
            should_exit: false,
            selected_region: SelectableRegion::MemoryTable,
            keystrokes: KeySequence::default(),
        };
        out.update_details();

        Ok(out)
    }
}

impl TuiExplorer {
    pub fn new() -> Result<Self, Error> {
        TuiExplorerBuilder::new()?
            .init_symbols()
            .default_detail_formatters()
            .default_column_formatters()
            .initialize_view_to_stardew_dll()?
            //.initialize_view_to_stack()?
            .initialize_annotations()?
            .build()
    }

    pub fn run(&mut self) -> Result<(), Error> {
        use crossterm::event;

        let mut context = TerminalContext::new()?;
        let handler = SigintHandler::new();

        // let stardew_dll_regions: Vec<_> = self
        //     .reader
        //     .regions
        //     .iter()
        //     .filter(|region| {
        //         region.short_name() == "Stardew Valley.dll"
        //             || region.short_name() == "StardewValley.Gamedata.dll"
        //     })
        //     .map(|region| region.address_range())
        //     .collect();

        // Pointers to IL method definitions in the loaded DLL.
        let dll_method_def: HashSet<Pointer> = {
            let dll_region = stardew_valley_dll(&self.reader)?.read()?;

            let dll_info = dll_unpacker::Unpacker::new(&dll_region);
            dll_info
                .metadata_tables()?
                .method_def_table()?
                .iter_rows()
                .filter_map(|method_def| method_def.cil_method().ok().flatten())
                .filter_map(|cil_method| cil_method.body_range().ok())
                .map(|method_body| method_body.start)
                .collect()
        };

        self.running_log.add_log(format!(
            "Found {} method definitions in Stardew Valley.dll",
            dll_method_def.len()
        ));

        let potential_module_ptr = self
            .reader
            .regions
            .iter()
            .filter(|region| {
                region.is_readable
                    && region.is_writable
                    && !region.is_shared_memory
            })
            .flat_map(|region| {
                region
                    .read()
                    .unwrap()
                    .into_iter_as_pointers()
                    .map(|mem_value| mem_value.value)
                    .tuple_windows()
            })
            .filter(|(_, il_ptr)| dll_method_def.contains(il_ptr))
            .map(|(module_ptr, _)| module_ptr)
            .filter(|module_ptr| {
                self.reader
                    .regions
                    .iter()
                    .map(|region| region.address_range())
                    .any(|range| range.contains(module_ptr))
            })
            .counts();

        self.running_log.add_log(format!(
            "Found {} potential pointers to the Module",
            potential_module_ptr.len()
        ));

        // The pattern-matching above is pretty reliable at finding
        // the most common location.  With about 17k methods in
        // StardewValley.dll, there were 243 unique pairs of
        // `(pointer, pointer_to_module_def)`, one of which occurred
        // over 500 times.  No other pair occurred more than 3 times.
        //
        // I suppose I could further filter the `module_ptr` by values
        // that point to a pointer, which itself points to
        // `libcoreclr.so`, but that doesn't seem necessary at the
        // moment.
        let module_ptr = potential_module_ptr
            .iter()
            .max_by_key(|(_, counts)| std::cmp::Reverse(*counts))
            .map(|(value, _)| value)
            .ok_or(Error::ModulePointerNodeFound)?;

        potential_module_ptr
            .into_iter()
            .sorted_by_key(|(_, counts)| std::cmp::Reverse(*counts))
            .take(5)
            .rev()
            .for_each(|(addr, counts)| {
                self.running_log.add_log(format!(
                    "Found {counts} candidates pointing to {addr}"
                ));
            });

        let stardew_dll_regions: Vec<(String, Range<Pointer>)> = {
            let region = stardew_valley_dll(&self.reader)?.read()?;

            let dll_info = dll_unpacker::Unpacker::new(&region);
            let metadata = dll_info.metadata()?;
            let metadata_tables = dll_info.metadata_tables()?;

            let pe_sections = dll_info.iter_section_header()?.map(
                |section| -> (String, Range<Pointer>) {
                    let name = section.name().unwrap().value();
                    let range = section.section_range().unwrap();
                    (name.into(), range)
                },
            );

            let data_dirs = dll_info.iter_data_directories()?.map(
                |(kind, bytes)| -> (_, Range<Pointer>) {
                    (format!("{kind:?}"), bytes.into())
                },
            );

            let streams = metadata.iter_stream_header()?.filter_map(
                |stream| -> Option<(String, Range<Pointer>)> {
                    let stream = stream.ok()?;
                    Some((stream.name.value().into(), stream.bytes.into()))
                },
            );

            let metadata =
                [("Metadata tables".to_string(), metadata_tables.ptr_range())];

            let method_defs = metadata_tables
                .method_def_table()?
                .iter_rows()
                .filter_map(|method_def| method_def.cil_method().ok().flatten())
                .filter_map(|cil_method| cil_method.method_range().ok())
                .peekable()
                .batching(|iter| {
                    let mut range = iter.next()?;
                    while let Some(next) =
                        iter.next_if(|peek| peek.start - range.end < 4)
                    {
                        range = range.start..next.end;
                    }
                    Some(range)
                })
                .map(|range| ("CIL method".to_string(), range));

            let game_obj_methods = metadata_tables
                .type_def_table()?
                .iter_rows()
                .find(|type_def| {
                    type_def
                        .name()
                        .map(|name| name.value() == "Game1")
                        .unwrap_or(false)
                })
                .expect("Could not find 'Game1' class (top-level Stardew info)")
                .iter_methods()?
                // .enumerate()
                // .inspect(|(i, method_def)| {
                //     if *i < 5 {
                //         self.running_log.add_log(format!(
                //             "Game1.{}",
                //             method_def.name().unwrap().value()
                //         ));
                //     }
                // })
                // .map(|(_, method_def)| method_def)
                // .collect::<Vec<_>>()
                // .into_iter()
                .filter_map(|method_def| method_def.cil_method().ok().flatten())
                .filter_map(|cil_method| cil_method.method_range().ok())
                .peekable()
                .batching(|iter| {
                    let mut range = iter.next()?;
                    while let Some(next) =
                        iter.next_if(|peek| peek.start - range.end < 4)
                    {
                        range = range.start..next.end;
                    }
                    Some(range)
                })
                .map(|range| ("Game1 methods".to_string(), range));

            std::iter::empty()
                // .chain(pe_sections)
                // .chain(data_dirs)
                // .chain(streams)
                // .chain(metadata)
                // .chain(method_defs)
                // .chain(game_obj_methods)
                .collect::<Vec<_>>()
                .into_iter()
                .rev()
                .collect()
        };

        self.reader
            .regions
            .iter()
            .filter(|region| {
                region.is_readable
                    && region.is_writable
                    && !region.is_shared_memory
            })
            .flat_map(|region| {
                region
                    .read()
                    .unwrap()
                    .into_iter()
                    .iter_as::<MemoryValue<Pointer>>()
                    .rev()
            })
            .filter_map(|mem_pointer| {
                stardew_dll_regions
                    .iter()
                    .find(|(_, range)| range.contains(&mem_pointer.value))
                    .map(|(name, _)| (name, mem_pointer))
            })
            // .filter(|(name, _)| name.as_str() == ".text")
            // .filter(|(name, _)| name.as_str() == "Metadata tables")
            .filter(|(name, _)| name.as_str() == "Game1 methods")
            .map(|(_, mem_pointer)| -> (Pointer, Pointer) {
                (mem_pointer.value, mem_pointer.location)
            })
            .into_group_map()
            .into_iter()
            .sorted_by_key(|(_, sources)| std::cmp::Reverse(sources.len()))
            .take(100)
            .rev()
            .for_each(|(addr, sources)| {
                let num_sources = sources.len();
                sources.iter().take(5).for_each(|source| {
                    self.running_log.add_log(format!("Pointer from {source}"));
                });
                self.running_log
                    .add_log(format!("Ptr to {addr}: {num_sources}"));
            });

        self.reader
            .regions
            .iter()
            .filter(|region| {
                region.is_readable
                    && region.is_writable
                    && !region.is_shared_memory
            })
            .flat_map(|region| {
                region
                    .read()
                    .unwrap()
                    .into_iter()
                    .iter_as::<MemoryValue<Pointer>>()
            })
            .filter_map(|mem_pointer| {
                stardew_dll_regions
                    .iter()
                    .find(|(_, range)| range.contains(&mem_pointer.value))
                    .map(|(name, _)| name)
            })
            .counts()
            .into_iter()
            .sorted_by_key(|(_, counts)| *counts)
            .for_each(|(name, counts)| {
                self.running_log
                    .add_log(format!("Num ptr to {name}: {counts}"));
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

    pub fn draw(&mut self, frame: &mut Frame) {
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

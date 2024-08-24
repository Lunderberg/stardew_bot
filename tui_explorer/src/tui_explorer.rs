use std::collections::{HashMap, HashSet};
use std::ops::Range;

use itertools::Itertools as _;
use ratatui::style::Stylize as _;

use memory_reader::{MemoryMapRegion, MemoryReader, Pointer, Symbol};
use stardew_utils::stardew_valley_pid;

use crate::extensions::*;
use crate::{
    ColumnFormatter, Error, InfoFormatter, KeyBindingMatch, KeySequence,
    SigintHandler,
};

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
    running_log: RunningLog,
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

impl From<Range<Pointer>> for Annotation {
    fn from(range: Range<Pointer>) -> Self {
        Annotation {
            range,
            name: String::default(),
            value: String::default(),
            highlight_range: true,
        }
    }
}

impl dll_unpacker::Annotator for TuiExplorerBuilder {
    fn range(
        &mut self,
        range: Range<Pointer>,
    ) -> &mut impl dll_unpacker::Annotation {
        self.annotations.range(range)
    }
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
            running_log: RunningLog::new(100),
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

    pub fn initialize_view_to_annotation(
        self,
        name: &str,
    ) -> Result<Self, Error> {
        let initial_pointer = self
            .annotations
            .iter()
            .find(|ann| ann.name == name)
            .map(|ann| ann.range.start)
            .ok_or(Error::AnnotationNotFound)?;

        Ok(Self {
            initial_pointer,
            ..self
        })
    }

    pub fn initialize_view_to_stardew_dll(self) -> Result<Self, Error> {
        let region = self.stardew_valley_dll()?.read()?;
        let dll_info = dll_unpacker::DLLUnpacker::new(&region);

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

    pub fn initialize_annotations(mut self) -> Result<Self, Error> {
        let region = self.stardew_valley_dll()?.read()?;
        let dll_info = dll_unpacker::DLLUnpacker::new(&region);

        dll_info.collect_annotations(&mut self)?;

        Ok(self)
    }

    pub fn search_based_on_annotations(mut self) -> Result<Self, Error> {
        use dll_unpacker::{Annotation, Annotator};

        let dll_region = stardew_valley_dll(&self.reader)?.read()?;
        let dll_info = dll_unpacker::DLLUnpacker::new(&dll_region);
        let metadata = dll_info.metadata()?;

        metadata
            .iter_table_locations()
            .filter(|(_, range)| range.start != range.end)
            .for_each(|(kind, range)| {
                self.running_log
                    .add_log(format!("{kind} table: {}", range.start));
            });

        let module_ptr =
            dotnet_debugger::RuntimeModule::locate(&self.reader, &metadata)?;

        self.running_log.add_log(format!(
            "Found module pointer at {module_ptr} for {}",
            dll_region.name()
        ));

        let runtime_module = dotnet_debugger::RuntimeModule::build(
            &self.reader,
            &metadata,
            module_ptr,
        )?;

        self.running_log.add_log(format!(
            "Method table: {}",
            runtime_module.ptr_to_table_of_method_tables
        ));

        self.range(runtime_module.method_table_lookup.location.clone())
            .name("TypeDefToMethodDef table");

        runtime_module
            .iter_method_tables(&self.reader)
            .try_for_each(|table| -> Result<_, Error> {
                let table = table?;

                let class_name =
                    metadata.type_def_table()?.get(table.token())?.name()?;
                self.annotations
                    .range(table.ptr_range())
                    .name(format!("MethodTable, {class_name}"));

                self.annotations
                    .range(
                        runtime_module
                            .method_table_lookup
                            .location_of_method_table_pointer(table.token()),
                    )
                    .name(format!("Ptr to {class_name} MethodTable"));

                table.collect_annotations(&mut self.annotations)?;
                let ee_class = table.get_ee_class(&self.reader)?;

                self.annotations
                    .range(ee_class.ptr_range())
                    .name(format!("EEClass, {class_name}"));
                ee_class.collect_annotations(&mut self.annotations)?;

                let fields = table.get_field_descriptions(&self.reader)?;

                if let Some(fields) = &fields {
                    self.annotations
                        .range(fields.ptr_range())
                        .name(format!("Fields of {class_name}"));
                }

                fields.iter().flatten().enumerate().try_for_each(
                    |(i, field)| -> Result<_, Error> {
                        assert!(
                            field.method_table() == table.ptr_range().start,
                            "Field {i} of class {class_name} \
                             (index = {}) \
                             did not point back to MethodTable",
                            table.token()
                        );
                        let field_name = metadata.get(field.token())?.name()?;
                        self.annotations
                            .range(field.ptr_range())
                            .name(format!("FieldDesc {field_name}"));
                        field.collect_annotations(&mut self.annotations)?;
                        Ok(())
                    },
                )?;

                Ok(())
            })?;

        let game_obj_method_table = runtime_module
            .iter_method_tables(&self.reader)
            .try_find(|method_table| -> Result<_, Error> {
                let type_def = metadata.get(method_table.token())?;
                let name = type_def.name()?;
                Ok(name == "Game1")
            })?
            .ok_or(Error::MethodTableNotFound("Game1"))?;

        let game_obj: Pointer = dotnet_debugger::find_object_instances(
            &game_obj_method_table,
            &self.reader,
        )?
        .exactly_one()
        .map_err(|err| Error::UniqueGameObjectInstanceNotFound(err.count()))?;

        self.running_log
            .add_log(format!("Top-level Game1 object {game_obj}"));

        self.range(game_obj..game_obj + Pointer::SIZE)
            .name("Game object method table");
        self.range(game_obj..game_obj + game_obj_method_table.base_size())
            .name("Top-level game object")
            .disable_highlight();

        game_obj_method_table
            .get_field_descriptions(&self.reader)?
            .iter()
            .flatten()
            .filter(|field| !field.is_static())
            .try_for_each(|field| -> Result<_, Error> {
                let field_metadata = metadata.get(field.token())?;
                let name = field_metadata.name()?;
                let field_start = game_obj + Pointer::SIZE + field.offset();
                let size = field.runtime_type()?.size_bytes();
                self.range(field_start..field_start + size)
                    .name(format!("Field {name}"));
                Ok(())
            })?;

        self.initial_pointer = game_obj;

        // let parent_map: HashMap<_,_> = metadata.type_def_table()?
        //     .iter_rows()
        //     .filter_map(|type_def|{
        //         type_def.extends().unwrap().and_then(|def_or_ref|{
        //             match def_or_ref{
        //                 dll_unpacker::dll_unpacker::MetadataTypeDefOrRef::TypeDef(parent) => Some(parent),
        //                 _ => None,
        //             }
        //         }).map(|parent| {
        //             (type_def.index(), parent.index())
        //         })
        //     })
        //     .collect();
        // println!("Num classes which extend locals: {}", parent_map.len());

        // let classes_with_static: HashSet<_> = metadata
        //     .type_def_table()?
        //     .iter_rows()
        //     .filter(|type_def| {
        //         type_def
        //             .iter_fields()
        //             .unwrap()
        //             .any(|field| field.is_static().unwrap())
        //     })
        //     .map(|type_def| type_def.index())
        //     .collect();
        // println!(
        //     "Num classes with immediate statics: {}",
        //     classes_with_static.len()
        // );

        // let num_classes_with_static = metadata
        //     .type_def_table()?
        //     .iter_rows()
        //     .filter(|type_def| {
        //         std::iter::successors(Some(type_def.index()), |index| {
        //             parent_map.get(index).copied()
        //         })
        //         .any(|index| classes_with_static.contains(&index))
        //     })
        //     .count();
        // println!("Num classes with statics: {num_classes_with_static}");

        // let num_static_fields = metadata
        //     .field_table()?
        //     .iter_rows()
        //     .filter(|field| field.is_static().unwrap())
        //     .count();
        // println!("Num static fields: {num_static_fields}");

        // let num_static_gc_fields = metadata
        //     .field_table()?
        //     .iter_rows()
        //     .filter(|field| field.is_static().unwrap())
        //     .filter(|field| field.is_garbage_collected().unwrap())
        //     .count();
        // println!("Num static GC fields: {num_static_gc_fields}");

        Ok(self)
    }

    pub fn build(self) -> Result<TuiExplorer, Error> {
        let annotations = {
            let mut arr = self.annotations;
            arr.sort_by_key(|ann| {
                (ann.highlight_range, ann.range.end, ann.range.start)
            });
            arr
        };

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
            running_log: self.running_log,
            memory_table,
            annotations,
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
            .initialize_annotations()?
            .search_based_on_annotations()?
            // .initialize_view_to_stardew_dll()?
            //.initialize_view_to_stack()?
            // .initialize_view_to_annotation("#Blob Stream")?
            .initialize_view_to_annotation("Field[100]")?
            .build()
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

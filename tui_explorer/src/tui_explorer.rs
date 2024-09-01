use std::ops::Range;

use dotnet_debugger::{RuntimeObject, TypedPointer};
use memory_reader::{
    MemoryMapRegion, MemoryReader, MemoryRegion, Pointer, Symbol,
};
use stardew_utils::stardew_valley_pid;

use crate::extended_tui::{
    DynamicLayout, WidgetGlobals, WidgetSideEffects, WidgetWindow,
};
use crate::{extensions::*, ObjectExplorer};
use crate::{
    ColumnFormatter, Error, InfoFormatter, KeyBindingMatch, KeySequence,
    SigintHandler,
};

use super::{
    DetailView, MemoryTable, RunningLog, StackFrameTable, TerminalContext,
};

use crossterm::event::Event;
use ratatui::Frame;

pub struct TuiExplorer {
    // Application state
    _pid: u32,
    reader: MemoryReader,
    current_region: MemoryRegion,
    _symbols: Vec<Symbol>,
    // Display state
    layout: DynamicLayout,
    // Display widgets
    stack_frame_table: StackFrameTable,
    running_log: RunningLog,
    memory_table: MemoryTable,
    detail_view: DetailView,
    object_explorer: ObjectExplorer,
    annotations: Vec<Annotation>,
    // Display state
    should_exit: bool,
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

pub struct TuiExplorerBuilder {
    pid: u32,
    reader: MemoryReader,
    symbols: Vec<Symbol>,
    detail_formatters: Vec<Box<dyn InfoFormatter>>,
    column_formatters: Vec<Box<dyn ColumnFormatter>>,
    initial_pointer: Pointer,
    annotations: Vec<Annotation>,
    running_log: RunningLog,
    layout: DynamicLayout,
    top_object: Option<TypedPointer<RuntimeObject>>,
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
            layout: DynamicLayout::new(),
            top_object: None,
        })
    }

    pub fn layout_memory_table(mut self) -> Self {
        self.layout.close_all_other_windows();
        self.layout.split_horizontally(Some(45), None);
        self.layout.switch_to_buffer(0);
        self.layout.split_vertically(Some(15), None);
        self.layout.cycle_next();
        self.layout.switch_to_buffer(1);
        self.layout.cycle_next();
        self.layout.switch_to_buffer(3);
        self.layout.split_horizontally(Some(65), Some(60));
        self.layout.switch_to_buffer(2);
        self
    }

    pub fn layout_object_explorer(mut self) -> Self {
        self.layout.close_all_other_windows();
        self.layout.switch_to_buffer(3);
        self.layout.split_horizontally(None, Some(60));
        self.layout.switch_to_buffer(4);
        self
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

    pub fn initialize_view_to_game_obj(mut self) -> Result<Self, Error> {
        let dll_region = stardew_valley_dll(&self.reader)?.read()?;
        let dll_info = dll_unpacker::DLLUnpacker::new(&dll_region);
        let metadata = dll_info.metadata()?;

        let module_ptr =
            dotnet_debugger::RuntimeModule::locate(&metadata, &self.reader)?;

        let runtime_module = module_ptr.read(&self.reader)?;

        let game_obj_method_table = runtime_module
            .iter_method_tables(&self.reader)
            .try_find(|method_table| -> Result<_, Error> {
                let type_def = metadata.get(method_table.token())?;
                let name = type_def.name()?;
                Ok(name == "Game1")
            })?
            .ok_or(Error::MethodTableNotFound("Game1"))?;

        let game_obj: TypedPointer<RuntimeObject> =
            dotnet_debugger::find_most_likely_object_instance(
                &game_obj_method_table,
                &self.reader,
            )?;

        self.initial_pointer = game_obj.into();
        self.top_object = Some(game_obj);

        Ok(self)
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
            dotnet_debugger::RuntimeModule::locate(&metadata, &self.reader)?;

        self.running_log.add_log(format!(
            "Found module pointer at {module_ptr} for {}",
            dll_region.name()
        ));

        let runtime_module = module_ptr.read(&self.reader)?;

        self.running_log.add_log(format!(
            "Method table: {}",
            runtime_module.ptr_to_table_of_method_tables
        ));

        self.running_log.add_log(format!(
            "Base-ptr of non-GC statics: {}",
            runtime_module.base_ptr_of_non_gc_statics
        ));

        self.running_log.add_log(format!(
            "Base-ptr of GC statics: {}",
            runtime_module.base_ptr_of_gc_statics
        ));

        self.range(runtime_module.method_table_lookup.location.clone())
            .name("TypeDefToMethodDef table");

        runtime_module
            .iter_method_tables(&self.reader)
            .try_for_each(|table| -> Result<_, Error> {
                let table = table?;

                let class_name = metadata.get(table.token())?.name()?;
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

                fields
                    .iter()
                    .flatten()
                    .filter(|field| field.is_static())
                    .try_for_each(|field| -> Result<_, Error> {
                        let field_name = metadata.get(field.token())?.name()?;
                        let location = field.location(&runtime_module, None)?;

                        let ann = self
                            .annotations
                            .range(location.clone())
                            .name(format!("{class_name}.{field_name}"));

                        if !field.is_pointer()? {
                            let bytes = self.reader.read_bytes(
                                location.start,
                                location.end - location.start,
                            )?;
                            let value = field.runtime_type()?.parse(&bytes)?;
                            ann.value(value);
                        }

                        Ok(())
                    })?;

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

        let game_obj: Pointer =
            dotnet_debugger::find_most_likely_object_instance(
                &game_obj_method_table,
                &self.reader,
            )?
            .into();

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
            .filter(|field| {
                // Static fields across all classes in the module are
                // already annotated.
                !field.is_static()
            })
            .try_for_each(|field| -> Result<_, Error> {
                let field_name = metadata.get(field.token())?.name()?;
                let location =
                    field.location(&runtime_module, Some(game_obj))?;

                let ann = self
                    .annotations
                    .range(location.clone())
                    .name(format!(" {field_name}"));

                if !field.is_pointer()? {
                    let bytes = self.reader.read_bytes(
                        location.start,
                        location.end - location.start,
                    )?;
                    let value = field.runtime_type()?.parse(&bytes)?;
                    ann.value(value);
                }
                Ok(())
            })?;

        self.initial_pointer = game_obj;

        Ok(self)
    }

    pub fn build(self) -> Result<TuiExplorer, Error> {
        let reader = self.reader;

        let current_region = reader
            .find_containing_region(self.initial_pointer)
            .ok_or(Error::PointerNotFound(self.initial_pointer))?
            .read()?;

        let annotations = {
            let mut arr = self.annotations;
            arr.sort_by_key(|ann| {
                (ann.highlight_range, ann.range.end, ann.range.start)
            });
            arr
        };

        let globals = WidgetGlobals {
            reader: &reader,
            current_region: &current_region,
            annotations: &annotations,
        };

        let stack_memory = reader.stack()?.read()?;
        let stack_frame_table = StackFrameTable::new(&reader, &stack_memory);
        let detail_view = {
            let mut detail_view = DetailView::new(self.detail_formatters);
            detail_view.update_details(globals, self.initial_pointer);
            detail_view
        };

        let memory_table = MemoryTable::new(
            &reader,
            self.initial_pointer,
            self.column_formatters,
        )?;

        let out = TuiExplorer {
            _pid: self.pid,
            reader,
            current_region,
            _symbols: self.symbols,
            layout: self.layout,
            stack_frame_table,
            running_log: self.running_log,
            memory_table,
            detail_view,
            object_explorer: ObjectExplorer::new(self.top_object),
            annotations,

            should_exit: false,
            keystrokes: KeySequence::default(),
        };

        Ok(out)
    }
}

impl TuiExplorer {
    pub fn new() -> Result<Self, Error> {
        TuiExplorerBuilder::new()?
            // .layout_memory_table()
            .layout_object_explorer()
            .init_symbols()
            .initialize_view_to_stardew_dll()?
            .default_detail_formatters()
            .default_column_formatters()
            .initialize_annotations()?
            .search_based_on_annotations()?
            // .initialize_view_to_stack()?
            // .initialize_view_to_annotation("#Blob Stream")?
            // .initialize_view_to_annotation("Field[100]")?
            .initialize_view_to_game_obj()?
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
            self.periodic_update();
        }
        Ok(())
    }

    pub fn draw(&mut self, frame: &mut Frame) {
        let mut buffers: Vec<Box<&mut dyn WidgetWindow>> = vec![
            Box::new(&mut self.stack_frame_table),
            Box::new(&mut self.detail_view),
            Box::new(&mut self.memory_table),
            Box::new(&mut self.running_log),
            Box::new(&mut self.object_explorer),
        ];
        let layout = self.layout.drawable(
            &mut buffers,
            WidgetGlobals {
                reader: &self.reader,
                current_region: &self.current_region,
                annotations: &self.annotations,
            },
        );

        frame.render_widget(layout, frame.size());
    }

    pub fn handle_event(&mut self, event: Event) {
        if let Err(err) = self.try_handle_event(event) {
            self.running_log.add_log(format!("Error: {err}"));
        }
    }

    fn try_handle_event(&mut self, event: Event) -> Result<(), Error> {
        match event {
            Event::Key(key) => {
                self.keystrokes.push(key);

                match self.apply_key_binding()? {
                    KeyBindingMatch::Full => {
                        self.keystrokes.clear();
                    }
                    KeyBindingMatch::Partial => {}
                    KeyBindingMatch::Mismatch => {
                        return Err(Error::UnknownKeySequence(std::mem::take(
                            &mut self.keystrokes,
                        )));
                    }
                }
            }
            Event::Mouse(mouse) => self.layout.handle_mouse_event(mouse),
            _ => {}
        }

        Ok(())
    }

    fn apply_key_binding(&mut self) -> Result<KeyBindingMatch, Error> {
        let keystrokes = &self.keystrokes;
        let globals = WidgetGlobals {
            reader: &self.reader,
            current_region: &self.current_region,
            annotations: &self.annotations,
        };

        let mut side_effects = WidgetSideEffects::default();

        // TODO: Move the buffers into a separate class, to allow this
        // to be de-duplicated between `apply_key_binding` and `draw`,
        // without running into multiple borrows of `self`.
        let mut buffer_list: Vec<Box<&mut dyn WidgetWindow>> = vec![
            Box::new(&mut self.stack_frame_table),
            Box::new(&mut self.detail_view),
            Box::new(&mut self.memory_table),
            Box::new(&mut self.running_log),
            Box::new(&mut self.object_explorer),
        ];

        let result = KeyBindingMatch::Mismatch
            .or_try_binding("C-c", keystrokes, || {
                self.should_exit = true;
            })
            .or_try_binding("C-x o", keystrokes, || {
                self.layout.cycle_next();
            })
            .or_else(|| {
                // The layout will forward the key bindings to either
                // the active window, or to a buffer selection window
                // if present.
                self.layout.apply_key_binding(
                    &keystrokes,
                    globals,
                    &mut side_effects,
                    &mut buffer_list,
                )
            });

        self.apply_side_effects(side_effects);

        Ok(result)
    }

    pub fn periodic_update(&mut self) {
        let globals = WidgetGlobals {
            reader: &self.reader,
            current_region: &self.current_region,
            annotations: &self.annotations,
        };

        // TODO: Move the buffers into a separate class, to allow this
        // to be de-duplicated between `apply_key_binding`, `update`, and `draw`,
        // without running into multiple borrows of `self`.
        let mut buffer_list: Vec<Box<&mut dyn WidgetWindow>> = vec![
            Box::new(&mut self.stack_frame_table),
            Box::new(&mut self.detail_view),
            Box::new(&mut self.memory_table),
            Box::new(&mut self.running_log),
            Box::new(&mut self.object_explorer),
        ];

        let mut side_effects = WidgetSideEffects::default();

        let res = buffer_list.iter_mut().try_for_each(|buffer| {
            buffer.periodic_update(globals, &mut side_effects)
        });

        if let Err(err) = res {
            self.running_log.add_log(format!("Error: {err}"));
        }

        self.apply_side_effects(side_effects);
    }

    fn apply_side_effects(&mut self, mut side_effects: WidgetSideEffects) {
        if let Some(ptr) = side_effects.change_address {
            if !self.current_region.contains(ptr) {
                let new_region = self
                    .reader
                    .find_containing_region(ptr)
                    .ok_or(Error::PointerNotFound(ptr))
                    .and_then(|region| region.read().map_err(Into::into));

                match new_region {
                    Ok(region) => {
                        self.current_region = region;
                    }
                    Err(err) => {
                        self.running_log.add_log(format!("Error: {err}"));
                        return;
                    }
                }
            }
        }

        let globals = WidgetGlobals {
            reader: &self.reader,
            current_region: &self.current_region,
            annotations: &self.annotations,
        };

        // TODO: Move the buffers into a separate class, to allow this
        // to be de-duplicated between `apply_key_binding` and `draw`,
        // without running into multiple borrows of `self`.
        let mut buffer_list: Vec<Box<&mut dyn WidgetWindow>> = vec![
            Box::new(&mut self.stack_frame_table),
            Box::new(&mut self.detail_view),
            Box::new(&mut self.memory_table),
            Box::new(&mut self.running_log),
            Box::new(&mut self.object_explorer),
        ];

        if let Some(ptr) = side_effects.change_address {
            buffer_list.iter_mut().for_each(|buffer| {
                buffer.change_address(globals, &mut side_effects, ptr)
            });
        }

        // Process the log messages last, since handling other side
        // effects may result in additional log messages.
        for message in side_effects.log_messages {
            self.running_log.add_log(message);
        }
    }
}

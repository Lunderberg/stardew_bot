use std::ops::Range;

use dotnet_debugger::{
    CachedReader, FieldContainer, RuntimeObject, RuntimeType, StaticValueCache,
    TypedPointer,
};
use memory_reader::{
    MemoryMapRegion, MemoryReader, MemoryRegion, Pointer, Symbol,
};
use stardew_utils::stardew_valley_pid;

use crate::extended_tui::{DynamicLayout, WidgetSideEffects, WidgetWindow};
use crate::{
    extensions::*, MetadataDisplay, ObjectExplorer, UserConfig,
    UserConfigEditor,
};
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
    /// Information shared to all sub-windows
    tui_globals: TuiGlobals,

    /// Layout of sub-windows
    layout: DynamicLayout,

    /// Container of subwindows
    buffers: TuiBuffers,

    // Interactions owned by the top-level UI
    should_exit: bool,
    keystrokes: KeySequence,
}

/// Contains information that may be required by more than one widget.
/// A read-only reference is provided to each widget during input
/// handling and rendering.
pub(crate) struct TuiGlobals {
    pub(crate) reader: MemoryReader,
    pub(crate) current_region: MemoryRegion,
    pub(crate) annotations: Vec<Annotation>,
    #[allow(dead_code)]
    pub(crate) symbols: Vec<Symbol>,
    pub(crate) static_value_cache: StaticValueCache,
}

struct TuiBuffers {
    stack_frame_table: StackFrameTable,
    running_log: RunningLog,
    memory_table: MemoryTable,
    detail_view: DetailView,
    object_explorer: ObjectExplorer,
    user_config_editor: UserConfigEditor,
    metadata_display: MetadataDisplay,
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
    tui_globals: TuiGlobals,
    detail_formatters: Vec<Box<dyn InfoFormatter>>,
    column_formatters: Vec<Box<dyn ColumnFormatter>>,
    initial_pointer: Pointer,
    running_log: RunningLog,
    layout: DynamicLayout,
    top_object: Option<TypedPointer<RuntimeObject>>,
    user_config: UserConfig,
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
        self.tui_globals.annotations.range(range)
    }
}

impl TuiExplorerBuilder {
    pub fn new() -> Result<Self, Error> {
        let pid = stardew_valley_pid()?;
        let reader = MemoryReader::new(pid)?;

        // Even if this is overridden later, finding the [stack]
        // memory region is cheap to do, and ensures that we're
        // looking at a valid memory location in the remote process.
        let initial_pointer = reader.stack()?.address_range().start;

        Ok(Self {
            tui_globals: TuiGlobals {
                reader,
                current_region: MemoryRegion::empty(),
                annotations: Vec::new(),
                symbols: Vec::new(),
                static_value_cache: StaticValueCache::new(),
            },

            detail_formatters: Vec::new(),
            column_formatters: Vec::new(),
            initial_pointer,

            running_log: RunningLog::new(100),
            layout: DynamicLayout::new(),
            top_object: None,
            user_config: Default::default(),
        })
    }

    pub fn load_config_from_default_location(mut self) -> Result<Self, Error> {
        self.user_config = UserConfig::load_default()?;
        Ok(self)
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
        self.layout.switch_to_buffer(5);
        self.layout.split_horizontally(None, Some(60));
        self.layout.switch_to_buffer(4);
        self
    }

    pub fn layout_metadata_display(mut self) -> Self {
        self.layout.close_all_other_windows();
        self.layout.switch_to_buffer(3);
        self.layout.split_horizontally(None, Some(60));
        self.layout.switch_to_buffer(6);
        self
    }

    pub fn init_symbols(self) -> Self {
        let symbols = self.tui_globals.reader.iter_symbols().collect();
        Self {
            tui_globals: TuiGlobals {
                symbols,
                ..self.tui_globals
            },
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
                Box::new(FormatSymbolContainingCursor(
                    self.tui_globals.symbols.clone(),
                )),
                Box::new(FormatSpacer),
                Box::new(FormatRegionPointedTo),
                Box::new(FormatSymbolPointedTo(
                    self.tui_globals.symbols.clone(),
                )),
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
        stardew_valley_dll(&self.tui_globals.reader)
    }

    pub fn initialize_view_to_annotation(
        self,
        name: &str,
    ) -> Result<Self, Error> {
        let initial_pointer = self
            .tui_globals
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

    pub fn initialize_view_to_symbol(self, name: &str) -> Result<Self, Error> {
        let initial_pointer = self
            .tui_globals
            .symbols
            .iter()
            .find(|sym| sym.name == name)
            .map(|sym| sym.location.start)
            .ok_or(Error::SymbolNotFound)?;

        Ok(Self {
            initial_pointer,
            ..self
        })
    }

    pub fn initialize_view_to_stardew_dll(self) -> Result<Self, Error> {
        let region = self.stardew_valley_dll()?;

        Ok(Self {
            initial_pointer: region.address_range().start,
            ..self
        })
    }

    pub fn initialize_view_to_region(self, name: &str) -> Result<Self, Error> {
        let region = self
            .tui_globals
            .reader
            .regions
            .iter()
            .filter(|region| region.short_name() == name)
            .filter(|region| region.file_offset() == 0)
            .max_by_key(|region| {
                region.address_range().end - region.address_range().start
            })
            .unwrap();

        Ok(Self {
            initial_pointer: region.address_range().start,
            ..self
        })
    }

    pub fn initialize_view_to_stack(self) -> Result<Self, Error> {
        let stack_memory = self.tui_globals.reader.stack()?.read()?;

        let bottom_stack_frame = stack_memory
            .stack_pointers(&self.tui_globals.reader)
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
        let reader = self.tui_globals.cached_reader();
        let module_ptr = reader.runtime_module_by_name("Stardew Valley")?;
        let runtime_module = reader.runtime_module(module_ptr)?;
        let metadata = runtime_module.metadata(&reader)?;

        let game_obj_method_table = runtime_module
            .iter_method_tables(&self.tui_globals.reader)?
            .try_find(|method_table| -> Result<_, Error> {
                if let Some(type_def) = metadata.get(method_table.token())? {
                    let name = type_def.name()?;
                    Ok(name == "Game1")
                } else {
                    Ok(false)
                }
            })?
            .ok_or(Error::MethodTableNotFound("Game1"))?;

        let game_obj: TypedPointer<RuntimeObject> =
            dotnet_debugger::find_most_likely_object_instance(
                &game_obj_method_table,
                &self.tui_globals.reader,
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

        let reader = self
            .tui_globals
            .static_value_cache
            .cached_reader(&self.tui_globals.reader);
        let module_ptr = reader.runtime_module_by_name("Stardew Valley")?;
        let runtime_module = reader.runtime_module(module_ptr)?;
        let metadata = runtime_module.metadata(&reader)?;

        metadata.iter_heap_locations().for_each(|(kind, range)| {
            self.running_log
                .add_log(format!("{kind} heap: {}", range.start));
        });

        metadata
            .iter_table_locations()
            .filter(|(_, range)| range.start != range.end)
            .for_each(|(kind, range)| {
                self.running_log
                    .add_log(format!("{kind} table: {}", range.start));
            });

        let module_ptr =
            dotnet_debugger::RuntimeModule::locate(metadata, None, &reader)?;

        self.running_log.add_log(format!(
            "Found module pointer at {module_ptr} for {}",
            metadata
                .assembly_table()
                .iter_rows()
                .next()
                .unwrap()
                .name()? // dll_region.name()
        ));

        let runtime_module = module_ptr.read(&self.tui_globals.reader)?;

        self.running_log.add_log(format!(
            "Method table: {}",
            runtime_module
                .ptr_to_table_of_method_tables(&self.tui_globals.reader)?
        ));

        self.running_log.add_log(format!(
            "Base-ptr of non-GC statics: {}",
            runtime_module
                .base_ptr_of_non_gc_statics(&self.tui_globals.reader)?
        ));

        self.running_log.add_log(format!(
            "Base-ptr of GC statics: {}",
            runtime_module.base_ptr_of_gc_statics(&self.tui_globals.reader)?
        ));

        let annotations = &mut self.tui_globals.annotations;

        annotations
            .range(
                runtime_module
                    .method_table_lookup(&self.tui_globals.reader)?
                    .location
                    .clone(),
            )
            .name("TypeDefToMethodDef table");

        let game_obj_method_table = runtime_module
            .iter_method_tables(&self.tui_globals.reader)?
            .try_find(|method_table| -> Result<_, Error> {
                if let Some(type_def) = metadata.get(method_table.token())? {
                    let name = type_def.name()?;
                    Ok(name == "Game1")
                } else {
                    Ok(false)
                }
            })?
            .ok_or(Error::MethodTableNotFound("Game1"))?;

        let game_obj_method_table_ptr = game_obj_method_table.ptr_range().start;

        let game_obj = dotnet_debugger::find_most_likely_object_instance(
            &game_obj_method_table,
            &self.tui_globals.reader,
        )?;

        self.running_log
            .add_log(format!("Top-level Game1 object {game_obj}"));

        {
            let game_obj: Pointer = game_obj.into();
            annotations
                .range(game_obj..game_obj + Pointer::SIZE)
                .name("Game object method table");
            annotations
                .range(game_obj..game_obj + game_obj_method_table.base_size())
                .name("Top-level game object")
                .disable_highlight();
        }

        runtime_module
            .iter_method_tables(&self.tui_globals.reader)?
            .try_for_each(|res_table| -> Result<_, Error> {
                let table = res_table?;

                let type_def = metadata.get(table.token())?;
                let class_name = if let Some(row) = type_def {
                    row.name()?
                } else {
                    "(unknown class name)"
                };
                annotations
                    .range(table.ptr_range())
                    .name(format!("MethodTable, {class_name}"));

                if let Some(type_def_token) = table.token() {
                    annotations
                        .range(
                            runtime_module
                                .method_table_lookup(&self.tui_globals.reader)?
                                .location_of_method_table_pointer(
                                    type_def_token,
                                ),
                        )
                        .name(format!("Ptr to {class_name} MethodTable"));
                }

                table.collect_annotations(annotations)?;
                let ee_class = table.get_ee_class(&self.tui_globals.reader)?;

                annotations
                    .range(ee_class.ptr_range())
                    .name(format!("EEClass, {class_name}"));
                ee_class.collect_annotations(annotations)?;

                let fields =
                    table.get_field_descriptions(&self.tui_globals.reader)?;

                if let Some(fields) = &fields {
                    annotations
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
                            if let Some(token) = table.token() {
                                format!("{token}")
                            } else {
                                "None".to_string()
                            }
                        );
                        let field_name = metadata.get(field.token())?.name()?;
                        annotations
                            .range(field.ptr_range())
                            .name(format!("FieldDesc {field_name}"));
                        field.collect_annotations(annotations)?;
                        Ok(())
                    },
                )?;

                Ok(())
            })?;

        runtime_module
            .iter_method_tables(&self.tui_globals.reader)?
            .try_for_each(|res_table| -> Result<_, Error> {
                let table = res_table?;
                let class_name = metadata
                    .get(table.token())?
                    .map(|row| row.name())
                    .transpose()?
                    .unwrap_or("(unknown class)");
                let fields =
                    table.get_field_descriptions(&self.tui_globals.reader)?;

                fields
                    .iter()
                    .flatten()
                    .filter(|field| {
                        field.is_static()
                            || field.method_table() == game_obj_method_table_ptr
                    })
                    .try_for_each(|field| -> Result<_, Error> {
                        let field_name = metadata.get(field.token())?.name()?;
                        let location = field.location(
                            &runtime_module,
                            if field.is_static() {
                                FieldContainer::Static
                            } else {
                                FieldContainer::Class(game_obj.into())
                            },
                            &self.tui_globals.reader,
                        )?;

                        let runtime_type = reader
                            .field_to_runtime_type(table.ptr(), &field)?;
                        let size_bytes = runtime_type.size_bytes();
                        let byte_range = location..location + size_bytes;

                        let ann = annotations
                            .range(byte_range.clone())
                            .name(format!("{class_name}.{field_name}"));

                        if !matches!(
                            runtime_type,
                            RuntimeType::ValueType { .. }
                        ) {
                            let bytes = reader.read_bytes(byte_range)?;
                            let value = runtime_type.parse(&bytes)?;
                            ann.value(value);
                        }

                        Ok(())
                    })?;

                Ok(())
            })?;

        self.initial_pointer = game_obj.into();

        Ok(self)
    }

    pub fn build(self) -> Result<TuiExplorer, Error> {
        let mut tui_globals = self.tui_globals;

        tui_globals.cached_reader().init_dlls()?;

        tui_globals.current_region = tui_globals
            .reader
            .find_containing_region(self.initial_pointer)
            .ok_or(Error::PointerNotFound(self.initial_pointer))?
            .read()?;

        tui_globals.annotations.sort_by_key(|ann| {
            (ann.highlight_range, ann.range.end, ann.range.start)
        });

        let stack_memory = tui_globals.reader.stack()?.read()?;
        let stack_frame_table =
            StackFrameTable::new(&tui_globals.reader, &stack_memory);
        let detail_view = {
            let mut detail_view = DetailView::new(self.detail_formatters);
            detail_view.update_details(&tui_globals, self.initial_pointer);
            detail_view
        };

        let memory_table = MemoryTable::new(
            &tui_globals.reader,
            self.initial_pointer,
            self.column_formatters,
        )?;

        let object_explorer =
            ObjectExplorer::new(&self.user_config, &tui_globals)?;

        let metadata_display =
            MetadataDisplay::new(tui_globals.cached_reader())?;

        let out = TuiExplorer {
            tui_globals,
            layout: self.layout,
            buffers: TuiBuffers {
                stack_frame_table,
                running_log: self.running_log,
                memory_table,
                detail_view,
                object_explorer,
                user_config_editor: UserConfigEditor::new(self.user_config),
                metadata_display,
            },

            should_exit: false,
            keystrokes: KeySequence::default(),
        };

        Ok(out)
    }
}

impl TuiBuffers {
    fn buffer_list(&mut self) -> Vec<Box<&mut dyn WidgetWindow>> {
        vec![
            Box::new(&mut self.stack_frame_table),
            Box::new(&mut self.detail_view),
            Box::new(&mut self.memory_table),
            Box::new(&mut self.running_log),
            Box::new(&mut self.object_explorer),
            Box::new(&mut self.user_config_editor),
            Box::new(&mut self.metadata_display),
        ]
    }
}

impl TuiGlobals {
    pub(crate) fn cached_reader(&self) -> CachedReader {
        self.static_value_cache.cached_reader(&self.reader)
    }
}

impl TuiExplorer {
    pub fn new() -> Result<Self, Error> {
        TuiExplorerBuilder::new()?
            .load_config_from_default_location()?
            // .layout_memory_table()
            .layout_object_explorer()
            // .layout_metadata_display()
            .init_symbols()
            .initialize_view_to_stardew_dll()?
            .default_detail_formatters()
            .default_column_formatters()
            .initialize_annotations()?
            .search_based_on_annotations()?
            // .initialize_view_to_stack()?
            // .initialize_view_to_annotation("#Blob Stream")?
            // .initialize_view_to_annotation("Field[100]")?
            // .initialize_view_to_game_obj()?
            // .initialize_view_to_symbol("AppDomain::m_pTheAppDomain")?
            // .initialize_view_to_symbol("SystemDomain::m_pSystemDomain")?
            // .initialize_view_to_symbol("g_dacTable")?
            .build()
    }

    pub fn run(&mut self) -> Result<(), Error> {
        use crossterm::event;

        let mut context = TerminalContext::new()?;
        let handler = SigintHandler::new();

        loop {
            context.draw(|frame| self.draw(frame))?;

            let timeout = std::time::Duration::from_millis(100);
            let poll = event::poll(timeout)?;

            if poll {
                let event_received = event::read()?;
                self.handle_event(event_received);
            }
            self.periodic_update();

            if handler.received() || self.should_exit {
                break;
            }
        }
        Ok(())
    }

    pub fn draw(&mut self, frame: &mut Frame) {
        let mut buffers = self.buffers.buffer_list();
        let layout = self.layout.drawable(&mut buffers, &self.tui_globals);

        frame.render_widget(layout, frame.size());
    }

    pub fn handle_event(&mut self, event: Event) {
        if let Err(err) = self.try_handle_event(event) {
            self.buffers.running_log.add_log(format!("Error: {err}"));
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

        let mut side_effects = WidgetSideEffects::default();

        let mut buffer_list = self.buffers.buffer_list();

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
                    &self.tui_globals,
                    &mut side_effects,
                    &mut buffer_list,
                )
            });

        self.apply_side_effects(side_effects);

        Ok(result)
    }

    pub fn periodic_update(&mut self) {
        let mut buffer_list = self.buffers.buffer_list();

        let mut side_effects = WidgetSideEffects::default();

        let res = buffer_list.iter_mut().try_for_each(|buffer| {
            buffer.periodic_update(&self.tui_globals, &mut side_effects)
        });

        if let Err(err) = res {
            side_effects.add_log(format!("Error: {err}"));
        }

        self.apply_side_effects(side_effects);
    }

    fn apply_side_effects(&mut self, mut side_effects: WidgetSideEffects) {
        if let Some(ptr) = side_effects.change_address {
            if !self.tui_globals.current_region.contains(ptr) {
                let new_region = self
                    .tui_globals
                    .reader
                    .find_containing_region(ptr)
                    .ok_or(Error::PointerNotFound(ptr))
                    .and_then(|region| region.read().map_err(Into::into));

                match new_region {
                    Ok(region) => {
                        self.tui_globals.current_region = region;
                    }
                    Err(err) => {
                        self.buffers
                            .running_log
                            .add_log(format!("Error: {err}"));
                        return;
                    }
                }
            }
        }

        let mut buffer_list = self.buffers.buffer_list();

        if let Some(ptr) = side_effects.change_address {
            buffer_list.iter_mut().for_each(|buffer| {
                buffer.change_address(&self.tui_globals, &mut side_effects, ptr)
            });
        }

        // Process the log messages last, since handling other side
        // effects may result in additional log messages.
        for message in side_effects.log_messages {
            self.buffers.running_log.add_log(message);
        }
    }
}

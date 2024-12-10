use std::ops::Range;

use dotnet_debugger::{
    FieldContainer, RuntimeObject, RuntimeType, TypedPointer,
};
use iterator_extensions::ResultIteratorExt as _;
use itertools::Itertools as _;
use memory_reader::{
    MemoryMapRegion, MemoryReader, MemoryRegion, Pointer, Symbol,
};
use stardew_utils::stardew_valley_pid;

use tui_utils::{
    inputs::{KeyBindingMatch, KeySequence, SigintHandler},
    widgets::DynamicLayout,
    TerminalContext, TuiGlobals, WidgetSideEffects, WidgetWindow,
};

use dll_unpacker::{Annotation as _, Annotator as _};

use crate::{
    Annotation, ChangeAddress, ColumnFormatter, Error, InfoFormatter,
    LiveVariableDisplay, MetadataDisplay, ObjectExplorer, RuntimeModuleView,
    UserConfig, UserConfigEditor,
};

use super::{DetailView, MemoryTable, RunningLog, StackFrameTable};

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

struct TuiBuffers {
    stack_frame_table: StackFrameTable,
    running_log: RunningLog,
    memory_table: MemoryTable,
    detail_view: DetailView,
    object_explorer: ObjectExplorer,
    user_config_editor: UserConfigEditor,
    metadata_display: MetadataDisplay,
    live_variable_display: LiveVariableDisplay,
    runtime_module_view: RuntimeModuleView,
}

pub struct TuiExplorerBuilder {
    tui_globals: TuiGlobals,
    detail_formatters: Vec<Box<dyn InfoFormatter>>,
    column_formatters: Vec<Box<dyn ColumnFormatter>>,
    symbols: Vec<Symbol>,
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

impl dll_unpacker::Annotator for TuiExplorerBuilder {
    fn range(
        &mut self,
        range: Range<Pointer>,
    ) -> &mut impl dll_unpacker::Annotation {
        self.tui_globals
            .get_or_default::<Vec<Annotation>>()
            .range(range)
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
            tui_globals: TuiGlobals::new(reader),

            detail_formatters: Vec::new(),
            column_formatters: Vec::new(),
            initial_pointer,
            symbols: Vec::new(),

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
        self.layout.switch_to_buffer(4);
        self.layout.split_horizontally(None, None);
        self.layout.cycle_next();
        self.layout.switch_to_buffer(7);
        self.layout.split_vertically(None, None);
        self.layout.cycle_next();
        self.layout.switch_to_buffer(3);
        self.layout.cycle_next();
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
        let symbols = self.tui_globals.reader().iter_symbols().collect();
        Self { symbols, ..self }
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
        stardew_valley_dll(self.tui_globals.reader())
    }

    pub fn initialize_view_to_annotation(
        self,
        name: &str,
    ) -> Result<Self, Error> {
        let initial_pointer = self
            .tui_globals
            .get::<Vec<Annotation>>()
            .into_iter()
            .flatten()
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
            .reader()
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

    pub fn initialize_view_to_runtime_module(
        self,
        name: &str,
    ) -> Result<Self, Error> {
        let reader = self.tui_globals.cached_reader();
        let initial_pointer = reader
            .iter_known_modules()
            .find_map(|res_module_ptr| {
                let module_ptr = res_module_ptr.ok()?;
                let module = reader.runtime_module(module_ptr).ok()?;
                let module_name = module.name(reader).ok()?;
                (name == module_name).then(|| module.location)
            })
            .ok_or_else(|| Error::RuntimeModuleNotFound(name.to_string()))?;
        Ok(Self {
            initial_pointer,
            ..self
        })
    }

    pub fn initialize_view_to_stack(self) -> Result<Self, Error> {
        let stack_memory = self.tui_globals.reader().stack()?.read()?;

        let bottom_stack_frame = stack_memory
            .stack_pointers(self.tui_globals.reader())
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
            .iter_method_tables(self.tui_globals.reader())?
            .and_find_ok(|method_table| -> Result<_, Error> {
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
                self.tui_globals.reader(),
            )?;

        self.initial_pointer = game_obj.into();
        self.top_object = Some(game_obj);

        Ok(self)
    }

    pub fn annotate_dll_metadata(mut self) -> Result<Self, Error> {
        let region = self.stardew_valley_dll()?.read()?;
        let dll_info = dll_unpacker::DLLUnpacker::new(&region);

        dll_info.collect_annotations(&mut self)?;

        Ok(self)
    }

    pub fn annotate_runtime_modules(mut self) -> Result<Self, Error> {
        let reader = self.tui_globals.cached_reader();

        let mut annotator = Vec::<Annotation>::new();

        reader.iter_known_modules().try_for_each(
            |res_module_ptr| -> Result<(), Error> {
                let module_ptr = res_module_ptr?;
                let module = reader.runtime_module(module_ptr)?;

                module.collect_annotations(&mut annotator, reader)?;

                Ok(())
            },
        )?;

        self.tui_globals
            .get_or_default::<Vec<Annotation>>()
            .extend(annotator.into_iter());

        Ok(self)
    }

    pub fn annotate_game_obj(mut self) -> Result<Self, Error> {
        let reader = self.tui_globals.cached_reader();
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
                .name()?
        ));

        let runtime_module = reader.runtime_module(module_ptr)?;

        self.running_log.add_log(format!(
            "Method table: {}",
            runtime_module.ptr_to_type_def_table(self.tui_globals.reader())?
        ));

        self.running_log.add_log(format!(
            "Base-ptr of non-GC statics: {}",
            runtime_module
                .base_ptr_of_non_gc_statics(self.tui_globals.reader())?
        ));

        self.running_log.add_log(format!(
            "Base-ptr of GC statics: {}",
            runtime_module.base_ptr_of_gc_statics(self.tui_globals.reader())?
        ));

        let mut annotator = Vec::<Annotation>::new();

        annotator
            .range(
                runtime_module
                    .type_def_table(self.tui_globals.reader())?
                    .location
                    .clone(),
            )
            .name("TypeDefToMethodDef table");

        let game_obj_method_table = runtime_module
            .iter_method_tables(self.tui_globals.reader())?
            .and_find_ok(|method_table| -> Result<_, Error> {
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
            self.tui_globals.reader(),
        )?;

        self.running_log
            .add_log(format!("Top-level Game1 object {game_obj}"));

        {
            let game_obj: Pointer = game_obj.into();
            annotator
                .range(game_obj..game_obj + Pointer::SIZE)
                .name("Game object method table");
            annotator
                .range(game_obj..game_obj + game_obj_method_table.base_size())
                .name("Top-level game object")
                .disable_highlight();
        }

        runtime_module
            .iter_method_tables(self.tui_globals.reader())?
            .try_for_each(|res_table| -> Result<_, Error> {
                let table = res_table?;

                let type_def = metadata.get(table.token())?;
                let class_name = if let Some(row) = type_def {
                    row.name()?
                } else {
                    "(unknown class name)"
                };
                annotator
                    .range(table.ptr_range())
                    .name(format!("MethodTable, {class_name}"));

                if let Some(type_def_token) = table.token() {
                    annotator
                        .range(
                            runtime_module
                                .type_def_table(self.tui_globals.reader())?
                                .location_of_method_table_pointer(
                                    type_def_token,
                                ),
                        )
                        .name(format!("Ptr to {class_name} MethodTable"));
                }

                table.collect_annotations(&mut annotator)?;
                let ee_class = table.get_ee_class(self.tui_globals.reader())?;

                annotator
                    .range(ee_class.ptr_range())
                    .name(format!("EEClass, {class_name}"));
                ee_class.collect_annotations(&mut annotator)?;

                let fields =
                    table.get_field_descriptions(self.tui_globals.reader())?;

                if let Some(fields) = &fields {
                    annotator
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
                        annotator
                            .range(field.ptr_range())
                            .name(format!("FieldDesc {field_name}"));
                        field.collect_annotations(&mut annotator)?;
                        Ok(())
                    },
                )?;

                Ok(())
            })?;

        runtime_module
            .iter_method_tables(self.tui_globals.reader())?
            .try_for_each(|res_table| -> Result<_, Error> {
                let table = res_table?;
                let class_name = metadata
                    .get(table.token())?
                    .map(|row| row.name())
                    .transpose()?
                    .unwrap_or("(unknown class)");
                let fields =
                    table.get_field_descriptions(self.tui_globals.reader())?;

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
                            self.tui_globals.reader(),
                        )?;

                        let runtime_type = reader
                            .field_to_runtime_type(table.ptr(), &field)?;
                        let size_bytes = runtime_type.size_bytes();
                        let byte_range = location..location + size_bytes;

                        let ann = annotator
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

        self.tui_globals
            .get_or_default::<Vec<Annotation>>()
            .extend(annotator.into_iter());

        Ok(self)
    }

    pub fn build(self) -> Result<TuiExplorer, Error> {
        let mut tui_globals = self.tui_globals;

        tui_globals.insert::<MemoryRegion>(
            tui_globals
                .reader()
                .find_containing_region(self.initial_pointer)
                .ok_or(Error::PointerNotFound(self.initial_pointer))?
                .read()?,
        );

        tui_globals
            .get_or_default::<Vec<Annotation>>()
            .sort_by_key(Annotation::sort_key);

        let stack_memory = tui_globals.reader().stack()?.read()?;
        let stack_frame_table =
            StackFrameTable::new(tui_globals.reader(), &stack_memory);
        let detail_view = {
            let mut detail_view = DetailView::new(self.detail_formatters);
            detail_view.update_details(&tui_globals, self.initial_pointer);
            detail_view
        };

        let memory_table = MemoryTable::new(
            tui_globals.reader(),
            self.initial_pointer,
            self.column_formatters,
        )?;

        let object_explorer =
            ObjectExplorer::new(&self.user_config, &tui_globals)?;

        let metadata_display =
            MetadataDisplay::new(tui_globals.cached_reader())?;

        let runtime_module_view =
            RuntimeModuleView::new(tui_globals.cached_reader())?;

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
                live_variable_display: LiveVariableDisplay::new(),
                runtime_module_view,
            },

            should_exit: false,
            keystrokes: KeySequence::default(),
        };

        Ok(out)
    }
}

impl TuiBuffers {
    fn buffer_list(&mut self) -> Vec<Box<&mut dyn WidgetWindow<Error>>> {
        vec![
            Box::new(&mut self.stack_frame_table),
            Box::new(&mut self.detail_view),
            Box::new(&mut self.memory_table),
            Box::new(&mut self.running_log),
            Box::new(&mut self.object_explorer),
            Box::new(&mut self.user_config_editor),
            Box::new(&mut self.metadata_display),
            Box::new(&mut self.live_variable_display),
            Box::new(&mut self.runtime_module_view),
        ]
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
            .annotate_dll_metadata()?
            .annotate_runtime_modules()?
            // .annotate_game_obj()?
            .initialize_view_to_runtime_module("Stardew Valley")?
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

        frame.render_widget(layout, frame.area());
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
        side_effects
            .iter::<ChangeAddress>()
            .map(|addr| addr.0)
            .find(|ptr| {
                let current_region = self
                    .tui_globals
                    .get::<MemoryRegion>()
                    .expect("Should be initialized with a memory region");
                !current_region.contains(*ptr)
            })
            .into_iter()
            .for_each(|ptr| {
                let new_region = self
                    .tui_globals
                    .reader()
                    .find_containing_region(ptr)
                    .ok_or(Error::PointerNotFound(ptr))
                    .and_then(|region| region.read().map_err(Into::into));

                match new_region {
                    Ok(region) => {
                        self.tui_globals.insert(region);
                    }
                    Err(err) => {
                        self.buffers
                            .running_log
                            .add_log(format!("Error: {err}"));
                        return;
                    }
                }
            });

        let mut buffer_list = self.buffers.buffer_list();

        if let Err(err) = buffer_list.iter_mut().try_for_each(|buffer| {
            buffer.apply_side_effects(&self.tui_globals, &mut side_effects)
        }) {
            side_effects.add_log(format!("Error: {err}"));
        }

        side_effects
            .into_iter::<Annotation>()
            .with_position()
            .for_each(|(pos, ann)| {
                let annotations =
                    self.tui_globals.get_or_default::<Vec<Annotation>>();
                annotations.push(ann);
                if matches!(
                    pos,
                    itertools::Position::Last | itertools::Position::Only
                ) {
                    annotations.sort_by_key(Annotation::sort_key);
                }
            });

        // Re-process the log messages last, since handling other side
        // effects may result in additional log messages.
        self.buffers
            .running_log
            .apply_side_effects(&self.tui_globals, &mut side_effects)
            .unwrap();
    }
}

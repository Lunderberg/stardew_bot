use std::ops::Range;

use itertools::{Either, Itertools};
use regex::Regex;

use ratatui::{
    style::{Color, Modifier, Style},
    text::{Line, Text},
    widgets::{List, ListState, StatefulWidget, Widget as _},
};

use dotnet_debugger::{
    CachedReader, FieldContainer, FieldDescription, RuntimeType, RuntimeValue,
};
use memory_reader::{OwnedBytes, Pointer};

use crate::{
    extended_tui::{
        ScrollableState as _, SearchDirection, SearchWindow, WidgetWindow,
    },
    extensions::*,
    TuiGlobals, UserConfig,
};
use crate::{Error, KeyBindingMatch};

pub struct ObjectExplorer {
    object_tree: ObjectTreeNode,
    list_state: ListState,
    prev_draw_height: usize,
    search: Option<SearchWindow<ListState>>,
    // TODO: Allow UserConfigEditor to propagate changes to
    // ObjectExplorer without restarting the program.
    display_options: DisplayOptions,
}

struct DisplayOptions {
    sort_top: Vec<Regex>,
    sort_bottom: Vec<Regex>,
}

#[derive(Clone)]
struct ObjectTreeNode {
    context: ObjectTreeContext,
    value: ObjectTreeValue,
}

#[derive(Clone)]
enum ObjectTreeContext {
    ListElement,
    Field {
        field_name: String,
        field_type: String,
    },
}

#[derive(Clone)]
struct ObjectTreeValue {
    location: Range<Pointer>,
    runtime_type: RuntimeType,
    should_read: bool,
    display_expanded: bool,
    kind: ObjectTreeValueKind,
}

#[derive(Clone)]
enum ObjectTreeValueKind {
    UnreadValue,
    Value(RuntimeValue),
    String(String),
    Array(Vec<ObjectTreeNode>),
    Object {
        class_name: String,
        fields: Vec<ObjectTreeNode>,
    },
}

struct ObjectTreeIterator<'a> {
    stack: Vec<ObjectTreeIteratorItem<'a>>,
}

enum IterationOrder {
    PreVisit,
    LeafNode,
    PostVisit,
}

struct ObjectTreeIteratorItem<'a> {
    node: &'a ObjectTreeNode,
    pos: IterationOrder,
    tree_depth: usize,
}

#[derive(Clone, Copy)]
struct Indent(usize);

impl ObjectExplorer {
    pub(crate) fn new(
        user_config: &UserConfig,
        globals: &TuiGlobals,
    ) -> Result<Self, Error> {
        // TOOD: Indicate that there's been a failed Regex parsing,
        // rather than just silently ignoring it.
        let display_options = DisplayOptions {
            sort_top: user_config
                .object_explorer_sort_top
                .iter()
                .cloned()
                .filter_map(|string| string.try_into().ok())
                .collect(),

            sort_bottom: user_config
                .object_explorer_sort_bottom
                .iter()
                .cloned()
                .filter_map(|string| string.try_into().ok())
                .collect(),
        };

        let reader = globals.cached_reader();

        // TODO: Allow a configurable hiding of static fields
        // (e.g. a lot of the compiler-generated fields).

        let static_fields = reader
            .iter_known_modules()
            .flat_map_ok(|module_ptr| {
                let prefetch_statics: Vec<_> = reader
                    .static_value_ranges(module_ptr)
                    .map(|range| reader.read_bytes(range))
                    .collect::<Result<_, _>>()?;

                // The static_ranges must have their ownership moved
                // into the `.map_ok` lambda function that uses them,
                // as that lambda lives beyond the local function
                // scope.  However, using the `move` keyword would
                // also move the `display_options` and `reader`
                // objects to be owned by the lambda function.  To
                // avoid this, declare new variables that hold the
                // reference, and move the reference into the lambda.
                let reader_ref = &reader;
                let display_options_ref = &display_options;

                let module = reader.runtime_module(module_ptr)?;
                let iter_vtable_ptr =
                    module.iter_method_table_pointers(&reader)?;
                let iter_static_fields = iter_vtable_ptr
                    .map(Ok)
                    .flat_map_ok(|method_table_ptr| {
                        reader.iter_static_fields(method_table_ptr)
                    })
                    .map_ok(move |field| -> Result<_, Error> {
                        let node = ObjectTreeNode::initial_field(
                            FieldContainer::Static,
                            field,
                            reader_ref,
                            display_options_ref,
                            &prefetch_statics,
                        )?;
                        Ok((field, node))
                    })
                    .map(|res| res?);
                Ok(iter_static_fields)
            })
            .map(|res| res?)
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .sorted_by_key(|(field, _)| {
                display_options.sort_key(*field, &reader)
            })
            .map(|(_, node)| node)
            .collect::<Vec<_>>();

        // TODO: Make a better display, since displaying the static
        // fields as being in a single class is incorrect.
        let object_tree = ObjectTreeNode {
            value: ObjectTreeValue {
                kind: ObjectTreeValueKind::Object {
                    class_name: "statics".into(),
                    fields: static_fields,
                },
                location: Pointer::null()..Pointer::null(),
                runtime_type: RuntimeType::Class,
                should_read: false,
                display_expanded: true,
            },

            context: ObjectTreeContext::Field {
                field_name: "(static fields)".into(),
                field_type: "(static fields)".into(),
            },
        };

        Ok(ObjectExplorer {
            object_tree,
            list_state: ListState::default().with_selected(Some(0)),
            prev_draw_height: 1,
            search: None,
            display_options,
        })
    }

    fn selected_node(&self) -> Option<&ObjectTreeValue> {
        self.list_state.selected().and_then(|selected| {
            self.object_tree
                .iter()
                .map(|item| &item.node.value)
                .nth(selected)
        })
    }

    fn toggle_expansion(&mut self) {
        let selected = self
            .list_state
            .selected()
            .expect("ObjectExplorer should always have a selected line.");

        let node = self
            .object_tree
            .value
            .node_at_line(selected)
            .expect("The selected line should always point to a node.");

        if matches!(
            node.kind,
            ObjectTreeValueKind::Object { .. } | ObjectTreeValueKind::Array(_)
        ) {
            // TODO: Find a good UX to distinguish between
            // "collapse this node" and "re-read this node".
            node.display_expanded = !node.display_expanded;
        } else {
            node.should_read = !node.should_read;
        }
    }

    fn start_search(&mut self, direction: SearchDirection) {
        self.search =
            Some(SearchWindow::new(direction, self.list_state.clone()));
    }

    fn cancel_search(&mut self) {
        if let Some(search) = self.search.take() {
            self.list_state = search.pre_search_state;
        }
    }

    fn finalize_search(&mut self) {
        self.search = None;
    }
}

impl std::fmt::Display for Indent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{: >indent$}", "", indent = self.0)
    }
}
impl std::ops::Add<usize> for Indent {
    type Output = Indent;

    fn add(self, rhs: usize) -> Self::Output {
        Self(self.0 + rhs)
    }
}

impl ObjectTreeValue {
    fn initial_value(
        location: Pointer,
        runtime_type: RuntimeType,
        reader: &CachedReader,
        display_options: &DisplayOptions,
        prefetch: &[OwnedBytes],
    ) -> Result<Self, Error> {
        let location = location..location + runtime_type.size_bytes();

        let kind = match runtime_type {
            RuntimeType::Prim(_)
            | RuntimeType::Class
            | RuntimeType::String
            | RuntimeType::Array => {
                let opt_bytes = prefetch
                    .iter()
                    .find(|bytes| bytes.contains_range(location.clone()));
                if let Some(bytes) = opt_bytes {
                    let runtime_value = runtime_type
                        .parse(bytes.subrange(location.clone()).into())?;
                    ObjectTreeValueKind::Value(runtime_value)
                } else {
                    ObjectTreeValueKind::UnreadValue
                }
            }
            RuntimeType::ValueType {
                method_table: field_method_table,
                ..
            } => {
                let class_name = reader
                    .method_table_to_name(field_method_table)?
                    .to_string();

                let fields = reader
                    .field_descriptions(field_method_table)?
                    .into_iter()
                    .flatten()
                    .filter(|subfield| !subfield.is_static())
                    .sorted_by_key(|subfield| {
                        display_options.sort_key(*subfield, reader)
                    })
                    .map(|subfield| {
                        ObjectTreeNode::initial_field(
                            FieldContainer::ValueType(location.start),
                            subfield,
                            reader,
                            display_options,
                            prefetch,
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                ObjectTreeValueKind::Object { class_name, fields }
            }
        };

        Ok(ObjectTreeValue {
            kind,
            location,
            runtime_type,
            should_read: false,
            display_expanded: true,
        })
    }

    fn num_lines(&self) -> usize {
        self.display_expanded
            .then(|| {
                self.kind
                    .iter_children()
                    .map(|child| child.value.num_lines())
                    .sum::<usize>()
            })
            .filter(|child_lines| *child_lines > 0)
            .map(|child_lines| child_lines + 2)
            .unwrap_or(1)
    }

    fn node_at_line<'a>(
        &'a mut self,
        mut i_line: usize,
    ) -> Option<&'a mut ObjectTreeValue> {
        let num_lines = self.num_lines();
        if i_line >= num_lines {
            return None;
        }
        if i_line == 0 {
            Some(self)
        } else if i_line + 1 < num_lines {
            assert!(
                self.display_expanded,
                "This conditional should only be reached \
                 if this is a multi-line object."
            );

            i_line -= 1;
            for child in self.kind.iter_mut_children() {
                let child_lines = child.value.num_lines();
                if i_line < child_lines {
                    return child.value.node_at_line(i_line);
                }
                i_line -= child_lines;
            }

            panic!(
                "This should be unreachable, \
                 should have found a field containing the line."
            );
        } else if i_line < num_lines {
            Some(self)
        } else {
            None
        }
    }
}

impl ObjectTreeNode {
    fn iter(&self) -> impl Iterator<Item = ObjectTreeIteratorItem> + '_ {
        ObjectTreeIterator::new(self)
    }

    fn initial_field(
        container: FieldContainer,
        field: FieldDescription,
        reader: &CachedReader,
        display_options: &DisplayOptions,
        prefetch: &[OwnedBytes],
    ) -> Result<ObjectTreeNode, Error> {
        let field_name = reader.field_to_name(&field)?.to_string();

        // TODO: Move this formatting over to the Signature class
        let field_name = if field.is_static() {
            let class_name =
                reader.method_table_to_name(field.method_table())?;
            format!("{class_name}.{field_name}")
        } else {
            field_name
        };

        let runtime_type = reader.field_to_runtime_type(&field)?;
        let field_type = reader.field_to_type_name(&field)?.to_string();

        let method_table = reader.method_table(field.method_table())?;
        let module = reader.runtime_module(method_table.module())?;
        let location = field.location(module, container, reader)?;

        let value = ObjectTreeValue::initial_value(
            location,
            runtime_type,
            reader,
            display_options,
            prefetch,
        )?;

        Ok(ObjectTreeNode {
            context: ObjectTreeContext::Field {
                field_name,
                field_type,
            },
            value,
        })
    }

    fn expand_marked(
        &mut self,
        display_options: &DisplayOptions,
        reader: &CachedReader,
    ) -> Result<(), Error> {
        if self.value.should_read {
            self.value.should_read = false;

            // Check if a generic `TypedPointer<RuntimeObject>` should
            // actually be a type with a more specific unpacker.
            //
            // TODO: See if there's a cleaner way to handle this.
            // These mostly occur from generic entries, where a member
            // variable has type `GenericType`, and are currently
            // treated as arbitrary objects.  These should instead be
            // substituted with the types from the containing
            // `GenericInst`.  Granted, this wouldn't cover all cases,
            // as there could also be a generic `List<Object>`, so
            // maybe the later check will still be necessary anyways.
            match self.value.kind {
                ObjectTreeValueKind::Value(RuntimeValue::Object(ptr)) => {
                    let obj = reader.object(ptr)?;
                    let method_table =
                        reader.method_table(obj.method_table())?;

                    let ptr: Pointer = ptr.into();
                    self.value.runtime_type =
                        method_table.runtime_type(reader)?;
                    match self.value.runtime_type {
                        RuntimeType::Prim(_) => {
                            self.value.kind = ObjectTreeValueKind::UnreadValue;
                        }
                        RuntimeType::ValueType { .. } => {
                            panic!("Leaf node should not have ValueType")
                        }
                        RuntimeType::Class => {}
                        RuntimeType::String => {
                            self.value.kind = ObjectTreeValueKind::Value(
                                RuntimeValue::String(ptr.into()),
                            );
                        }
                        RuntimeType::Array => {
                            self.value.kind = ObjectTreeValueKind::Value(
                                RuntimeValue::Array(ptr.into()),
                            );
                        }
                    }
                }
                _ => {}
            }

            // Reading each type
            match self.value.kind {
                ObjectTreeValueKind::UnreadValue => {
                    let value = reader.value(
                        self.value.runtime_type,
                        self.value.location.clone(),
                    )?;
                    self.value.kind = ObjectTreeValueKind::Value(value);
                }
                ObjectTreeValueKind::Object { .. } => {
                    let value = reader.value(
                        RuntimeType::Class,
                        self.value.location.clone(),
                    )?;
                    self.value.kind = ObjectTreeValueKind::Value(value);
                }
                ObjectTreeValueKind::Array { .. } => {
                    let value = reader.value(
                        RuntimeType::Array,
                        self.value.location.clone(),
                    )?;
                    self.value.kind = ObjectTreeValueKind::Value(value);
                }

                ObjectTreeValueKind::Value(RuntimeValue::String(ptr)) => {
                    if ptr.is_null() {
                        return Err(Error::CannotExpandNullField);
                    }
                    self.value.kind =
                        ObjectTreeValueKind::String(ptr.read(reader)?.into());
                }

                ObjectTreeValueKind::Value(RuntimeValue::Array(ptr)) => {
                    if ptr.is_null() {
                        return Err(Error::CannotExpandNullField);
                    }

                    let array = ptr.read(reader)?;
                    let prefetch = reader.read_bytes(array.ptr_range())?;
                    let prefetch = &[prefetch];

                    let element_type = array.element_type();

                    let items = (0..array.num_elements())
                        .map(|index| -> Result<_, Error> {
                            let location = array.element_location(index);
                            let value = ObjectTreeValue::initial_value(
                                location.start,
                                element_type,
                                reader,
                                display_options,
                                prefetch,
                            )?;
                            Ok(ObjectTreeNode {
                                context: ObjectTreeContext::ListElement,
                                value,
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    self.value.kind = ObjectTreeValueKind::Array(items);
                }

                ObjectTreeValueKind::Value(RuntimeValue::Object(ptr)) => {
                    if ptr.is_null() {
                        return Err(Error::CannotExpandNullField);
                    }

                    let obj = reader.object(ptr)?;
                    let class_name = reader
                        .method_table_to_name(obj.method_table())?
                        .to_string();

                    let instance_location: Pointer = obj.location().into();
                    let size_bytes =
                        reader.method_table(obj.method_table())?.base_size();
                    let prefetch = reader.read_bytes(
                        instance_location..instance_location + size_bytes,
                    )?;
                    let prefetch = &[prefetch];

                    let fields = reader
                        .iter_instance_fields(obj.method_table())?
                        .sorted_by_key(|field| {
                            display_options.sort_key(*field, reader)
                        })
                        .map(|field| -> Result<_, Error> {
                            Self::initial_field(
                                FieldContainer::Class(instance_location.into()),
                                field,
                                reader,
                                display_options,
                                prefetch,
                            )
                        })
                        .collect::<Result<Vec<_>, Error>>()?;

                    self.value.kind =
                        ObjectTreeValueKind::Object { class_name, fields };
                }

                ObjectTreeValueKind::Value { .. }
                | ObjectTreeValueKind::String(_) => {
                    return Err(Error::NotImplementedYet(
                    "Collapse view of containing object when value selected"
                        .to_string(),
                ));
                }
            }
        } else {
            self.value.kind.iter_mut_children().try_for_each(|field| {
                field.expand_marked(display_options, reader)
            })?;
        }

        Ok(())
    }

    fn iter_visit_type(&self) -> IterationOrder {
        match self.value.kind {
            ObjectTreeValueKind::Array(_)
            | ObjectTreeValueKind::Object { .. }
                if self.value.display_expanded =>
            {
                IterationOrder::PreVisit
            }

            ObjectTreeValueKind::Array(_)
            | ObjectTreeValueKind::Object { .. }
            | ObjectTreeValueKind::UnreadValue
            | ObjectTreeValueKind::Value(_)
            | ObjectTreeValueKind::String(_) => IterationOrder::LeafNode,
        }
    }
}

impl ObjectTreeValueKind {
    fn iter_children(
        &self,
    ) -> impl Iterator<Item = &ObjectTreeNode> + DoubleEndedIterator + '_ {
        match self {
            ObjectTreeValueKind::Array(children)
            | ObjectTreeValueKind::Object {
                fields: children, ..
            } => Either::Left(children.iter()),

            ObjectTreeValueKind::UnreadValue
            | ObjectTreeValueKind::Value(_)
            | ObjectTreeValueKind::String(_) => {
                Either::Right(std::iter::empty())
            }
        }
    }

    fn iter_mut_children(
        &mut self,
    ) -> impl Iterator<Item = &mut ObjectTreeNode> + DoubleEndedIterator + '_
    {
        match self {
            ObjectTreeValueKind::Array(children)
            | ObjectTreeValueKind::Object {
                fields: children, ..
            } => Either::Left(children.iter_mut()),

            ObjectTreeValueKind::UnreadValue
            | ObjectTreeValueKind::Value(_)
            | ObjectTreeValueKind::String(_) => {
                Either::Right(std::iter::empty())
            }
        }
    }
}

impl<'a> ObjectTreeIterator<'a> {
    fn new(node: &'a ObjectTreeNode) -> Self {
        Self {
            stack: vec![ObjectTreeIteratorItem {
                pos: node.iter_visit_type(),
                node,
                tree_depth: 0,
            }],
        }
    }
}

impl<'a> Iterator for ObjectTreeIterator<'a> {
    type Item = ObjectTreeIteratorItem<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.stack.pop()?;

        if matches!(next.pos, IterationOrder::PreVisit) {
            next.node
                .value
                .kind
                .iter_children()
                .rev()
                .map(|child| ObjectTreeIteratorItem {
                    pos: child.iter_visit_type(),
                    node: child,
                    tree_depth: next.tree_depth + 1,
                })
                .with_position()
                .for_each(|(pos, item)| {
                    if matches!(
                        pos,
                        itertools::Position::First | itertools::Position::Only
                    ) {
                        self.stack.push(ObjectTreeIteratorItem {
                            pos: IterationOrder::PostVisit,
                            ..next
                        });
                    }
                    self.stack.push(item);
                });
        }

        Some(next)
    }
}

impl<'a> std::fmt::Display for ObjectTreeIteratorItem<'a> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(fmt, "{}", Indent(self.tree_depth * 4))?;

        let value = &self.node.value;

        match self.pos {
            IterationOrder::PreVisit | IterationOrder::LeafNode => {
                match &self.node.context {
                    ObjectTreeContext::Field {
                        field_name,
                        field_type,
                    } => {
                        write!(fmt, "{field_type} {field_name} = ")?;
                    }
                    ObjectTreeContext::ListElement { .. } => {}
                }
            }
            IterationOrder::PostVisit => {}
        }

        match self.pos {
            IterationOrder::PreVisit => match &value.kind {
                ObjectTreeValueKind::Array(_) => write!(fmt, "[")?,

                ObjectTreeValueKind::Object { class_name, .. } => {
                    write!(fmt, "{class_name} {{")?;
                }

                ObjectTreeValueKind::UnreadValue
                | ObjectTreeValueKind::Value(_)
                | ObjectTreeValueKind::String(_) => {
                    panic!(
                        "Should be unreachable, \
                         these value kinds are single-line."
                    )
                }
            },
            IterationOrder::LeafNode => match &value.kind {
                ObjectTreeValueKind::Object { class_name, .. } => {
                    write!(fmt, "{class_name} {{...}}")?;
                }
                ObjectTreeValueKind::Array(_) => write!(fmt, "[...]")?,
                ObjectTreeValueKind::UnreadValue => {
                    let ptr = value.location.start;
                    write!(fmt, "unread value @ {ptr}")?
                }
                ObjectTreeValueKind::Value(value) => write!(fmt, "{value}")?,
                ObjectTreeValueKind::String(value) => write!(fmt, "{value:?}")?,
            },
            IterationOrder::PostVisit => match &value.kind {
                ObjectTreeValueKind::Array(_) => write!(fmt, "]")?,
                ObjectTreeValueKind::Object { .. } => write!(fmt, "}}")?,

                ObjectTreeValueKind::UnreadValue
                | ObjectTreeValueKind::Value(_)
                | ObjectTreeValueKind::String(_) => {
                    panic!(
                        "Should be unreachable, \
                                 these value kinds are single-line."
                    )
                }
            },
        }

        match self.pos {
            IterationOrder::PreVisit => {}
            IterationOrder::LeafNode | IterationOrder::PostVisit => {
                match &self.node.context {
                    ObjectTreeContext::Field { .. } => write!(fmt, ";")?,
                    ObjectTreeContext::ListElement { .. } => write!(fmt, ",")?,
                }
            }
        }

        Ok(())
    }
}

impl WidgetWindow for ObjectExplorer {
    fn title(&self) -> std::borrow::Cow<str> {
        "ObjectExplorer".into()
    }

    fn apply_key_binding<'a>(
        &'a mut self,
        keystrokes: &'a crate::KeySequence,
        _globals: &'a crate::TuiGlobals,
        side_effects: &'a mut crate::extended_tui::WidgetSideEffects,
    ) -> KeyBindingMatch {
        let num_lines = self.object_tree.value.num_lines();

        KeyBindingMatch::Mismatch
            .or_else(|| {
                self.list_state
                    .apply_key_binding(
                        keystrokes,
                        num_lines,
                        self.prev_draw_height,
                    )
                    .then(|| self.finalize_search())
                    .or_else(|| {
                        if let Some(search) = self.search.as_mut() {
                            search
                                .apply_key_binding(
                                    keystrokes,
                                    num_lines,
                                    |i_line| {
                                        self.object_tree
                                            .iter()
                                            .nth(i_line)
                                            .map(|item| format!("{item}"))
                                            .into_iter()
                                            .collect()
                                    },
                                )
                                .then(|| {
                                    self.list_state.select(Some(
                                        search.recommended_row_selection(),
                                    ))
                                })
                                .or_try_binding("C-g", keystrokes, || {
                                    self.cancel_search()
                                })
                        } else {
                            KeyBindingMatch::Mismatch
                        }
                    })
            })
            .or_try_binding("<tab>", keystrokes, || self.toggle_expansion())
            .or_try_binding("C-s", keystrokes, || {
                self.start_search(SearchDirection::Forward)
            })
            .or_try_binding("C-r", keystrokes, || {
                self.start_search(SearchDirection::Reverse)
            })
            .or_try_binding("<enter>", keystrokes, || {
                if let Some(selected) = self.selected_node() {
                    side_effects.change_address(selected.location.start);
                }
            })
    }

    fn periodic_update<'a>(
        &mut self,
        globals: &'a crate::TuiGlobals,
        _side_effects: &'a mut crate::extended_tui::WidgetSideEffects,
    ) -> Result<(), Error> {
        let reader = globals.cached_reader();
        self.object_tree
            .expand_marked(&self.display_options, &reader)?;
        Ok(())
    }

    fn draw<'a>(
        &'a mut self,
        _globals: &'a crate::TuiGlobals,
        area: ratatui::layout::Rect,
        buf: &mut ratatui::prelude::Buffer,
    ) {
        self.prev_draw_height = area.height as usize;

        let area = if let Some(search) = &self.search {
            let (area, search_area) = area.split_from_bottom(3);
            search.render(search_area, buf);
            area
        } else {
            area
        };

        // This is a hack to update the `list_state` so that the
        // `display_range` will have the correct range.  If the lines
        // outside of the `display_range` are generated, it slows down
        // the rendering considerably.  Rendering the list of empty
        // lines updates `display_range` to be accurate for the actual
        // rendering.
        let num_lines = self.object_tree.value.num_lines();
        StatefulWidget::render(
            List::new(vec![Text::default(); num_lines]),
            area,
            buf,
            &mut self.list_state,
        );

        let display_range = {
            let start = self.list_state.offset();
            let num_lines = area.height as usize;
            start..start + num_lines
        };

        let lines = self.object_tree.iter().enumerate().map(|(i, item)| {
            if display_range.contains(&i) {
                let mut line: Line = format!("{item}").into();

                line = line.style_regex(
                    "0x[0-9a-fA-F]+",
                    Style::default().fg(Color::LightRed),
                );

                if let Some(search) = self.search.as_ref() {
                    line = search.highlight_search_matches(line);
                }

                line
            } else {
                Line::default()
            }
        });

        let widget = List::new(lines)
            .highlight_style(Style::default().add_modifier(Modifier::REVERSED))
            .with_scrollbar(self.object_tree.value.num_lines())
            .number_each_row();

        StatefulWidget::render(widget, area, buf, &mut self.list_state);
    }
}

impl DisplayOptions {
    fn sort_key(
        &self,
        field: FieldDescription,
        reader: &CachedReader,
    ) -> impl Ord {
        let res_sort_key = || -> Result<_, Error> {
            let class_name =
                reader.method_table_to_name(field.method_table())?;
            let field_name = reader.field_to_name(&field)?.to_string();
            let field_type = reader.field_to_type_name(&field)?.to_string();

            let static_str = if field.is_static() { "static " } else { "" };

            let search_string =
                format!("{static_str}{field_type} {class_name}.{field_name}");

            let sort_key = if let Some((top_match, _)) = self
                .sort_top
                .iter()
                .enumerate()
                .find(|(_, regex)| regex.is_match(&search_string))
            {
                (false, 0, top_match)
            } else if let Some((bottom_match, _)) = self
                .sort_bottom
                .iter()
                .enumerate()
                .find(|(_, regex)| regex.is_match(&search_string))
            {
                (false, 2, bottom_match)
            } else {
                (false, 1, 0)
            };

            Ok(sort_key)
        }();
        res_sort_key.unwrap_or_else(|_| (true, 0, 0))
    }
}

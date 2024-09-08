use std::ops::Range;

use dotnet_debugger::{
    CachedReader, FieldContainer, FieldDescription, MethodTable,
    PersistentState, RuntimeType, RuntimeValue, TypedPointer,
};
use memory_reader::{MemoryReader, Pointer};
use ratatui::{
    style::{Color, Modifier, Style},
    text::Line,
    widgets::{List, ListState, StatefulWidget, Widget as _},
};

use crate::extensions::*;
use crate::{
    extended_tui::{
        ScrollableState as _, SearchDirection, SearchWindow, WidgetWindow,
    },
    Error, KeyBindingMatch,
};

pub struct ObjectExplorer {
    state: PersistentState,
    // TODO: Make this be mandatory, not an Option<T>.
    object_tree: Option<ObjectTreeNode>,
    list_state: ListState,
    prev_draw_height: usize,
    search: Option<SearchWindow<ListState>>,
}

struct ObjectTreeNode {
    location: Range<Pointer>,
    runtime_type: RuntimeType,
    should_read: bool,
    tree_depth: usize,
    field_name: String,
    field_type: String,
    kind: ObjectTreeNodeKind,
}

enum ObjectTreeNodeKind {
    NewValue,
    Value(RuntimeValue),
    Object {
        display_expanded: bool,
        class_name: String,
        fields: Vec<ObjectTreeNode>,
    },
}

#[derive(Clone, Copy)]
struct Indent(usize);

impl ObjectExplorer {
    pub fn new(reader: &MemoryReader) -> Result<Self, Error> {
        let state = PersistentState::new().init_dlls(reader)?;

        let reader = state.cached_reader(reader);
        let static_fields = reader
            .iter_known_modules()
            .map(|res_module_ptr| {
                res_module_ptr
                    .and_then(|module_ptr| reader.runtime_module(module_ptr))
            })
            .flat_map(|res_module| {
                match res_module.and_then(|module| {
                    module.iter_method_table_pointers(&reader)
                }) {
                    Ok(iter) => itertools::Either::Left(iter.map(Ok)),
                    Err(err) => {
                        itertools::Either::Right(std::iter::once(Err(err)))
                    }
                }
            })
            .flat_map(|res_method_table_ptr| {
                // TODO: Make a .flat_map_ok extension method.
                match res_method_table_ptr.and_then(|method_table_ptr| {
                    reader.iter_static_fields(method_table_ptr)
                }) {
                    Ok(iter) => itertools::Either::Left(iter.map(Ok)),
                    Err(err) => {
                        itertools::Either::Right(std::iter::once(Err(err)))
                    }
                }
            })
            // TODO: Allow a configurable display order for the static fields.
            //
            // TODO: Allow a configurable hiding of static fields
            // (e.g. a lot of the compiler-generated fields.
            //
            // TODO: Performance improvement, since this GUI moves at
            // a crawl now with all static values displayed in it.
            .map(|res_field| -> Result<_, Error> {
                let field = res_field?;
                let node = ObjectTreeNode::initial_field(
                    FieldContainer::Static,
                    field.method_table(),
                    1,
                    field,
                    &reader,
                )?;
                Ok(node)
            })
            .collect::<Result<Vec<_>, _>>()?;

        // TODO: Make a better display, since displaying the static
        // fields as being in a single class is incorrect.
        let object_tree = Some(ObjectTreeNode {
            kind: ObjectTreeNodeKind::Object {
                display_expanded: true,
                class_name: "statics".into(),
                fields: static_fields,
            },
            location: Pointer::null()..Pointer::null(),
            runtime_type: RuntimeType::Class,
            should_read: false,
            tree_depth: 0,
            field_name: "(static fields)".into(),
            field_type: "(static fields)".into(),
        });

        Ok(ObjectExplorer {
            state,
            object_tree,
            list_state: ListState::default().with_selected(Some(0)),
            prev_draw_height: 1,
            search: None,
        })
    }

    fn selected_node(&self) -> Option<&ObjectTreeNode> {
        self.list_state.selected().and_then(|selected| {
            self.object_tree
                .as_ref()
                .into_iter()
                .flat_map(|node| node.iter())
                .map(|(node, _)| node)
                .nth(selected)
        })
    }

    fn toggle_expansion(&mut self) {
        let selected = self
            .list_state
            .selected()
            .expect("ObjectExplorer should always have a selected line.");

        let Some(object_tree) = &mut self.object_tree else {
            return;
        };

        let node = object_tree
            .node_at_line(selected)
            .expect("The selected line should always point to a node.");

        match &mut node.kind {
            ObjectTreeNodeKind::Object {
                display_expanded, ..
            } => {
                // TODO: Find a good UX to distinguish between
                // "collapse this node" and "re-read this node".
                *display_expanded = !*display_expanded;
            }
            _ => {
                node.should_read = !node.should_read;
            }
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

impl ObjectTreeNode {
    fn num_lines(&self) -> usize {
        match &self.kind {
            ObjectTreeNodeKind::Object {
                display_expanded: true,
                fields,
                ..
            } => {
                let child_lines: usize =
                    fields.iter().map(|field| field.num_lines()).sum();
                child_lines + 2
            }
            _ => 1,
        }
    }

    fn iter(&self) -> impl Iterator<Item = (&Self, bool)> + '_ {
        let fields: Option<Box<dyn Iterator<Item = (&Self, bool)>>> =
            match &self.kind {
                ObjectTreeNodeKind::Object {
                    display_expanded: true,
                    fields,
                    ..
                } => {
                    let iter = fields
                        .iter()
                        .flat_map(|field| field.iter())
                        .chain(std::iter::once((self, false)));
                    Some(Box::new(iter))
                }
                _ => None,
            };

        std::iter::once((self, true)).chain(fields.into_iter().flatten())
    }

    fn node_at_line<'a>(
        &'a mut self,
        mut i_line: usize,
    ) -> Option<&'a mut Self> {
        let num_lines = self.num_lines();
        if i_line >= num_lines {
            return None;
        }
        if i_line == 0 {
            Some(self)
        } else if i_line + 1 < num_lines {
            let ObjectTreeNodeKind::Object {
                display_expanded: true,
                fields,
                ..
            } = &mut self.kind
            else {
                panic!(
                    "This conditional should only be reached \
                     if this is a multi-line object."
                )
            };

            i_line -= 1;
            for field in fields {
                let field_lines = field.num_lines();
                if i_line < field_lines {
                    return field.node_at_line(i_line);
                }
                i_line -= field_lines;
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

    fn initial_field(
        container: FieldContainer,
        method_table_ptr: TypedPointer<MethodTable>,
        tree_depth: usize,
        field: FieldDescription,
        reader: &CachedReader,
    ) -> Result<ObjectTreeNode, Error> {
        let method_table = reader.method_table(method_table_ptr)?;
        let obj_module = reader.runtime_module(method_table.module())?;

        let runtime_type = reader.field_to_runtime_type(&field)?;

        let field_name = reader.field_to_name(&field)?.to_string();
        let field_type = reader.field_to_type_name(&field)?.to_string();

        let location = field.location(obj_module, container, reader)?;

        let kind = match runtime_type {
            dotnet_debugger::RuntimeType::Prim(_)
            | dotnet_debugger::RuntimeType::Class => {
                ObjectTreeNodeKind::NewValue
            }
            dotnet_debugger::RuntimeType::ValueType {
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
                    .map(|subfield| {
                        Self::initial_field(
                            FieldContainer::ValueType(location.start),
                            field_method_table,
                            tree_depth + 1,
                            subfield,
                            reader,
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                ObjectTreeNodeKind::Object {
                    display_expanded: true,
                    class_name,
                    fields,
                }
            }
        };

        Ok(ObjectTreeNode {
            kind,
            location,
            runtime_type,
            should_read: false,
            tree_depth: tree_depth + 1,
            field_name,
            field_type,
        })
    }

    fn expand_marked(&mut self, reader: &CachedReader) -> Result<(), Error> {
        if self.should_read {
            self.should_read = false;
            match &mut self.kind {
                ObjectTreeNodeKind::NewValue => {
                    let value = reader
                        .value(self.runtime_type, self.location.clone())?;
                    self.kind = ObjectTreeNodeKind::Value(value);
                }
                ObjectTreeNodeKind::Object { .. } => {
                    let value = reader
                        .value(RuntimeType::Class, self.location.clone())?;
                    self.kind = ObjectTreeNodeKind::Value(value);
                }

                ObjectTreeNodeKind::Value(RuntimeValue::Object(ptr)) => {
                    if ptr.is_null() {
                        return Err(Error::CannotExpandNullField);
                    }

                    let obj = reader.object((*ptr).into())?;
                    let class_name = reader
                        .method_table_to_name(obj.method_table())?
                        .to_string();

                    let instance_location = obj.location();

                    let fields = reader
                        .iter_instance_fields(obj.method_table())?
                        .map(|field| -> Result<_, Error> {
                            Self::initial_field(
                                FieldContainer::Class(instance_location.into()),
                                obj.method_table(),
                                self.tree_depth + 1,
                                field,
                                reader,
                            )
                        })
                        .collect::<Result<Vec<_>, Error>>()?;

                    self.kind = ObjectTreeNodeKind::Object {
                        class_name,
                        fields,
                        display_expanded: true,
                    };
                }

                ObjectTreeNodeKind::Value { .. } => {
                    return Err(Error::NotImplementedYet(
                    "Collapse view of containing object when value selected"
                        .to_string(),
                ));
                }
            }
        } else if let ObjectTreeNodeKind::Object { fields, .. } = &mut self.kind
        {
            fields
                .iter_mut()
                .try_for_each(|field| field.expand_marked(reader))?;
        }

        Ok(())
    }

    fn format_str(&self, is_start: bool) -> String {
        let indent = Indent(self.tree_depth * 4);
        let field_name = self.field_name.as_str();
        let field_type = self.field_type.as_str();
        let ptr = self.location.start;

        match (&self.kind, is_start) {
            (ObjectTreeNodeKind::NewValue, _) => {
                format!("{indent}{field_type} {field_name} @ {ptr};")
            }
            (ObjectTreeNodeKind::Value(value), _) => {
                format!("{indent}{field_type} {field_name} = {value};")
            }

            (
                ObjectTreeNodeKind::Object {
                    display_expanded: false,
                    class_name,
                    ..
                },
                _,
            ) => format!(
                "{indent}{field_type} {field_name} = {class_name} {{...}}"
            ),
            (
                ObjectTreeNodeKind::Object {
                    display_expanded: true,
                    class_name,
                    ..
                },
                true,
            ) => format!("{indent}{field_type} {field_name} = {class_name} {{"),
            (
                ObjectTreeNodeKind::Object {
                    display_expanded: true,
                    ..
                },
                false,
            ) => format!("{indent}}}"),
        }
    }

    fn iter_lines(&self) -> impl Iterator<Item = String> + '_ {
        self.iter()
            .map(|(field, is_start)| field.format_str(is_start))
    }
}

impl WidgetWindow for ObjectExplorer {
    fn title(&self) -> std::borrow::Cow<str> {
        "ObjectExplorer".into()
    }

    fn apply_key_binding<'a>(
        &'a mut self,
        keystrokes: &'a crate::KeySequence,
        _globals: crate::extended_tui::WidgetGlobals<'a>,
        side_effects: &'a mut crate::extended_tui::WidgetSideEffects,
    ) -> KeyBindingMatch {
        let num_lines = self
            .object_tree
            .as_ref()
            .map(|obj| obj.num_lines())
            .unwrap_or(1);

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
                                            .filter_map(|node| {
                                                node.iter().nth(i_line).map(
                                                    |(field, is_start)| {
                                                        field.format_str(
                                                            is_start,
                                                        )
                                                    },
                                                )
                                            })
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
        globals: crate::extended_tui::WidgetGlobals<'a>,
        _side_effects: &'a mut crate::extended_tui::WidgetSideEffects,
    ) -> Result<(), Error> {
        if let Some(obj) = &mut self.object_tree {
            let reader = self.state.cached_reader(globals.reader);
            obj.expand_marked(&reader)?;
        }
        Ok(())
    }

    fn draw<'a>(
        &'a mut self,
        _globals: crate::extended_tui::WidgetGlobals<'a>,
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

        let (area_sidebar, area) = area.split_from_left(5);

        let lines = self
            .object_tree
            .iter()
            .flat_map(|node| node.iter_lines())
            .map(|text| Line::raw(text))
            .map(|line| {
                line.style_regex(
                    "0x[0-9a-fA-F]+",
                    Style::default().fg(Color::Red),
                )
            })
            .map(|line| {
                if let Some(search) = self.search.as_ref() {
                    search.highlight_search_matches(line)
                } else {
                    line
                }
            });

        let widget = List::new(lines)
            .highlight_style(Style::default().add_modifier(Modifier::REVERSED))
            .with_scrollbar(
                self.object_tree
                    .as_ref()
                    .map(|tree| tree.num_lines())
                    .unwrap_or(1),
            );

        StatefulWidget::render(widget, area, buf, &mut self.list_state);

        {
            let lines = self
                .object_tree
                .iter()
                .flat_map(|node| node.iter_lines())
                .enumerate()
                .map(|(i, _)| Line::raw(format!("{i: >4}")));

            let widget = List::new(lines)
                .highlight_style(
                    Style::default().add_modifier(Modifier::REVERSED),
                )
                .style(Style::default().fg(ratatui::style::Color::Gray));

            StatefulWidget::render(
                widget,
                area_sidebar,
                buf,
                &mut self.list_state,
            );
        }
    }
}

use std::ops::Range;

use itertools::Itertools;
use regex::Regex;

use ratatui::{
    style::{Color, Modifier, Style},
    text::Line,
    widgets::{List, ListState, StatefulWidget, Widget as _},
};

use dotnet_debugger::{
    CachedReader, FieldContainer, FieldDescription, PersistentState,
    RuntimeType, RuntimeValue,
};
use memory_reader::{MemoryReader, Pointer};

use crate::{
    extended_tui::{
        ScrollableState as _, SearchDirection, SearchWindow, WidgetWindow,
    },
    extensions::*,
};
use crate::{Error, KeyBindingMatch, UserConfig};

pub struct ObjectExplorer {
    state: PersistentState,
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
    pub(crate) fn new(
        user_config: &UserConfig,
        reader: &MemoryReader,
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

        let state = PersistentState::new().init_dlls(reader)?;

        let reader = state.cached_reader(reader);
        let static_fields = reader
            .iter_known_modules()
            .flat_map_ok(|module_ptr| {
                reader.runtime_module(module_ptr).and_then(|module| {
                    module.iter_method_table_pointers(&reader)
                })
            })
            .flat_map_ok(|method_table_ptr| {
                reader.iter_static_fields(method_table_ptr)
            })
            .sorted_by_key(|res_field| {
                display_options.sort_key(res_field, &reader)
            })
            // TODO: Allow a configurable hiding of static fields
            // (e.g. a lot of the compiler-generated fields.
            .map(|res_field| -> Result<_, Error> {
                let field = res_field?;
                let node = ObjectTreeNode::initial_field(
                    FieldContainer::Static,
                    1,
                    field,
                    &reader,
                    &display_options,
                )?;
                Ok(node)
            })
            .collect::<Result<Vec<_>, _>>()?;

        // TODO: Make a better display, since displaying the static
        // fields as being in a single class is incorrect.
        let object_tree = ObjectTreeNode {
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
        };

        Ok(ObjectExplorer {
            state,
            object_tree,
            list_state: ListState::default().with_selected(Some(0)),
            prev_draw_height: 1,
            search: None,
            display_options,
        })
    }

    fn selected_node(&self) -> Option<&ObjectTreeNode> {
        self.list_state.selected().and_then(|selected| {
            self.object_tree.iter().map(|(node, _)| node).nth(selected)
        })
    }

    fn toggle_expansion(&mut self) {
        let selected = self
            .list_state
            .selected()
            .expect("ObjectExplorer should always have a selected line.");

        let node = self
            .object_tree
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
        tree_depth: usize,
        field: FieldDescription,
        reader: &CachedReader,
        display_options: &DisplayOptions,
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

        let field_type = reader.field_to_type_name(&field)?.to_string();

        let method_table = reader.method_table(field.method_table())?;
        let module = reader.runtime_module(method_table.module())?;
        let location = field.location(module, container, reader)?;

        let runtime_type = reader.field_to_runtime_type(&field)?;
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
                    .sorted_by_key(|subfield| {
                        display_options
                            .sort_key(&Ok::<_, Error>(*subfield), reader)
                    })
                    .map(|subfield| {
                        Self::initial_field(
                            FieldContainer::ValueType(location.start),
                            tree_depth + 1,
                            subfield,
                            reader,
                            display_options,
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

    fn expand_marked(
        &mut self,
        display_options: &DisplayOptions,
        reader: &CachedReader,
    ) -> Result<(), Error> {
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
                        .sorted_by_key(|field| {
                            display_options
                                .sort_key(&Ok::<_, Error>(*field), reader)
                        })
                        .map(|field| -> Result<_, Error> {
                            Self::initial_field(
                                FieldContainer::Class(instance_location.into()),
                                self.tree_depth + 1,
                                field,
                                reader,
                                display_options,
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
            fields.iter_mut().try_for_each(|field| {
                field.expand_marked(display_options, reader)
            })?;
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
        _globals: &'a crate::TuiGlobals,
        side_effects: &'a mut crate::extended_tui::WidgetSideEffects,
    ) -> KeyBindingMatch {
        let num_lines = self.object_tree.num_lines();

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
                                            .map(|(field, is_start)| {
                                                field.format_str(is_start)
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
        globals: &'a crate::TuiGlobals,
        _side_effects: &'a mut crate::extended_tui::WidgetSideEffects,
    ) -> Result<(), Error> {
        let reader = self.state.cached_reader(&globals.reader);
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

        let (area_sidebar, area) = area.split_from_left(5);

        let display_range = {
            let start = self.list_state.offset();
            let num_lines = area.height as usize;
            start..start + num_lines
        };

        let lines = self
            .object_tree
            .iter_lines()
            .map(|text| Line::raw(text))
            .enumerate()
            .map(|(i, mut line)| {
                if display_range.contains(&i) {
                    line = line.style_regex(
                        "0x[0-9a-fA-F]+",
                        Style::default().fg(Color::Red),
                    );
                }

                if display_range.contains(&i) {
                    if let Some(search) = self.search.as_ref() {
                        line = search.highlight_search_matches(line);
                    }
                }

                line
            });

        let widget = List::new(lines)
            .highlight_style(Style::default().add_modifier(Modifier::REVERSED))
            .with_scrollbar(self.object_tree.num_lines());

        StatefulWidget::render(widget, area, buf, &mut self.list_state);

        {
            let lines = self
                .object_tree
                .iter_lines()
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

impl DisplayOptions {
    fn sort_key<E>(
        &self,
        res_field: &Result<FieldDescription, E>,
        reader: &CachedReader,
    ) -> impl Ord {
        res_field
            .as_ref()
            .map_err(|_| Error::NotImplementedYet("".to_string()))
            .and_then(|field| {
                let class_name =
                    reader.method_table_to_name(field.method_table())?;
                let field_name = reader.field_to_name(&field)?.to_string();
                let field_type = reader.field_to_type_name(&field)?.to_string();

                let static_str = if field.is_static() { "static " } else { "" };

                let search_string = format!(
                    "{static_str}{field_type} {class_name}.{field_name}"
                );

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
            })
            .unwrap_or_else(|_| (true, 0, 0))
    }
}

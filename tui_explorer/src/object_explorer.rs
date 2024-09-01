use std::ops::Range;

use dotnet_debugger::{
    CachedReader, PersistentState, RuntimeObject, RuntimeType, RuntimeValue,
    TypedPointer,
};
use memory_reader::Pointer;
use ratatui::{
    style::{Modifier, Style},
    text::Line,
    widgets::{List, ListState, StatefulWidget},
};

use crate::extensions::*;
use crate::{
    extended_tui::{ScrollableState as _, WidgetWindow},
    Error, KeyBindingMatch,
};

pub struct ObjectExplorer {
    state: PersistentState,
    object_tree: Option<ObjectTreeNode>,
    list_state: ListState,
    prev_draw_height: usize,
}

enum ObjectTreeNode {
    NewValue {
        runtime_type: RuntimeType,
        location: Range<Pointer>,
        should_read: bool,
    },
    Value {
        value: RuntimeValue,
        should_attempt_expand: bool,
    },
    Object {
        display_expanded: bool,
        obj: RuntimeObject,
        class_name: String,
        fields: Vec<ObjectTreeField>,
    },
}

struct ObjectTreeField {
    field_name: String,
    field_type: String,
    obj: ObjectTreeNode,
}

#[derive(Clone, Copy)]
struct Indent(usize);

impl ObjectExplorer {
    pub fn new(top_object: Option<TypedPointer<RuntimeObject>>) -> Self {
        let object_tree = top_object.map(|ptr| {
            let ptr: Pointer = ptr.into();

            // ObjectTreeNode::NewValue {
            //     runtime_type: RuntimeType::Class,
            //     location: ptr..ptr + Pointer::SIZE,
            //     should_read: false,
            // }

            ObjectTreeNode::Value {
                value: RuntimeValue::Object(ptr),
                should_attempt_expand: false,
            }
        });
        ObjectExplorer {
            state: PersistentState::new(),
            object_tree,
            list_state: ListState::default().with_selected(Some(0)),
            prev_draw_height: 1,
        }
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

        match node {
            ObjectTreeNode::NewValue { should_read, .. } => {
                *should_read = !*should_read;
            }
            ObjectTreeNode::Object {
                display_expanded, ..
            } => {
                *display_expanded = !*display_expanded;
            }
            ObjectTreeNode::Value {
                should_attempt_expand,
                ..
            } => {
                *should_attempt_expand = !*should_attempt_expand;
            }
        }
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
    fn expand_marked(
        &mut self,
        reader: &mut CachedReader,
    ) -> Result<(), Error> {
        match self {
            ObjectTreeNode::NewValue {
                runtime_type,
                location,
                should_read: should_read @ true,
            } => {
                *should_read = false;
                let value = reader.value(*runtime_type, location.clone())?;
                *self = ObjectTreeNode::Value {
                    value,
                    should_attempt_expand: false,
                };
            }
            ObjectTreeNode::Object { fields, .. } => fields
                .iter_mut()
                .try_for_each(|field| field.obj.expand_marked(reader))?,

            ObjectTreeNode::Value {
                should_attempt_expand: should_attempt_expand @ true,
                value: RuntimeValue::Object(ptr),
            } => {
                *should_attempt_expand = false;
                if ptr.is_null() {
                    return Err(Error::CannotExpandNullField);
                }

                let obj = reader.object((*ptr).into())?;
                let class_name = reader.class_name(&obj)?;
                let fields = reader
                    .iter_fields(&obj)?
                    .map(|(obj_module, field, field_metadata)| {
                        let runtime_type = field.runtime_type()?;

                        let field_name = field_metadata.name()?.to_string();
                        let is_static_str = if field_metadata.is_static()? {
                            "static "
                        } else {
                            ""
                        };
                        let signature = field_metadata.signature()?;
                        let field_type = format!("{is_static_str}{signature}");

                        let location =
                            field.location(obj_module, Some(obj.location()))?;

                        Ok(ObjectTreeField {
                            field_name,
                            field_type,
                            obj: ObjectTreeNode::NewValue {
                                runtime_type,
                                location,
                                should_read: false,
                            },
                        })
                    })
                    .collect::<Result<_, Error>>()?;

                *self = ObjectTreeNode::Object {
                    obj,
                    class_name,
                    fields,
                    display_expanded: true,
                };
            }

            ObjectTreeNode::Value {
                should_attempt_expand: should_attempt_expand @ true,
                ..
            } => {
                *should_attempt_expand = false;
                return Err(Error::NotImplementedYet(
                    "Collapse view of containing object when value selected"
                        .to_string(),
                ));
            }

            _ => {}
        }

        Ok(())
    }

    fn num_lines(&self) -> usize {
        match self {
            ObjectTreeNode::Object {
                display_expanded: true,
                fields,
                ..
            } => {
                2 + fields
                    .iter()
                    .map(|field| field.obj.num_lines())
                    .sum::<usize>()
            }
            _ => 1,
        }
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
            let Self::Object {
                display_expanded: true,
                fields,
                ..
            } = self
            else {
                panic!(
                    "This conditional should only be reached \
                     if this is a multi-line object."
                )
            };

            i_line -= 1;
            for field in fields {
                let field_lines = field.obj.num_lines();
                if i_line < field_lines {
                    return field.obj.node_at_line(i_line);
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

    fn iter_lines(
        &self,
        indent: Indent,
        field_name: &str,
        field_type: &str,
    ) -> impl Iterator<Item = String> + '_ {
        let iter: Box<dyn Iterator<Item = String>> = match self {
            ObjectTreeNode::NewValue { location, .. } => {
                let ptr = location.start;
                let iter =
                    [format!("{indent}{field_type} {field_name} @ {ptr};")]
                        .into_iter();
                Box::new(iter)
            }
            ObjectTreeNode::Value { value, .. } => {
                let iter =
                    [format!("{indent}{field_type} {field_name} = {value};")]
                        .into_iter();
                Box::new(iter)
            }
            ObjectTreeNode::Object {
                display_expanded: false,
                class_name,
                ..
            } => {
                let iter = [format!(
                    "{indent}{field_type} {field_name} = {class_name} {{...}}"
                )]
                .into_iter();
                Box::new(iter)
            }
            ObjectTreeNode::Object {
                class_name, fields, ..
            } => {
                let iter = std::iter::empty::<String>()
                    .chain([format!(
                        "{indent}{field_type} {field_name} = {class_name} {{"
                    )
                    .into()])
                    .chain(fields.iter().flat_map(move |field| {
                        field.obj.iter_lines(
                            indent + 4,
                            &field.field_name,
                            &field.field_type,
                        )
                    }))
                    .chain([format!("{indent}}}")]);
                Box::new(iter)
            }
        };

        iter
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
        _side_effects: &'a mut crate::extended_tui::WidgetSideEffects,
    ) -> KeyBindingMatch {
        KeyBindingMatch::Mismatch
            .or_else(|| {
                self.list_state.apply_key_binding(
                    keystrokes,
                    self.object_tree
                        .as_ref()
                        .map(|obj| obj.num_lines())
                        .unwrap_or(1),
                    self.prev_draw_height,
                )
            })
            .or_try_binding("<tab>", keystrokes, || self.toggle_expansion())
    }

    fn periodic_update<'a>(
        &mut self,
        globals: crate::extended_tui::WidgetGlobals<'a>,
        _side_effects: &'a mut crate::extended_tui::WidgetSideEffects,
    ) -> Result<(), Error> {
        if let Some(obj) = &mut self.object_tree {
            let mut reader = self.state.cached_reader(globals.reader);
            obj.expand_marked(&mut reader)?;
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

        let (area_sidebar, area) = area.split_from_left(5);

        let lines = self
            .object_tree
            .iter()
            .flat_map(|node| {
                node.iter_lines(Indent(0), "top_object", "TopObject")
            })
            .map(|text| Line::raw(text));

        let widget = List::new(lines)
            .highlight_style(Style::default().add_modifier(Modifier::REVERSED));

        StatefulWidget::render(widget, area, buf, &mut self.list_state);

        {
            let lines = self
                .object_tree
                .iter()
                .flat_map(|node| {
                    node.iter_lines(Indent(0), "top_object", "TopObject")
                })
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

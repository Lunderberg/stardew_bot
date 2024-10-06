use std::{borrow::Borrow, ops::Range};

use itertools::Itertools;
use regex::Regex;

use ratatui::{
    style::{Color, Modifier, Style},
    text::{Line, Text},
    widgets::{List, ListState, StatefulWidget, Widget as _},
};

use dotnet_debugger::{
    CachedReader, FieldContainer, FieldDescription, MethodTable, RuntimeType,
    RuntimeValue, SymbolicAccessChain, SymbolicOperation, TypedPointer,
};
use memory_reader::{OwnedBytes, Pointer};

use crate::{
    extended_tui::{
        Indent, ScrollableState as _, SearchDirection, SearchWindow,
        WidgetSideEffects, WidgetWindow,
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
    aliases: Vec<DisplayAlias>,
}

struct DisplayAlias {
    namespace: String,
    name: String,
    field: String,
}

#[derive(Clone)]
struct ClassName {
    namespace: Option<String>,
    name: String,
    base_class: Option<String>,
}

#[derive(Clone)]
struct ObjectTreeNode {
    location: Range<Pointer>,
    runtime_type: RuntimeType,
    should_read: ShouldReadState,
    kind: ObjectTreeNodeKind,
}

#[derive(Clone)]
enum ShouldReadState {
    NoRead,
    ReadAndExpand,
    ReadAndCollapse,
    ErrOnPreviousRead,
}

#[derive(Clone)]
enum ObjectTreeNodeKind {
    Value(RuntimeValue),
    String(String),
    Array {
        items: Vec<ObjectTreeNode>,
        display_expanded: bool,
    },
    Object {
        class_name: ClassName,
        fields: Vec<ObjectTreeNode>,
        display_expanded: bool,
    },
    Field {
        field_name: String,
        field_type: String,
        is_static: bool,
        value: Box<ObjectTreeNode>,
    },
    DisplayList(Vec<ObjectTreeNode>),
    ListItem {
        index: usize,
        value: Box<ObjectTreeNode>,
    },
}

macro_rules! define_visitor_mutator {
    ($visitor:ident, $ext_trait:ident, $node_ty:ty $(,)?) => {
        trait $ext_trait {
            /// Called whenever the display moves to the next line
            fn next_display_line(&mut self) {}

            /// Called whenever the visitor moves into a deeper level of the
            /// tree.
            fn increase_tree_depth(&mut self) {}

            /// Called whenever the visitor moves out of a level of the
            /// tree.
            fn decrease_tree_depth(&mut self) {}

            /// Override the node being visited, to instead visit a child
            /// of that node.
            fn override_visit<'node>(&self, node: $node_ty) -> $node_ty {
                node
            }

            /// Returns true if the node is a leaf node, false if it may have
            /// children, or None if this extension falls back to the default
            /// behavior.
            fn is_leaf_node<'node>(&self, _node: $node_ty) -> Option<bool> {
                None
            }

            /// Called for each node, prior to recursing to any child nodes.
            fn previsit<'node>(&mut self, _node: $node_ty) {}

            /// Called for each leaf node.  A leaf node is any node for which
            /// an extension has returned true for `is_leaf_node`.
            fn visit_leaf<'node>(&mut self, _node: $node_ty) {}

            /// Called prior to recursing into a child node.  A child node is
            /// visited if all extensions return `true`.  A child node is not
            /// visited if any extension returns `false`.
            fn child_filter<'node>(&mut self, _node: $node_ty) -> bool {
                true
            }

            /// Called for each node, after recursing to any child nodes.
            fn postvisit<'node>(&mut self, _node: $node_ty) {}
        }

        struct $visitor<'v> {
            extensions: Vec<Box<dyn $ext_trait + 'v>>,
        }

        impl<'v> $visitor<'v> {
            fn new() -> Self {
                Self {
                    extensions: Vec::new(),
                }
            }

            fn with_extension(mut self, ext: impl $ext_trait + 'v) -> Self {
                self.extensions.push(Box::new(ext));
                self
            }

            fn visit<'node>(&mut self, node: $node_ty) {
                let node = self.override_visit(node);

                self.previsit(node);
                if self.is_leaf_node(node) {
                    self.visit_leaf(node);
                } else {
                    self.visit_children(node);
                }

                self.postvisit(node);
            }

            fn next_display_line(&mut self) {
                self.extensions
                    .iter_mut()
                    .for_each(|ext| ext.next_display_line());
            }

            fn increase_tree_depth(&mut self) {
                self.extensions
                    .iter_mut()
                    .for_each(|ext| ext.increase_tree_depth());
            }

            fn decrease_tree_depth(&mut self) {
                self.extensions
                    .iter_mut()
                    .for_each(|ext| ext.decrease_tree_depth());
            }

            fn override_visit<'node>(&mut self, node: $node_ty) -> $node_ty {
                self.extensions
                    .iter()
                    .fold(node, |prev, ext| ext.override_visit(prev))
            }

            fn is_leaf_node<'node>(&mut self, node: $node_ty) -> bool {
                self.extensions
                    .iter_mut()
                    .find_map(|ext| ext.is_leaf_node(node))
                    .unwrap_or_else(|| match &node.kind {
                        ObjectTreeNodeKind::Value(_) => true,
                        ObjectTreeNodeKind::String(_) => true,
                        ObjectTreeNodeKind::Array { items, .. } => {
                            items.is_empty()
                        }
                        ObjectTreeNodeKind::Object { fields, .. } => {
                            fields.is_empty()
                        }
                        ObjectTreeNodeKind::Field { .. } => false,
                        ObjectTreeNodeKind::DisplayList(values) => {
                            values.is_empty()
                        }
                        ObjectTreeNodeKind::ListItem { .. } => false,
                    })
            }

            fn previsit<'node>(&mut self, node: $node_ty) {
                self.extensions
                    .iter_mut()
                    .for_each(|ext| ext.previsit(node));
            }

            fn visit_leaf<'node>(&mut self, node: $node_ty) {
                self.extensions
                    .iter_mut()
                    .for_each(|ext| ext.visit_leaf(node));
            }

            fn child_filter<'node>(&mut self, node: $node_ty) -> bool {
                self.extensions.iter_mut().all(|ext| ext.child_filter(node))
            }

            fn visit_children<'node>(&mut self, node: $node_ty) {
                match node {
                    ObjectTreeNode {
                        kind:
                            ObjectTreeNodeKind::Array { items: values, .. }
                            | ObjectTreeNodeKind::Object { fields: values, .. },
                        ..
                    } => {
                        let mut displayed_first_child = false;
                        self.increase_tree_depth();
                        for value in values {
                            if self.child_filter(value) {
                                if !displayed_first_child {
                                    self.next_display_line();
                                    displayed_first_child = true;
                                }
                                self.visit(value);
                                self.next_display_line();
                            }
                        }
                        self.decrease_tree_depth();
                    }
                    ObjectTreeNode {
                        kind: ObjectTreeNodeKind::Field { value, .. },
                        ..
                    } => {
                        if self.child_filter(value) {
                            self.visit(value);
                        }
                    }
                    ObjectTreeNode {
                        kind: ObjectTreeNodeKind::ListItem { value, .. },
                        ..
                    } => {
                        if self.child_filter(value) {
                            self.visit(value);
                        }
                    }
                    ObjectTreeNode {
                        kind: ObjectTreeNodeKind::DisplayList(values),
                        ..
                    } => {
                        for value in values {
                            if self.child_filter(value) {
                                self.visit(value);
                                self.next_display_line();
                            }
                        }
                    }

                    ObjectTreeNode {
                        kind:
                            ObjectTreeNodeKind::Value(_)
                            | ObjectTreeNodeKind::String(_),
                        ..
                    } => {}
                }
            }

            fn postvisit<'node>(&mut self, node: $node_ty) {
                self.extensions
                    .iter_mut()
                    .for_each(|ext| ext.postvisit(node));
            }
        }
    };
}

define_visitor_mutator! {
    TreeMutator,
    TreeMutatorExtension,
    &'node mut ObjectTreeNode,
}
define_visitor_mutator! {
    TreeVisitor,
    TreeVisitorExtension,
    &'node ObjectTreeNode,
}
macro_rules! forward_visitor_as_mutator {
    ($ext:ty) => {
        impl TreeMutatorExtension for $ext {
            fn next_display_line(&mut self) {
                <$ext as TreeVisitorExtension>::next_display_line(self)
            }

            fn increase_tree_depth(&mut self) {
                <$ext as TreeVisitorExtension>::increase_tree_depth(self)
            }

            fn decrease_tree_depth(&mut self) {
                <$ext as TreeVisitorExtension>::decrease_tree_depth(self)
            }

            fn override_visit<'node>(
                &self,
                node: &'node mut ObjectTreeNode,
            ) -> &'node mut ObjectTreeNode {
                // This method cannot be implemented in terms of
                // TreeObjectVisitor.  This is also the reason why the
                // forwarding is provided as a macro, rather than as a
                // blanket trait implementation.
                node
            }

            fn is_leaf_node<'node>(
                &self,
                node: &'node mut ObjectTreeNode,
            ) -> Option<bool> {
                <$ext as TreeVisitorExtension>::is_leaf_node(self, node)
            }

            fn previsit<'node>(&mut self, node: &'node mut ObjectTreeNode) {
                <$ext as TreeVisitorExtension>::previsit(self, node)
            }

            fn visit_leaf<'node>(&mut self, node: &'node mut ObjectTreeNode) {
                <$ext as TreeVisitorExtension>::visit_leaf(self, node)
            }

            fn child_filter<'node>(
                &mut self,
                node: &'node mut ObjectTreeNode,
            ) -> bool {
                <$ext as TreeVisitorExtension>::child_filter(self, node)
            }

            fn postvisit<'node>(&mut self, node: &'node mut ObjectTreeNode) {
                <$ext as TreeVisitorExtension>::postvisit(self, node)
            }
        }
    };
}

fn get_class_name(
    method_table_ptr: TypedPointer<MethodTable>,
    reader: CachedReader<'_>,
) -> Result<ClassName, Error> {
    let method_table = reader.method_table(method_table_ptr)?;
    let module = reader.runtime_module(method_table.module())?;
    let metadata = module.metadata(reader)?;

    let type_def_token =
        method_table.token().ok_or(Error::NotImplementedYet(
            "Handle null metadata token in MethodTable".to_string(),
        ))?;
    let type_def = metadata.get(type_def_token)?;

    let namespace = Some(type_def.namespace()?)
        .filter(|namespace| !namespace.is_empty())
        .map(|namespace| namespace.to_string());

    let name = type_def.name()?.to_string();

    let base_class = type_def
        .extends()?
        .map(|base| -> Result<_, Error> {
            let name = base.name()?;
            let namespace = base.namespace()?;
            Ok((namespace, name))
        })
        .transpose()?
        .filter(|(namespace, name)| {
            *namespace != "System" || (name != "Object" && name != "ValueType")
        })
        .map(|(namespace, name)| format!("{namespace}.{name}"));

    let cls_name = ClassName {
        namespace,
        name,
        base_class,
    };

    Ok(cls_name)
}

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

            aliases: user_config
                .object_explorer_alias
                .iter()
                .filter_map(|(pattern, alias)| {
                    let (namespace, name) = pattern.rsplit_once(".")?;
                    Some(DisplayAlias {
                        namespace: namespace.into(),
                        name: name.into(),
                        field: alias.clone(),
                    })
                })
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
                let display_options_ref = &display_options;

                let module = reader.runtime_module(module_ptr)?;
                let iter_static_fields = module
                    .iter_method_table_pointers(&reader)?
                    .filter(|&method_table_ptr| {
                        // TODO: Handle unpacking/display for static
                        // fields of generic types.
                        reader
                            .method_table(method_table_ptr)
                            .map(|method_table| !method_table.has_generics())
                            .unwrap_or(true)
                    })
                    .map(move |method_table_ptr| -> Result<_, Error> {
                        let class_name =
                            get_class_name(method_table_ptr, reader)?;

                        let prefetch_statics_ref = &prefetch_statics;

                        let fields = reader
                            .iter_static_fields(method_table_ptr)?
                            .map(|field| -> Result<_, Error> {
                                let node = ObjectTreeNode::initial_field(
                                    method_table_ptr,
                                    FieldContainer::Static,
                                    field,
                                    reader,
                                    display_options_ref,
                                    prefetch_statics_ref,
                                )?;
                                Ok((field, node))
                            })
                            .collect::<Result<Vec<_>, _>>()?
                            .into_iter()
                            .sorted_by_key(|(field, _)| {
                                display_options_ref
                                    .field_sort_key(*field, reader)
                            })
                            .map(|(_, node)| node)
                            .collect::<Vec<_>>();

                        let obj = if fields.is_empty() {
                            None
                        } else {
                            Some(ObjectTreeNode {
                                kind: ObjectTreeNodeKind::Object {
                                    class_name,
                                    fields,
                                    display_expanded: true,
                                },
                                location: Pointer::null()..Pointer::null(),
                                runtime_type: RuntimeType::Class,
                                should_read: ShouldReadState::NoRead,
                            })
                        };

                        Ok(obj)
                    });

                Ok(iter_static_fields)
            })
            .map(|res| res?)
            .filter_map(|res_opt| res_opt.transpose())
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .sorted_by_key(|node| {
                let ObjectTreeNode {
                    kind: ObjectTreeNodeKind::Object { class_name, .. },
                    ..
                } = node
                else {
                    panic!("Unreachable, only objects here");
                };
                display_options.class_sort_key(class_name)
            })
            .collect::<Vec<_>>();

        let object_tree = ObjectTreeNode {
            kind: ObjectTreeNodeKind::DisplayList(static_fields),
            location: Pointer::null()..Pointer::null(),
            runtime_type: RuntimeType::Class,
            should_read: ShouldReadState::NoRead,
        };

        Ok(ObjectExplorer {
            object_tree,
            list_state: ListState::default().with_selected(Some(0)),
            prev_draw_height: 1,
            search: None,
            display_options,
        })
    }

    fn address_of_selected_node(&self) -> Option<Pointer> {
        let selected = self.list_state.selected()?;

        let mut ptr = None;
        TreeVisitor::new()
            .with_extension(FollowDisplayAlias::new(
                &self.display_options.aliases,
            ))
            .with_extension(CollapseNonExpandedNodes)
            .with_extension(AddressFinder::new(selected, &mut ptr))
            .visit(&self.object_tree);
        ptr
    }

    fn toggle_expansion(&mut self) {
        let selected = self
            .list_state
            .selected()
            .expect("ObjectExplorer should always have a selected line.");

        TreeMutator::new()
            .with_extension(FollowDisplayAlias::new(
                &self.display_options.aliases,
            ))
            .with_extension(CollapseNonExpandedNodes)
            .with_extension(ToggleDisplayExpanded::new(selected))
            .visit(&mut self.object_tree);
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

    fn num_lines(&self) -> usize {
        let mut num_lines = 0;
        TreeVisitor::new()
            .with_extension(FollowDisplayAlias::new(
                &self.display_options.aliases,
            ))
            .with_extension(CollapseNonExpandedNodes)
            .with_extension(LineCounter::new(&mut num_lines))
            .visit(&self.object_tree);
        num_lines
    }

    fn selected_node_chain(&self) -> Result<Vec<&ObjectTreeNode>, Error> {
        let selected = self
            .list_state
            .selected()
            .expect("ObjectExplorer should always have selected row.");

        let mut node_address = None;

        TreeVisitor::new()
            .with_extension(FollowDisplayAlias::new(
                &self.display_options.aliases,
            ))
            .with_extension(CollapseNonExpandedNodes)
            .with_extension(TreeNodeFinder::new(selected, &mut node_address))
            .visit(&self.object_tree);

        let node_address = node_address
            .expect("Selected line should always point to some node.");

        let mut child_index_chain = vec![0];
        TreeVisitor::new()
            .with_extension(ChildIndexChainFinder::new(
                node_address,
                &mut child_index_chain,
            ))
            .visit(&self.object_tree);

        let mut output: Vec<&ObjectTreeNode> = Vec::new();
        let mut node = &self.object_tree;
        for index in child_index_chain.iter().cloned() {
            match &node.kind {
                ObjectTreeNodeKind::Value(_) => todo!(),
                ObjectTreeNodeKind::String(_) => todo!(),
                ObjectTreeNodeKind::Field { .. } => todo!(),
                ObjectTreeNodeKind::ListItem { .. } => todo!(),
                ObjectTreeNodeKind::Array { items, .. }
                | ObjectTreeNodeKind::Object { fields: items, .. }
                | ObjectTreeNodeKind::DisplayList(items) => {
                    let item = &items[index];
                    output.push(item);
                    node = item;
                }
            }

            node = match &node.kind {
                ObjectTreeNodeKind::Field { value, .. } => value,
                ObjectTreeNodeKind::ListItem { value, .. } => value,
                _ => node,
            };
        }

        Ok(output)
    }

    fn access_chain_selected_node(&self) -> Result<SymbolicAccessChain, Error> {
        let node_chain = self.selected_node_chain()?;

        let (class_namespace, class_name, static_field_name) = {
            let ObjectTreeNodeKind::Object { class_name, .. } = &node_chain
                .get(0)
                .ok_or(Error::InvalidMetadataDisplayIndex)?
                .kind
            else {
                panic!("Outermost node should be static field");
            };

            let ObjectTreeNodeKind::Field {
                field_name,
                is_static: true,
                ..
            } = &node_chain
                .get(1)
                .ok_or(Error::InvalidMetadataDisplayIndex)?
                .kind
            else {
                panic!("Outermost node should be static field");
            };

            (
                class_name.namespace.clone(),
                class_name.name.clone(),
                field_name.clone(),
            )
        };

        let ops = node_chain[2..]
            .iter()
            .map(|node| match &node.kind {
                ObjectTreeNodeKind::Field { field_name, .. } => {
                    SymbolicOperation::Field(field_name.clone())
                }
                ObjectTreeNodeKind::ListItem { index, .. } => {
                    SymbolicOperation::IndexAccess(*index)
                }
                ObjectTreeNodeKind::Value(_) => todo!(),
                ObjectTreeNodeKind::String(_) => todo!(),
                ObjectTreeNodeKind::Array { .. } => todo!(),
                ObjectTreeNodeKind::Object { .. } => todo!(),
                ObjectTreeNodeKind::DisplayList(_) => todo!(),
            })
            .collect();

        Ok(SymbolicAccessChain {
            class_namespace,
            class_name,
            static_field_name,
            ops,
        })
    }
}

impl ObjectTreeNode {
    fn initial_value(
        location: Pointer,
        runtime_type: RuntimeType,
        reader: CachedReader<'_>,
        display_options: &DisplayOptions,
        prefetch: &[OwnedBytes],
    ) -> Result<Self, Error> {
        let location = location..location + runtime_type.size_bytes();

        let kind = match runtime_type {
            RuntimeType::FixedSizeArray { .. } => {
                todo!("Display of fixed-size array")
            }

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
                    ObjectTreeNodeKind::Value(runtime_value)
                } else {
                    return Err(Error::BytesNotFoundInPrefetch(location.start));
                }
            }
            RuntimeType::ValueType {
                method_table: field_method_table,
                ..
            } => {
                let class_name = get_class_name(field_method_table, reader)?;

                let fields = reader
                    .field_descriptions(field_method_table)?
                    .into_iter()
                    .flatten()
                    .filter(|subfield| !subfield.is_static())
                    .sorted_by_key(|subfield| {
                        display_options.field_sort_key(*subfield, reader)
                    })
                    .map(|subfield| {
                        ObjectTreeNode::initial_field(
                            field_method_table,
                            FieldContainer::ValueType(location.start),
                            subfield,
                            reader,
                            display_options,
                            prefetch,
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                ObjectTreeNodeKind::Object {
                    class_name,
                    fields,
                    display_expanded: true,
                }
            }
        };

        Ok(ObjectTreeNode {
            kind,
            location,
            runtime_type,
            should_read: ShouldReadState::NoRead,
        })
    }

    fn initial_field(
        mtable_of_parent: TypedPointer<MethodTable>,
        container: FieldContainer,
        field: FieldDescription,
        reader: CachedReader<'_>,
        display_options: &DisplayOptions,
        prefetch: &[OwnedBytes],
    ) -> Result<ObjectTreeNode, Error> {
        assert!(mtable_of_parent.as_usize() % Pointer::SIZE == 0);
        assert!(field.method_table().as_usize() % Pointer::SIZE == 0);

        let method_table = reader.method_table(field.method_table())?;
        let module = reader.runtime_module(method_table.module())?;
        let metadata = module.metadata(reader)?;
        let field_metadata = metadata.get(field.token())?;

        let field_type = format!("{}", field_metadata.signature()?);

        let field_name = field_metadata.name()?.to_string();

        let location = field.location(module, container, reader)?;

        let runtime_type =
            reader.field_to_runtime_type(mtable_of_parent, &field)?;
        let value = ObjectTreeNode::initial_value(
            location.clone(),
            runtime_type.clone(),
            reader,
            display_options,
            prefetch,
        )?;

        let node = ObjectTreeNode {
            location: location..location + runtime_type.size_bytes(),
            runtime_type,
            should_read: ShouldReadState::NoRead,
            kind: ObjectTreeNodeKind::Field {
                field_name,
                field_type,
                is_static: field.is_static(),
                value: Box::new(value),
            },
        };

        Ok(node)
    }

    fn expand_if_marked(
        &mut self,
        display_options: &DisplayOptions,
        reader: CachedReader<'_>,
    ) -> Result<(), Error> {
        let display_expanded = match self.should_read {
            ShouldReadState::NoRead | ShouldReadState::ErrOnPreviousRead => {
                return Ok(());
            }
            ShouldReadState::ReadAndExpand => true,
            ShouldReadState::ReadAndCollapse => false,
        };

        self.should_read = ShouldReadState::NoRead;

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
        match self.kind {
            ObjectTreeNodeKind::Value(RuntimeValue::Object(ptr)) => {
                let obj = reader.object(ptr)?;
                let method_table = reader.method_table(obj.method_table())?;

                let ptr: Pointer = ptr.into();
                self.runtime_type = method_table.runtime_type(reader)?;
                match self.runtime_type {
                    RuntimeType::String => {
                        self.kind = ObjectTreeNodeKind::Value(
                            RuntimeValue::String(ptr.into()),
                        );
                    }
                    RuntimeType::Array => {
                        self.kind = ObjectTreeNodeKind::Value(
                            RuntimeValue::Array(ptr.into()),
                        );
                    }
                    RuntimeType::Prim(_)
                    | RuntimeType::ValueType { .. }
                    | RuntimeType::Class
                    | RuntimeType::FixedSizeArray { .. } => {}
                }
            }
            _ => {}
        }

        // Reading each type
        match self.kind {
            ObjectTreeNodeKind::Object { .. } => {
                let value =
                    reader.value(RuntimeType::Class, self.location.clone())?;
                self.kind = ObjectTreeNodeKind::Value(value);
            }
            ObjectTreeNodeKind::Array { .. } => {
                let value =
                    reader.value(RuntimeType::Array, self.location.clone())?;
                self.kind = ObjectTreeNodeKind::Value(value);
            }

            ObjectTreeNodeKind::Value(RuntimeValue::String(ptr)) => {
                if ptr.is_null() {
                    return Err(Error::CannotExpandNullField);
                }
                self.kind = ObjectTreeNodeKind::String(
                    ptr.read(reader.borrow())?.into(),
                );
            }

            ObjectTreeNodeKind::Value(RuntimeValue::Array(ptr)) => {
                if ptr.is_null() {
                    return Err(Error::CannotExpandNullField);
                }

                let array = ptr.read(reader.borrow())?;
                let prefetch = reader.read_bytes(array.ptr_range())?;
                let prefetch = &[prefetch];

                let element_type = array.element_type();

                let items = (0..array.num_elements())
                    .map(|index| -> Result<_, Error> {
                        let location = array.element_location(index);
                        let value = ObjectTreeNode::initial_value(
                            location.start,
                            element_type.clone(),
                            reader,
                            display_options,
                            prefetch,
                        )?;

                        let node = ObjectTreeNode {
                            location: value.location.clone(),
                            runtime_type: value.runtime_type.clone(),
                            should_read: ShouldReadState::NoRead,
                            kind: ObjectTreeNodeKind::ListItem {
                                index,
                                value: Box::new(value),
                            },
                        };

                        Ok(node)
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                self.kind = ObjectTreeNodeKind::Array {
                    items,
                    display_expanded,
                };
            }

            ObjectTreeNodeKind::Value(RuntimeValue::Object(ptr)) => {
                if ptr.is_null() {
                    return Err(Error::CannotExpandNullField);
                }

                let obj = reader.object(ptr)?;

                let class_name = get_class_name(obj.method_table(), reader)?;

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
                        display_options.field_sort_key(*field, reader)
                    })
                    .map(|field| -> Result<_, Error> {
                        Self::initial_field(
                            obj.method_table(),
                            FieldContainer::Class(instance_location.into()),
                            field,
                            reader,
                            display_options,
                            prefetch,
                        )
                    })
                    .collect::<Result<Vec<_>, Error>>()?;

                self.kind = ObjectTreeNodeKind::Object {
                    class_name,
                    fields,
                    display_expanded,
                };
            }

            ObjectTreeNodeKind::DisplayList(..)
            | ObjectTreeNodeKind::Field { .. }
            | ObjectTreeNodeKind::ListItem { .. } => {}

            ObjectTreeNodeKind::Value { .. }
            | ObjectTreeNodeKind::String(_) => {
                return Err(Error::NotImplementedYet(
                    "Collapse view of containing object when value selected"
                        .to_string(),
                ));
            }
        }

        Ok(())
    }

    fn expand_marked<'a>(
        &mut self,
        display_options: &DisplayOptions,
        reader: CachedReader<'_>,
        side_effects: &'a mut WidgetSideEffects,
    ) {
        let visitor = ReadMarkedNodes {
            display_options,
            reader,
            side_effects,
            current_path: Vec::new(),
        };
        TreeMutator::new().with_extension(visitor).visit(self);
    }
}

struct ToggleDisplayExpanded {
    current_line: usize,
    selected: usize,
}
impl ToggleDisplayExpanded {
    fn new(selected: usize) -> Self {
        Self {
            current_line: 0,
            selected,
        }
    }
}

impl TreeMutatorExtension for ToggleDisplayExpanded {
    fn next_display_line(&mut self) {
        self.current_line += 1;
    }

    fn previsit(&mut self, node: &mut ObjectTreeNode) {
        if self.selected == self.current_line {
            match &mut node.kind {
                ObjectTreeNodeKind::Array {
                    display_expanded, ..
                }
                | ObjectTreeNodeKind::Object {
                    display_expanded, ..
                } => {
                    *display_expanded ^= true;
                }
                _ => {
                    node.should_read = match node.should_read {
                        ShouldReadState::NoRead
                        | ShouldReadState::ErrOnPreviousRead => {
                            ShouldReadState::ReadAndExpand
                        }
                        ShouldReadState::ReadAndExpand
                        | ShouldReadState::ReadAndCollapse => {
                            ShouldReadState::NoRead
                        }
                    };
                }
            }
        }
    }
}

struct ToggleReadOnDisplayedNodes {
    current_line: usize,
    display_region: Range<usize>,
}
impl ToggleReadOnDisplayedNodes {
    fn new(display_region: Range<usize>) -> Self {
        Self {
            current_line: 0,
            display_region,
        }
    }
}
impl TreeMutatorExtension for ToggleReadOnDisplayedNodes {
    fn next_display_line(&mut self) {
        self.current_line += 1;
    }

    fn visit_leaf<'node>(&mut self, node: &'node mut ObjectTreeNode) {
        if self.display_region.contains(&self.current_line) {
            let can_expand = match node.kind {
                ObjectTreeNodeKind::Value(RuntimeValue::Object(ptr))
                    if !ptr.is_null() =>
                {
                    true
                }
                ObjectTreeNodeKind::Value(RuntimeValue::Array(ptr))
                    if !ptr.is_null() =>
                {
                    true
                }
                ObjectTreeNodeKind::Value(RuntimeValue::String(ptr))
                    if !ptr.is_null() =>
                {
                    true
                }
                _ => false,
            };
            if can_expand {
                node.should_read = match node.should_read.clone() {
                    ShouldReadState::NoRead => ShouldReadState::ReadAndCollapse,
                    other => other,
                };
            }
        }
    }
}

struct ReadMarkedNodes<'a> {
    display_options: &'a DisplayOptions,
    reader: CachedReader<'a>,
    side_effects: &'a mut WidgetSideEffects,
    current_path: Vec<String>,
}
impl<'a> TreeMutatorExtension for ReadMarkedNodes<'a> {
    fn previsit(&mut self, node: &mut ObjectTreeNode) {
        match &node.kind {
            ObjectTreeNodeKind::Field { field_name, .. } => {
                self.current_path.push(field_name.clone());
            }
            _ => {}
        }

        let res = node.expand_if_marked(self.display_options, self.reader);
        if let Err(err) = res {
            node.should_read = ShouldReadState::ErrOnPreviousRead;
            let path = self.current_path.iter().join(".");
            self.side_effects.add_log(format!("    {err}"));
            self.side_effects.add_log(format!("Error reading {path}"));
        }
    }

    fn postvisit(&mut self, node: &mut ObjectTreeNode) {
        match &node.kind {
            ObjectTreeNodeKind::Field { .. } => {
                self.current_path.pop();
            }
            _ => {}
        }
    }
}

struct FollowDisplayAlias<'a> {
    aliases: &'a [DisplayAlias],
}

impl<'a> FollowDisplayAlias<'a> {
    fn new(aliases: &'a [DisplayAlias]) -> Self {
        Self { aliases }
    }

    fn find_valid_alias(&self, node: &ObjectTreeNode) -> Option<usize> {
        match &node.kind {
            ObjectTreeNodeKind::Object {
                class_name, fields, ..
            } => self
                .aliases
                .iter()
                .find(|alias| {
                    let correct_namespace = match &class_name.namespace {
                        Some(namespace) => &alias.namespace == namespace,
                        None => alias.namespace.is_empty(),
                    };
                    let correct_name = alias.name == class_name.name;
                    correct_namespace && correct_name
                })
                .and_then(|alias| {
                    fields
                        .iter()
                        .enumerate()
                        .find(|(_, field)| match &field.kind {
                            ObjectTreeNodeKind::Field {
                                field_name, ..
                            } => field_name == &alias.field,
                            _ => false,
                        })
                        .map(|(i, _)| i)
                }),
            _ => None,
        }
    }
}

impl TreeMutatorExtension for FollowDisplayAlias<'_> {
    fn override_visit<'a>(
        &self,
        node: &'a mut ObjectTreeNode,
    ) -> &'a mut ObjectTreeNode {
        let opt_field_index: Option<usize> = self.find_valid_alias(node);

        if let Some(field_index) = opt_field_index {
            match &mut node.kind {
                ObjectTreeNodeKind::Object { fields, .. } => {
                    match &mut fields[field_index].kind {
                        ObjectTreeNodeKind::Field { value, .. } => {
                            // Recursively visit in case the alias
                            // points to another alias.
                            <Self as TreeMutatorExtension>::override_visit(
                                self,
                                value.as_mut(),
                            )
                        }
                        _ => panic!("Unreachable due to earlier check"),
                    }
                }
                _ => panic!("Unreachable due to earlier check"),
            }
        } else {
            node
        }
    }
}

impl TreeVisitorExtension for FollowDisplayAlias<'_> {
    fn override_visit<'a>(
        &self,
        node: &'a ObjectTreeNode,
    ) -> &'a ObjectTreeNode {
        let opt_field_index: Option<usize> = self.find_valid_alias(node);

        if let Some(field_index) = opt_field_index {
            match &node.kind {
                ObjectTreeNodeKind::Object { fields, .. } => {
                    match &fields[field_index].kind {
                        ObjectTreeNodeKind::Field { value, .. } => {
                            // Recursively visit in case the alias
                            // points to another alias.
                            <Self as TreeVisitorExtension>::override_visit(
                                self,
                                value.as_ref(),
                            )
                        }
                        _ => panic!("Unreachable due to earlier check"),
                    }
                }
                _ => panic!("Unreachable due to earlier check"),
            }
        } else {
            node
        }
    }
}

struct CollapseNonExpandedNodes;
impl TreeVisitorExtension for CollapseNonExpandedNodes {
    fn is_leaf_node<'node>(&self, node: &'node ObjectTreeNode) -> Option<bool> {
        match &node.kind {
            ObjectTreeNodeKind::Object {
                display_expanded, ..
            }
            | ObjectTreeNodeKind::Array {
                display_expanded, ..
            } if !display_expanded => Some(true),
            _ => None,
        }
    }
}
forward_visitor_as_mutator! {CollapseNonExpandedNodes}

struct LineCounter<'a> {
    num_lines: &'a mut usize,
}
impl<'a> LineCounter<'a> {
    fn new(num_lines: &'a mut usize) -> Self {
        Self { num_lines }
    }
}
impl<'a> TreeVisitorExtension for LineCounter<'a> {
    fn next_display_line(&mut self) {
        *self.num_lines += 1;
    }
}

struct AddressFinder<'a> {
    selected: usize,
    current_line: usize,
    ptr: &'a mut Option<Pointer>,
}
impl<'a> AddressFinder<'a> {
    fn new(selected: usize, ptr: &'a mut Option<Pointer>) -> Self {
        Self {
            selected,
            current_line: 0,
            ptr,
        }
    }
}
impl<'a> TreeVisitorExtension for AddressFinder<'a> {
    fn next_display_line(&mut self) {
        self.current_line += 1;
    }

    fn previsit<'node>(&mut self, node: &'node ObjectTreeNode) {
        if self.selected == self.current_line {
            *self.ptr = Some(node.location.start);
        }
    }
}

struct TreeNodeFinder<'a> {
    selected: usize,
    current_line: usize,
    result: &'a mut Option<*const ObjectTreeNode>,
}
impl<'a> TreeNodeFinder<'a> {
    fn new(
        selected: usize,
        result: &'a mut Option<*const ObjectTreeNode>,
    ) -> Self {
        Self {
            selected,
            current_line: 0,
            result,
        }
    }
}
impl<'a> TreeVisitorExtension for TreeNodeFinder<'a> {
    fn next_display_line(&mut self) {
        self.current_line += 1;
    }
    fn previsit(&mut self, node: &ObjectTreeNode) {
        if self.result.is_none() && self.current_line == self.selected {
            *self.result = Some(node as *const _);
        }
    }
    fn postvisit(&mut self, node: &ObjectTreeNode) {
        if self.current_line == self.selected {
            *self.result = Some(node as *const _);
        }
    }
}

struct ChildIndexChainFinder<'a> {
    target: Option<*const ObjectTreeNode>,
    child_index_chain: &'a mut Vec<usize>,
}

impl<'a> ChildIndexChainFinder<'a> {
    fn new(
        target: *const ObjectTreeNode,
        child_index_chain: &'a mut Vec<usize>,
    ) -> Self {
        Self {
            target: Some(target),
            child_index_chain,
        }
    }
}

impl<'a> TreeVisitorExtension for ChildIndexChainFinder<'a> {
    fn increase_tree_depth(&mut self) {
        if self.target.is_some() {
            self.child_index_chain.push(0);
        }
    }

    fn decrease_tree_depth(&mut self) {
        if self.target.is_some() {
            self.child_index_chain
                .pop()
                .expect("Should never have an empty index chain");
        }
    }

    fn previsit(&mut self, node: &ObjectTreeNode) {
        if self.target == Some(node as *const _) {
            self.target = None;
        }
    }
    fn postvisit(&mut self, node: &ObjectTreeNode) {
        if self.target == Some(node as *const _) {
            self.target = None;
        }

        if self.target.is_some() {
            let offset = match node.kind {
                ObjectTreeNodeKind::Field { .. } => 1,
                ObjectTreeNodeKind::ListItem { .. } => 1,
                ObjectTreeNodeKind::Value(_) => 0,
                ObjectTreeNodeKind::String(_) => 0,
                ObjectTreeNodeKind::Array { .. } => 0,
                ObjectTreeNodeKind::Object { .. } => 0,
                ObjectTreeNodeKind::DisplayList(_) => 0,
            };
            *self.child_index_chain.last_mut().unwrap() += offset;
        }
    }
}

struct LineCollector {
    lines: Vec<String>,
    tree_depth: usize,
    display_range: Range<usize>,
}
impl LineCollector {
    fn new(display_range: Range<usize>) -> Self {
        Self {
            lines: vec![String::default()],
            tree_depth: 0,
            display_range,
        }
    }

    fn current_line_is_displayed(&self) -> bool {
        let line_num = self.lines.len() - 1;
        self.display_range.contains(&line_num)
    }

    fn push(&mut self, val: impl std::fmt::Display) {
        let line = self
            .lines
            .last_mut()
            .expect("Vector of lines should always be non-empty");

        if line.is_empty() {
            let indent = Indent(self.tree_depth * 4);
            line.push_str(&format!("{indent}{val}"));
        } else {
            line.push_str(&format!("{val}"));
        }
    }
}

impl<'a> TreeVisitorExtension for &'a mut LineCollector {
    fn next_display_line(&mut self) {
        self.lines.push(String::new());
    }
    fn increase_tree_depth(&mut self) {
        self.tree_depth += 1;
    }
    fn decrease_tree_depth(&mut self) {
        self.tree_depth -= 1;
    }

    fn previsit(&mut self, node: &ObjectTreeNode) {
        if !self.current_line_is_displayed() {
            return;
        }
        match &node.kind {
            ObjectTreeNodeKind::Array { .. } => self.push("["),
            ObjectTreeNodeKind::Object { class_name, .. } => {
                if let Some(namespace) = &class_name.namespace {
                    self.push(format!("{namespace}."));
                }

                self.push(&class_name.name);
                self.push(" ");

                if let Some(base) = &class_name.base_class {
                    self.push(format!("extends {base} "));
                }
                self.push("{");
            }
            ObjectTreeNodeKind::Field {
                field_name,
                field_type,
                is_static,
                ..
            } => {
                if *is_static {
                    self.push("static ");
                }
                self.push(format!("{field_type} {field_name} = "))
            }
            _ => {}
        }
    }

    fn visit_leaf(&mut self, node: &ObjectTreeNode) {
        if !self.current_line_is_displayed() {
            return;
        }
        match &node.kind {
            ObjectTreeNodeKind::Value(val) => self.push(val),
            ObjectTreeNodeKind::String(val) => self.push(format!("{val:?}")),

            ObjectTreeNodeKind::Array { items, .. } if !items.is_empty() => {
                self.push("...")
            }
            ObjectTreeNodeKind::Object { fields, .. } if !fields.is_empty() => {
                self.push("...")
            }
            _ => {}
        }
    }

    fn postvisit<'node>(&mut self, node: &ObjectTreeNode) {
        if !self.current_line_is_displayed() {
            return;
        }
        match &node.kind {
            ObjectTreeNodeKind::Array { .. } => self.push("]"),
            ObjectTreeNodeKind::Object { .. } => self.push("}"),
            ObjectTreeNodeKind::Field { .. } => self.push(";"),
            _ => {}
        }
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
        let num_lines = self.num_lines();

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
                                        let mut collector = LineCollector::new(
                                            i_line..i_line + 1,
                                        );
                                        TreeVisitor::new()
                                            .with_extension(
                                                FollowDisplayAlias::new(
                                                    &self
                                                        .display_options
                                                        .aliases,
                                                ),
                                            )
                                            .with_extension(
                                                CollapseNonExpandedNodes,
                                            )
                                            .with_extension(&mut collector)
                                            .visit(&mut self.object_tree);

                                        collector
                                            .lines
                                            .into_iter()
                                            .nth(i_line)
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
                if let Some(pointer) = self.address_of_selected_node() {
                    side_effects.change_address(pointer);
                }
            })
            .or_try_binding("C-t", keystrokes, || {
                match self.access_chain_selected_node() {
                    Ok(access_chain) => side_effects
                        .add_log(format!("Symbolic access: {access_chain}")),
                    Err(err) => side_effects.add_log(format!("Err: {err}")),
                }
            })
    }

    fn periodic_update<'a>(
        &mut self,
        globals: &'a crate::TuiGlobals,
        side_effects: &'a mut WidgetSideEffects,
    ) -> Result<(), Error> {
        let reader = globals.cached_reader();

        let display_range = {
            let start = self.list_state.offset();
            start..start + self.prev_draw_height
        };
        TreeMutator::new()
            .with_extension(FollowDisplayAlias::new(
                &self.display_options.aliases,
            ))
            .with_extension(CollapseNonExpandedNodes)
            .with_extension(ToggleReadOnDisplayedNodes::new(display_range))
            .visit(&mut self.object_tree);

        self.object_tree.expand_marked(
            &self.display_options,
            reader,
            side_effects,
        );
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
        let num_lines = self.num_lines();
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

        let lines = {
            let mut collector = LineCollector::new(display_range);
            TreeVisitor::new()
                .with_extension(FollowDisplayAlias::new(
                    &self.display_options.aliases,
                ))
                .with_extension(CollapseNonExpandedNodes)
                .with_extension(&mut collector)
                .visit(&mut self.object_tree);
            collector.lines
        };
        let lines = lines.into_iter().map(|line| {
            if line.is_empty() {
                Line::default()
            } else {
                let line: Line = line.into();

                let line = line.style_regex(
                    "0x[0-9a-fA-F]+",
                    Style::default().fg(Color::LightRed),
                );

                let line = if let Some(search) = self.search.as_ref() {
                    search.highlight_search_matches(line)
                } else {
                    line
                };

                line
            }
        });

        let lines: Vec<_> = lines.collect();
        let num_lines = lines.len();

        let widget = List::new(lines)
            .highlight_style(Style::default().add_modifier(Modifier::REVERSED))
            .with_scrollbar(num_lines)
            .number_each_row();

        StatefulWidget::render(widget, area, buf, &mut self.list_state);
    }
}

impl DisplayOptions {
    fn field_sort_key(
        &self,
        field: FieldDescription,
        reader: CachedReader<'_>,
    ) -> impl Ord {
        let res_sort_key = || -> Result<_, Error> {
            let class_name =
                reader.method_table_to_name(field.method_table())?;
            let field_name = reader.field_to_name(&field)?.to_string();
            let field_type = reader.field_to_type_name(&field)?.to_string();

            let static_str = if field.is_static() { "static " } else { "" };

            let search_string =
                format!("{static_str}{field_type} {class_name}.{field_name}");

            self.string_sort_key(&search_string)
        }();
        res_sort_key.unwrap_or_else(|_| (true, 0, 0))
    }

    fn class_sort_key(&self, class_name: &ClassName) -> impl Ord {
        let res_sort_key = || -> Result<_, Error> {
            let name = &class_name.name;

            let search_string = if let Some(namespace) = &class_name.namespace {
                &format!("{namespace}.{name}")
            } else {
                name
            };

            self.string_sort_key(search_string)
        }();
        res_sort_key.unwrap_or_else(|_| (true, 0, 0))
    }

    fn string_sort_key(
        &self,
        search_string: &str,
    ) -> Result<(bool, usize, usize), Error> {
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
    }
}

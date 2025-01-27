use std::{borrow::Cow, cmp::Reverse};

use dll_unpacker::{Assembly, MetadataTableIndex, MetadataTableKind};
use dotnet_debugger::{CachedReader, RuntimeModule, TypedPointer};
use itertools::Itertools;
use memory_reader::Pointer;
use ratatui::{
    layout::Constraint,
    style::{Modifier, Style},
    text::Text,
    widgets::{Row, StatefulWidget, Table, TableState, Widget},
};
use tui_utils::{
    extensions::{SplitRect as _, WidgetWithScrollbar as _},
    inputs::{KeyBindingMatch, KeySequence},
    widgets::{Indent, ScrollableState as _, SearchDirection, SearchWindow},
    TuiGlobals, WidgetSideEffects, WidgetWindow,
};

use crate::{ChangeAddress, Error};

pub struct MetadataDisplay {
    dll_metadata: Vec<DllMetadata>,
    table_state: TableState,
    prev_draw_height: usize,
    search: Option<SearchWindow<TableState>>,
}

struct DllMetadata {
    name: String,
    runtime_module: TypedPointer<RuntimeModule>,
    tables: Vec<DisplayTable>,
    is_expanded: bool,
}

struct DisplayTable {
    kind: MetadataTableKind,
    rows: Vec<DisplayRow>,
    is_expanded: bool,
}

struct DisplayRow {
    kind: MetadataTableKind,
    index: usize,
    name: Option<String>,
    fields: Option<Vec<(Pointer, String, String)>>,
    is_expanded: bool,
}

struct TreeIteratorItem<'a> {
    tree_depth: usize,
    is_leaf: bool,
    is_expanded: bool,
    loc: Option<Pointer>,
    name: Cow<'a, str>,
    value: Option<&'a str>,
}

enum TreeItemMut<'a> {
    DLL(&'a mut DllMetadata),
    Table(&'a mut DisplayTable),
    Row(TypedPointer<RuntimeModule>, &'a mut DisplayRow),
}

#[derive(Default)]
struct FieldAnnotations(Vec<FieldAnnotation>);

struct FieldAnnotation {
    loc: Pointer,
    name: String,
    value: String,
}

impl FieldAnnotations {
    fn finalize(self) -> Vec<(Pointer, String, String)> {
        self.0
            .into_iter()
            .map(|field| (field.loc, field.name, field.value))
            .collect()
    }
}

impl dll_unpacker::Annotator for FieldAnnotations {
    fn range(
        &mut self,
        range: std::ops::Range<Pointer>,
    ) -> &mut impl dll_unpacker::Annotation {
        self.0.push(FieldAnnotation {
            loc: range.start,
            name: String::default(),
            value: String::default(),
        });
        self.0.last_mut().unwrap()
    }
}
impl dll_unpacker::Annotation for FieldAnnotation {
    fn name(&mut self, name: impl Into<String>) -> &mut Self {
        self.name = name.into();
        self
    }

    fn current_value(&self) -> &str {
        &self.value
    }

    fn value(&mut self, value: impl std::fmt::Display) -> &mut Self {
        self.value = format!("{value}");
        self
    }

    fn disable_highlight(&mut self) -> &mut Self {
        self
    }
}

impl MetadataDisplay {
    fn num_lines(&self) -> usize {
        self.dll_metadata.iter().map(|dll| dll.num_lines()).sum()
    }

    fn iter_lines(&self) -> impl Iterator<Item = TreeIteratorItem> + '_ {
        Self::iter_lines_impl(&self.dll_metadata)
    }

    fn iter_lines_impl(
        dll_metadata: &[DllMetadata],
    ) -> impl Iterator<Item = TreeIteratorItem> {
        dll_metadata.iter().flat_map(|dll| dll.iter_lines())
    }

    fn toggle_expansion(&mut self, reader: CachedReader) -> Result<(), Error> {
        let Some(selected) = self.table_state.selected() else {
            return Ok(());
        };

        self.apply_func(selected, |item| {
            match item {
                TreeItemMut::DLL(dll) => {
                    dll.is_expanded ^= true;
                }
                TreeItemMut::Table(table) => {
                    table.is_expanded ^= true;
                }
                TreeItemMut::Row(module_ptr, row) => {
                    if row.fields.is_none() {
                        let runtime_module =
                            reader.runtime_module(module_ptr)?;
                        let metadata = runtime_module.metadata(&reader)?;

                        let mut fields = FieldAnnotations::default();

                        metadata.collect_annotation_by_row(
                            &mut fields,
                            row.kind,
                            row.index,
                        )?;

                        row.fields = Some(fields.finalize());
                    }

                    row.is_expanded ^= true;
                }
            }
            Ok(())
        })
    }

    fn apply_func(
        &mut self,
        mut index: usize,
        func: impl FnOnce(TreeItemMut) -> Result<(), Error>,
    ) -> Result<(), Error> {
        for dll in self.dll_metadata.iter_mut() {
            if index == 0 {
                return func(TreeItemMut::DLL(dll));
            } else {
                index -= 1;
            }

            if dll.is_expanded {
                for table in dll.tables.iter_mut() {
                    if index == 0 {
                        return func(TreeItemMut::Table(table));
                    } else {
                        index -= 1;
                    }

                    if table.is_expanded {
                        for row in table.rows.iter_mut() {
                            let num_lines = row.num_lines();
                            if index < num_lines {
                                return func(TreeItemMut::Row(
                                    dll.runtime_module,
                                    row,
                                ));
                            } else {
                                index -= num_lines;
                            }
                        }
                    }
                }
            }
        }
        Err(Error::InvalidMetadataDisplayIndex)
    }

    fn start_search(&mut self, direction: SearchDirection) {
        self.search =
            Some(SearchWindow::new(direction, self.table_state.clone()));
    }

    fn cancel_search(&mut self) {
        if let Some(search) = self.search.take() {
            self.table_state = search.pre_search_state;
        }
    }

    fn finalize_search(&mut self) {
        self.search = None;
    }
}

impl DllMetadata {
    fn num_lines(&self) -> usize {
        let child_lines = if self.is_expanded {
            self.tables
                .iter()
                .map(|table| table.num_lines())
                .sum::<usize>()
        } else {
            0
        };
        child_lines + 1
    }

    fn iter_lines(&self) -> impl Iterator<Item = TreeIteratorItem> + '_ {
        let header = TreeIteratorItem {
            tree_depth: 0,
            is_leaf: false,
            is_expanded: self.is_expanded,
            loc: None,
            name: (&self.name).into(),
            value: None,
        };

        std::iter::once(header).chain(
            self.is_expanded
                .then(|| &self.tables)
                .into_iter()
                .flatten()
                .flat_map(|table| table.iter_lines()),
        )
    }
}

impl DisplayTable {
    fn num_lines(&self) -> usize {
        let child_lines = if self.is_expanded {
            self.rows.iter().map(|row| row.num_lines()).sum::<usize>()
        } else {
            0
        };
        child_lines + 1
    }

    fn iter_lines(&self) -> impl Iterator<Item = TreeIteratorItem> + '_ {
        let header = TreeIteratorItem {
            tree_depth: 1,
            is_leaf: false,
            is_expanded: self.is_expanded,
            loc: None,
            name: format!("{}", self.kind).into(),
            value: None,
        };
        std::iter::once(header).chain(
            self.is_expanded
                .then(|| &self.rows)
                .into_iter()
                .flatten()
                .flat_map(|row| row.iter_lines()),
        )
    }
}

impl DisplayRow {
    fn num_lines(&self) -> usize {
        let child_lines = if self.is_expanded {
            self.fields
                .as_ref()
                .expect("If expanded, DisplayRow fields should be populated")
                .len()
        } else {
            0
        };
        child_lines + 1
    }

    fn iter_lines(&self) -> impl Iterator<Item = TreeIteratorItem> + '_ {
        let header = TreeIteratorItem {
            tree_depth: 2,
            is_leaf: false,
            is_expanded: self.is_expanded,
            loc: None,
            name: format!("{}[{}]", self.kind, self.index).into(),
            value: self.name.as_ref().map(|s| s.as_str()),
        };
        std::iter::once(header).chain(
            self.fields
                .as_ref()
                .filter(|_| self.is_expanded)
                .into_iter()
                .flatten()
                .map(|(loc, name, value)| TreeIteratorItem {
                    tree_depth: 3,
                    is_leaf: true,
                    is_expanded: false,
                    loc: Some(*loc),
                    name: name.into(),
                    value: if value.is_empty() { None } else { Some(&value) },
                }),
        )
    }
}

impl MetadataDisplay {
    pub fn new(reader: CachedReader) -> Result<Self, Error> {
        let dll_metadata = reader
            .iter_known_modules()?
            .sorted_by_key(|module_ptr| {
                let get_name = || -> Result<_, Error> {
                    let module = reader.runtime_module(*module_ptr)?;
                    let metadata = module.metadata(&reader)?;

                    let assembly_index = MetadataTableIndex::<Assembly>::new(0);
                    let name =
                        metadata.get(assembly_index)?.name()?.to_string();
                    Ok(name)
                };
                let name = get_name().unwrap_or_default();
                (
                    Reverse(name.contains("Stardew")),
                    name.contains("System"),
                    name,
                )
            })
            .map(|module_ptr| -> Result<DllMetadata, Error> {
                let module = reader.runtime_module(module_ptr)?;
                let metadata = module.metadata(&reader)?;

                let assembly_index = MetadataTableIndex::<Assembly>::new(0);
                let name = metadata.get(assembly_index)?.name()?.to_string();

                let tables = metadata
                    .layout()
                    .num_rows_by_table()
                    .iter()
                    .filter(|(_, &num_rows)| num_rows > 0)
                    .map(|(kind, &num_rows)| -> Result<_, Error> {
                        let rows = (0..num_rows)
                            .map(|index| -> Result<_, Error> {
                                let name = metadata
                                    .get_name(kind, index)?
                                    .map(|name| name.to_string());
                                Ok(DisplayRow {
                                    kind,
                                    index,
                                    name,
                                    fields: None,
                                    is_expanded: false,
                                })
                            })
                            .collect::<Result<_, _>>()?;
                        Ok(DisplayTable {
                            kind,
                            rows,
                            is_expanded: false,
                        })
                    })
                    .collect::<Result<_, _>>()?;

                Ok(DllMetadata {
                    name,
                    runtime_module: module_ptr,
                    tables,
                    is_expanded: false,
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Self {
            dll_metadata,
            table_state: Default::default(),
            prev_draw_height: 1,
            search: None,
        })
    }
}

impl<'a> TreeIteratorItem<'a> {
    fn as_table_row(&self) -> impl Iterator<Item = Cow<'a, str>> + '_ {
        let indent = Indent(self.tree_depth * 4);
        let prefix = if self.is_leaf {
            ""
        } else if self.is_expanded {
            "▼"
        } else {
            "▶"
        };
        let name = format!("{indent}{prefix}{}", self.name);

        let value = self.value.unwrap_or("");

        [name.into(), value.into()].into_iter()
    }
}

impl WidgetWindow<Error> for MetadataDisplay {
    fn title(&self) -> std::borrow::Cow<str> {
        "DLL Metadata".into()
    }

    fn apply_key_binding<'a>(
        &'a mut self,
        keystrokes: &'a KeySequence,
        globals: &'a TuiGlobals,
        side_effects: &'a mut WidgetSideEffects,
    ) -> KeyBindingMatch {
        let num_lines = self.num_lines();

        KeyBindingMatch::Mismatch
            .or_else(|| {
                self.table_state
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
                                        Self::iter_lines_impl(
                                            &self.dll_metadata,
                                        )
                                        .nth(i_line)
                                        .map(|line| {
                                            line.as_table_row()
                                                .map(|cow| cow.into_owned())
                                                .collect()
                                        })
                                        .unwrap_or_default()
                                    },
                                )
                                .then(|| {
                                    self.table_state.select(Some(
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
            .or_try_binding("<tab>", keystrokes, || {
                if let Err(err) = self.toggle_expansion(globals.cached_reader())
                {
                    side_effects.add_log(format!("Error: {err}"));
                }
            })
            .or_try_binding("C-s", keystrokes, || {
                self.start_search(SearchDirection::Forward)
            })
            .or_try_binding("C-r", keystrokes, || {
                self.start_search(SearchDirection::Reverse)
            })
            .or_try_binding("<enter>", keystrokes, || {
                if let Some(selected) = self.table_state.selected() {
                    if let Some(loc) = self
                        .iter_lines()
                        .nth(selected as usize)
                        .and_then(|item| item.loc)
                    {
                        side_effects.broadcast(ChangeAddress(loc));
                    }
                }
            })
    }

    fn draw<'a>(
        &'a mut self,
        _globals: &'a TuiGlobals,
        area: ratatui::layout::Rect,
        buf: &mut ratatui::prelude::Buffer,
    ) {
        self.prev_draw_height = area.height as usize;
        let num_lines = self.num_lines();

        let area = if let Some(search) = &self.search {
            let (area, search_area) = area.split_from_bottom(3);
            search.render(search_area, buf);
            area
        } else {
            area
        };

        let column_widths =
            [Constraint::Percentage(75), Constraint::Percentage(25)];
        // Force the list_state to update, without needing to generate
        // the text for every element of the display.
        StatefulWidget::render(
            Table::new(vec![Row::default(); num_lines], column_widths),
            area,
            buf,
            &mut self.table_state,
        );

        let table: Table = Self::iter_lines_impl(&self.dll_metadata)
            .map(|line| {
                let mut height = 1;
                let row = Row::new(line.as_table_row().map(|text| {
                    let mut text = Text::raw(text);
                    height = height.max(text.height());

                    if let Some(search) = self.search.as_ref() {
                        text = text
                            .into_iter()
                            .map(|line| search.highlight_search_matches(line))
                            .collect();
                    }

                    text
                }));
                let row = row.height(height as u16);
                row
            })
            .collect();
        let table = table
            .row_highlight_style(
                Style::default().add_modifier(Modifier::REVERSED),
            )
            .with_scrollbar(num_lines);

        StatefulWidget::render(table, area, buf, &mut self.table_state);
    }
}

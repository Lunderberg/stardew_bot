use dotnet_debugger::CachedReader;
use iterator_extensions::ResultIteratorExt as _;
use ratatui::{
    layout::Constraint,
    style::{Modifier, Style},
    widgets::{Row, StatefulWidget, Table, TableState, Widget as _},
};
use tui_utils::{
    extensions::{SplitRect as _, WidgetWithScrollbar as _},
    inputs::{KeyBindingMatch, KeySequence},
    widgets::{ScrollableState as _, SearchDirection, SearchWindow},
    TuiGlobals, WidgetSideEffects, WidgetWindow,
};

use crate::Error;

pub struct RuntimeModuleView {
    modules: Vec<ModuleContents>,
    table_state: TableState,
    prev_draw_height: usize,
    search: Option<SearchWindow<TableState>>,
}

struct ModuleContents {
    name: String,
    loaded_types: Vec<String>,
}

impl RuntimeModuleView {
    pub fn new(reader: CachedReader) -> Result<Self, Error> {
        let modules = reader
            .iter_known_modules()?
            .map(|module_ptr| -> Result<_, Error> {
                let module = reader.runtime_module(module_ptr)?;
                let name =
                    module.dll_region_info(&reader)?.short_name().to_string();
                let loaded_types = module
                    .loaded_types(reader)?
                    .into_iter()
                    .map(Ok)
                    .and_flat_map_ok(|loaded_types| {
                        loaded_types.iter_method_tables(&reader)
                    })
                    .map(|res_type_handle_ptr| -> Result<_, Error> {
                        let type_handle_ptr = res_type_handle_ptr?;
                        let type_handle =
                            reader.type_handle(type_handle_ptr)?;
                        Ok(format!("{}", type_handle.printable(reader)))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(ModuleContents { name, loaded_types })
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Self {
            modules,
            table_state: TableState::default(),
            prev_draw_height: 0,
            search: None,
        })
    }

    fn num_lines(&self) -> usize {
        self.modules
            .iter()
            .map(|module| 1 + module.loaded_types.len())
            .sum()
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

impl ModuleContents {
    fn num_lines(&self) -> usize {
        1 + self.loaded_types.len()
    }

    fn get_line(&self, i_line: usize) -> String {
        if i_line == 0 {
            self.name.clone()
        } else {
            format!("    {}", self.loaded_types[i_line - 1])
        }
    }
}

impl WidgetWindow<Error> for RuntimeModuleView {
    fn title(&self) -> std::borrow::Cow<str> {
        "Runtime Modules".into()
    }

    fn apply_key_binding<'a>(
        &'a mut self,
        keystrokes: &'a KeySequence,
        _globals: &'a TuiGlobals,
        _side_effects: &'a mut WidgetSideEffects,
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
                                    |mut i_line| {
                                        for module in &self.modules {
                                            if i_line < module.num_lines() {
                                                return vec![
                                                    module.get_line(i_line)
                                                ];
                                            } else {
                                                i_line -= module.num_lines();
                                            }
                                        }
                                        Vec::new()
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
            .or_try_binding("C-s", keystrokes, || {
                self.start_search(SearchDirection::Forward)
            })
            .or_try_binding("C-r", keystrokes, || {
                self.start_search(SearchDirection::Reverse)
            })
    }

    fn draw<'a>(
        &'a mut self,
        _globals: &'a tui_utils::TuiGlobals,
        area: ratatui::layout::Rect,
        buf: &mut ratatui::prelude::Buffer,
    ) {
        let area = if let Some(search) = &self.search {
            let (area, search_area) = area.split_from_bottom(3);
            search.render(search_area, buf);
            area
        } else {
            area
        };

        self.prev_draw_height = area.height as usize;

        let rows = self
            .modules
            .iter()
            .flat_map(|module| {
                (0..module.num_lines()).map(|i_line| module.get_line(i_line))
            })
            .map(|s| Row::new([s]));
        let table = Table::new(rows, [Constraint::Percentage(100)]);

        let table = table
            .row_highlight_style(
                Style::default().add_modifier(Modifier::REVERSED),
            )
            .with_scrollbar(self.num_lines());

        StatefulWidget::render(table, area, buf, &mut self.table_state);
    }
}

use ratatui::{
    layout::{Constraint, Rect},
    style::{Color, Modifier, Style},
    text::Line,
    widgets::{Block, Borders, StatefulWidget as _, TableState, Widget},
};

use memory_reader::extensions::*;
use memory_reader::{MemoryReader, MemoryRegion, MemoryValue, Pointer};

use crate::extended_tui::{
    InputWindow, ScrollableState, SearchDirection, SearchWindow, WidgetWindow,
};
use crate::{extended_tui::DynamicTable, extensions::*};
use crate::{
    Annotation, ColumnFormatter, Error, KeyBindingMatch, KeySequence,
    NonEmptyVec, RunningLog, StackFrameTable,
};

pub struct MemoryTable {
    view_stack: NonEmptyVec<ViewFrame>,
    formatters: Vec<Box<dyn ColumnFormatter>>,
    jump_to_window: Option<InputWindow>,
}

pub(crate) struct DrawableMemoryTable<'a> {
    table: &'a mut MemoryTable,
    reader: &'a MemoryReader,
    annotations: &'a [Annotation],
}

struct ViewFrame {
    region: MemoryRegion,
    entry_point: Pointer,
    table_state: TableState,
    search: Option<SearchWindow<TableState>>,
    table_height: Option<usize>,
}

struct DrawableViewFrame<'a> {
    view: &'a mut ViewFrame,
    reader: &'a MemoryReader,
    annotations: &'a [Annotation],
    formatters: &'a [Box<dyn ColumnFormatter>],
}

impl MemoryTable {
    pub(crate) fn new(
        reader: &MemoryReader,
        entry_point: Pointer,
        formatters: Vec<Box<dyn ColumnFormatter>>,
    ) -> Result<Self, Error> {
        let region = reader
            .regions
            .iter()
            .find(|region| region.contains(entry_point))
            .ok_or(Error::PointerNotFound(entry_point))?
            .read()?;
        Ok(Self {
            view_stack: NonEmptyVec::new(ViewFrame::new(region, entry_point)),
            formatters,
            jump_to_window: None,
        })
    }

    pub(crate) fn push_view(
        &mut self,
        region: MemoryRegion,
        entry_point: Pointer,
    ) {
        self.finalize_search();
        self.view_stack.push(ViewFrame::new(region, entry_point));
    }

    pub(crate) fn pop_view(&mut self) {
        if self.view_stack.len() > 1 {
            self.view_stack.pop();
        }
    }

    pub(crate) fn start_jump_to_address(&mut self) {
        self.jump_to_window = Some(InputWindow::new("Jump to Address"))
    }

    pub(crate) fn finalize_jump_to_address(
        &mut self,
        reader: &MemoryReader,
        current_region_start: Pointer,
        log: &mut RunningLog,
        stack_frame_table: &mut StackFrameTable,
    ) {
        let Some(input_window) = self.jump_to_window.take() else {
            log.add_log("Finalization of jump when not in-progress");
            return;
        };
        let text = input_window.text();
        let text = text.as_str();

        let (opt_relative_dir, text) = match text.chars().next() {
            Some('+') => (Some(true), &text[1..]),
            Some('-') => (Some(false), &text[1..]),
            _ => (None, text),
        };

        let res_address = if text.starts_with("0x") {
            usize::from_str_radix(text.strip_prefix("0x").unwrap(), 16)
        } else {
            usize::from_str_radix(&text, 10)
        };
        let address = match res_address {
            Ok(address) => address,
            Err(err) => {
                log.add_log(format!("Invalid hex address: {err}"));
                return;
            }
        };

        let address: Pointer = if let Some(relative_dir) = opt_relative_dir {
            let current_loc = self.selected_value().location;
            if relative_dir {
                current_loc + address
            } else {
                current_loc - address
            }
        } else if address < 0x100000 {
            // Bit of a hack, should have distinct syntax for a location
            // relative to the current MemoryRegion and a global address.
            current_region_start + address
        } else {
            address.into()
        };

        // let address: Pointer = address.into();

        self.jump_to_address(address, reader, stack_frame_table, log);
    }

    pub(crate) fn jump_to_address(
        &mut self,
        address: Pointer,
        reader: &MemoryReader,
        stack_frame_table: &mut StackFrameTable,
        log: &mut RunningLog,
    ) {
        let Some(region) = reader
            .regions
            .iter()
            .find(|region| region.contains(address))
        else {
            log.add_log(format!("Address {address} not found in any region"));
            return;
        };

        let region = match region.read() {
            Ok(region) => region,
            Err(err) => {
                log.add_log(format!("Region cannot be read: {err}"));
                return;
            }
        };

        *stack_frame_table = StackFrameTable::new(reader, &region);
        self.view_stack = NonEmptyVec::new(ViewFrame::new(region, address));
    }

    pub(crate) fn current_region(&self) -> &MemoryRegion {
        &self.active_view().region
    }

    pub(crate) fn selected_value(&self) -> MemoryValue<[u8; 8]> {
        self.active_view().selected_value()
    }

    pub(crate) fn finalize_search(&mut self) {
        self.active_view_mut().finalize_search();
    }

    fn active_view(&self) -> &ViewFrame {
        self.view_stack.last()
    }

    fn active_view_mut(&mut self) -> &mut ViewFrame {
        self.view_stack.last_mut()
    }

    pub(crate) fn apply_key_binding(
        &mut self,
        keystrokes: &KeySequence,
        reader: &MemoryReader,
        log: &mut RunningLog,
        stack_frame_table: &mut StackFrameTable,
    ) -> KeyBindingMatch {
        KeyBindingMatch::Mismatch
            .or_else(|| {
                self.view_stack.last_mut().apply_key_binding(
                    keystrokes,
                    reader,
                    &self.formatters,
                )
            })
            .or_else(|| {
                if let Some(window) = self.jump_to_window.as_mut() {
                    window.apply_key_binding(keystrokes)
                } else {
                    KeyBindingMatch::Mismatch
                }
            })
            .or_else(|| {
                if self.jump_to_window.is_some() {
                    KeyBindingMatch::try_binding("C-g", keystrokes, || {
                        self.jump_to_window = None;
                    })
                } else if self.view_stack.len() > 1 {
                    KeyBindingMatch::try_binding("C-g", keystrokes, || {
                        self.pop_view()
                    })
                } else {
                    KeyBindingMatch::Mismatch
                }
            })
            .or_else(|| {
                if self.jump_to_window.is_some() {
                    KeyBindingMatch::try_binding("<enter>", keystrokes, || {
                        self.finalize_jump_to_address(
                            reader,
                            self.view_stack.last().region.start(),
                            log,
                            stack_frame_table,
                        )
                    })
                } else {
                    KeyBindingMatch::Mismatch
                }
            })
            .or_try_binding("M-g g", keystrokes, || {
                self.start_jump_to_address()
            })
            .or_try_binding("<enter>", keystrokes, || {
                let selection = self.selected_value();
                let as_pointer: Pointer = selection.value.into();
                let pointed_map_region =
                    reader.find_containing_region(as_pointer);
                if let Some(pointed_map_region) = pointed_map_region {
                    let pointed_region = pointed_map_region.read();
                    match pointed_region {
                        Ok(region) => {
                            self.push_view(region, as_pointer);
                        }
                        Err(_) => log.add_log("Error reading region"),
                    }
                } else {
                    log.add_log("Value does not point to any memory region");
                }
            })
    }

    pub(crate) fn drawable<'a>(
        &'a mut self,
        reader: &'a MemoryReader,
        annotations: &'a [Annotation],
    ) -> DrawableMemoryTable<'a> {
        DrawableMemoryTable {
            table: self,
            reader,
            annotations,
        }
    }
}

impl<'a> Widget for DrawableMemoryTable<'a> {
    fn render(self, area: Rect, buf: &mut ratatui::prelude::Buffer)
    where
        Self: Sized,
    {
        let area = if let Some(window) = self.table.jump_to_window.as_ref() {
            let (top, bottom) = area.split_from_bottom(3);
            window.render(bottom, buf);
            top
        } else {
            area
        };

        let area =
            self.table
                .view_stack
                .iter()
                .skip(1)
                .fold(area, |area, view| {
                    let border = Block::default()
                        .borders(Borders::ALL)
                        .title(view.title());
                    let inner_area = border.inner(area);
                    border.render(area, buf);
                    inner_area
                });

        self.table
            .view_stack
            .last_mut()
            .drawable(self.reader, self.annotations, &self.table.formatters)
            .render(area, buf);
    }
}

impl<'a> WidgetWindow for DrawableMemoryTable<'a> {
    fn title(&self) -> String {
        self.table.view_stack.first().title()
    }

    fn mut_render(&mut self, area: Rect, buf: &mut ratatui::prelude::Buffer) {
        DrawableMemoryTable {
            table: self.table,
            reader: self.reader,
            annotations: self.annotations,
        }
        .render(area, buf)
    }
}

impl ViewFrame {
    pub(crate) fn new(region: MemoryRegion, entry_point: Pointer) -> Self {
        let mut frame = ViewFrame {
            region,
            entry_point,
            table_state: TableState::default(),
            search: None,
            table_height: None,
        };

        frame.select_address(entry_point);
        frame
    }

    fn num_table_rows(&self) -> usize {
        self.region.size_bytes() / MemoryRegion::POINTER_SIZE
    }

    fn select_row(&mut self, row: usize) {
        self.finalize_search();
        self.table_state.select(Some(row));
    }

    fn selected_row(&self) -> usize {
        self.table_state.selected().unwrap_or(0)
    }

    fn selected_address(&self) -> Pointer {
        let row: usize = self.selected_row();
        let byte_offset = row * MemoryRegion::POINTER_SIZE;
        self.region.at_offset(byte_offset)
    }

    fn selected_value(&self) -> MemoryValue<[u8; 8]> {
        let row: usize = self.selected_row();
        let byte_offset = row * MemoryRegion::POINTER_SIZE;
        self.region.bytes_at_offset(byte_offset).unwrap_or_else(|| {
            panic!("Selected row {row} is outside of the MemoryRegion")
        })
    }

    fn select_address(&mut self, address: Pointer) {
        let row = (address - self.region.start()) / MemoryRegion::POINTER_SIZE;
        let row = row.clamp(0, self.num_table_rows() - 1);
        self.select_row(row);
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

    pub(crate) fn apply_key_binding(
        &mut self,
        keystrokes: &KeySequence,
        reader: &MemoryReader,
        formatters: &[Box<dyn ColumnFormatter>],
    ) -> KeyBindingMatch {
        KeyBindingMatch::Mismatch
            .or_else(|| {
                self.table_state
                    .apply_key_binding(
                        keystrokes,
                        self.num_table_rows(),
                        self.num_rows_shown() - 1,
                    )
                    .then(|| self.finalize_search())
            })
            .or_else(|| {
                let selected_address = self.selected_address();
                let table_size = self.num_table_rows();
                if let Some(search) = self.search.as_mut() {
                    search
                        .apply_key_binding(
                            keystrokes,
                            table_size,
                            Self::get_row_generator(
                                &reader,
                                &self.region,
                                selected_address,
                                formatters,
                            ),
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
            .or_try_binding("C-s", keystrokes, || {
                self.start_search(SearchDirection::Forward)
            })
            .or_try_binding("C-r", keystrokes, || {
                self.start_search(SearchDirection::Reverse)
            })
    }

    fn num_rows_shown(&self) -> usize {
        self.table_height.map(|height| height - 2).unwrap_or(1)
    }

    fn get_row_generator<'a>(
        reader: &'a MemoryReader,
        region: &'a MemoryRegion,
        selected_address: Pointer,
        formatters: &'a [Box<dyn ColumnFormatter>],
    ) -> impl Fn(usize) -> Vec<String> + 'a {
        move |row: usize| -> Vec<String> {
            let row: MemoryValue<[u8; 8]> = region
                .bytes_at_offset(row * MemoryRegion::POINTER_SIZE)
                .unwrap_or_else(|| {
                    panic!("Row {row} is outside of the memory region")
                });
            formatters
                .iter()
                .map(|formatter| {
                    formatter.cell_text(reader, region, selected_address, &row)
                })
                .collect()
        }
    }

    fn title(&self) -> String {
        let region_name = self.region.name();
        let permissions = self.region.permissions_str();

        let selected = self.selected_value().location;
        let entry_point = self.entry_point;

        let (sign, offset) = if selected >= entry_point {
            ("+", selected - entry_point)
        } else {
            ("-", entry_point - selected)
        };

        let prefix = self.region.as_range().prefix();

        let selected = selected - prefix;
        let entry_point = entry_point - prefix;

        format!(
            "{region_name} {permissions} (offset {prefix}) \
             @ {selected:#x} \
             ({entry_point:#x} {sign} 0x{offset:x})"
        )
    }

    fn generate_table<'a>(
        &self,
        reader: &'a MemoryReader,
        region: &'a MemoryRegion,
        formatters: &'a [Box<dyn ColumnFormatter>],
        search_window: &'a Option<SearchWindow<TableState>>,
        annotations: &'a [Annotation],
    ) -> impl ratatui::widgets::StatefulWidget<State = TableState> + 'a {
        let selected_address = self.selected_address();

        let header = formatters
            .iter()
            .map(|formatter| formatter.name())
            .map(crate::extended_tui::dynamic_table::Cell::from)
            .collect();

        let address_width = region.as_range().suffix_hexadecimal_digits();

        let table = DynamicTable::new(
            move |i: usize, j: usize| -> Option<Line> {
                let row =
                    region.bytes_at_offset(j * MemoryRegion::POINTER_SIZE)?;

                let formatter = formatters.get(i)?;

                let cell: Line = formatter.formatted_cell(
                    reader,
                    region,
                    selected_address,
                    annotations,
                    &row,
                );
                let cell = if let Some(search) = search_window {
                    search.highlight_search_matches(cell)
                } else {
                    cell
                };

                Some(cell)
            },
            self.num_table_rows(),
            vec![
                Constraint::Min((address_width + 3) as u16),
                Constraint::Min((2 * MemoryRegion::POINTER_SIZE + 3) as u16),
                Constraint::Min((MemoryRegion::POINTER_SIZE + 1) as u16),
                Constraint::Percentage(100),
            ],
        )
        .header(header)
        .header_style(
            Style::default()
                .bg(Color::Blue)
                .fg(Color::LightCyan)
                .add_modifier(Modifier::BOLD),
        )
        .highlight_style(Style::default().add_modifier(Modifier::REVERSED))
        .highlight_symbol(">> ");

        table
    }

    fn drawable<'a>(
        &'a mut self,
        reader: &'a MemoryReader,
        annotations: &'a [Annotation],
        formatters: &'a [Box<dyn ColumnFormatter>],
    ) -> DrawableViewFrame<'a> {
        DrawableViewFrame {
            view: self,
            reader,
            annotations,
            formatters,
        }
    }
}

impl<'a> ratatui::widgets::Widget for DrawableViewFrame<'a> {
    fn render(self, area: Rect, buf: &mut ratatui::prelude::Buffer)
    where
        Self: Sized,
    {
        // Layout.split puts all excess space into the last widget,
        // which I want to be a fixed size.  Doing the layout
        // explicitly, at least until there are more flexible
        // utilities.
        //
        // Should keep an eye on
        // https://github.com/fdehau/tui-rs/pull/596 and
        // https://github.com/fdehau/tui-rs/pull/519 to see if the
        // utilites improve.

        let search_area_height = if self.view.search.is_some() {
            area.height.min(3)
        } else {
            0
        };

        let (table_area, search_area) =
            area.split_from_bottom(search_area_height);

        if let Some(search) = self.view.search.as_ref() {
            search.render(search_area, buf);
        }

        self.view.table_height = Some(table_area.height as usize);
        let table = self
            .view
            .generate_table(
                self.reader,
                &self.view.region,
                self.formatters,
                &self.view.search,
                self.annotations,
            )
            .with_scrollbar(self.view.num_table_rows());
        table.render(table_area, buf, &mut self.view.table_state);
    }
}

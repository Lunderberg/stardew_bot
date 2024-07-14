use ratatui::{
    layout::{Constraint, Rect},
    style::{Color, Modifier, Style},
    widgets::{Block, Borders, Cell, Row, Table, TableState},
    Frame,
};

use memory_reader::extensions::*;
use memory_reader::{MemoryReader, MemoryRegion, MemoryValue, Pointer};

use crate::{extensions::*, Annotation, InputWindow};
use crate::{scroll_bar::ScrollableState, StackFrameTable};
use crate::{
    ColumnFormatter, KeyBindingMatch, KeySequence, NonEmptyVec, RunningLog,
    SearchDirection, SearchWindow,
};

use itertools::Itertools;

pub struct MemoryTable {
    view_stack: NonEmptyVec<ViewFrame>,
    formatters: Vec<Box<dyn ColumnFormatter>>,
    jump_to_window: Option<InputWindow>,
}

struct ViewFrame {
    region: MemoryRegion,
    entry_point: Pointer,
    table_state: TableState,
    search: Option<SearchWindow<TableState>>,
    table_height: Option<usize>,
}

impl MemoryTable {
    pub(crate) fn new(region: MemoryRegion, entry_point: Pointer) -> Self {
        use super::column_formatter::*;

        Self {
            view_stack: NonEmptyVec::new(ViewFrame::new(region, entry_point)),
            formatters: vec![
                Box::new(AddressColumn),
                Box::new(HexColumn),
                Box::new(AsciiColumn),
                Box::new(PointsToColumn),
            ],
            jump_to_window: None,
        }
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
        log: &mut RunningLog,
        stack_frame_table: &mut StackFrameTable,
    ) {
        let Some(input_window) = self.jump_to_window.take() else {
            log.add_log("Finalization of jump when not in-progress");
            return;
        };
        let text = input_window.text();

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
        let address: Pointer = address.into();

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

    pub(crate) fn draw(
        &mut self,
        frame: &mut Frame,
        area: Rect,
        reader: &MemoryReader,
        annotations: &[Annotation],
        border_style: Style,
    ) {
        let area = if let Some(window) = self.jump_to_window.as_mut() {
            let (top, bottom) = area.split_from_bottom(3);
            window.draw(frame, bottom);
            top
        } else {
            area
        };

        let area = self.view_stack.iter().with_position().fold(
            area,
            |area, (position, view)| {
                let style = match position {
                    itertools::Position::First | itertools::Position::Only => {
                        border_style
                    }
                    _ => Style::new(),
                };
                view.draw_border(frame, area, style)
            },
        );

        self.view_stack.last_mut().draw_inner(
            frame,
            area,
            reader,
            annotations,
            &self.formatters,
        );
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
            "{region_name} (offset {prefix}) \
             @ {selected:#x} \
             ({entry_point:#x} {sign} 0x{offset:x})"
        )
    }

    fn draw_border(&self, frame: &mut Frame, area: Rect, style: Style) -> Rect {
        let border = Block::default()
            .borders(Borders::ALL)
            .border_style(style)
            .title(self.title());
        let inner_area = border.inner(area);

        frame.render_widget(border, area);
        inner_area
    }

    fn draw_inner(
        &mut self,
        frame: &mut Frame,
        inner_area: Rect,
        reader: &MemoryReader,
        annotations: &[Annotation],
        formatters: &[Box<dyn ColumnFormatter>],
    ) {
        // Layout.split puts all excess space into the last widget,
        // which I want to be a fixed size.  Doing the layout
        // explicitly, at least until there are more flexible
        // utilities.
        //
        // Should keep an eye on
        // https://github.com/fdehau/tui-rs/pull/596 and
        // https://github.com/fdehau/tui-rs/pull/519 to see if the
        // utilites improve.

        let search_area_height = if self.search.is_some() {
            inner_area.height.min(3)
        } else {
            0
        };

        let (table_area, search_area) =
            inner_area.split_from_bottom(search_area_height);

        if let Some(search) = self.search.as_ref() {
            search.draw(frame, search_area);
        }

        self.table_height = Some(table_area.height as usize);
        let table = self
            .generate_table(reader, formatters, annotations)
            .with_scrollbar(self.num_table_rows());
        frame.render_stateful_widget(table, table_area, &mut self.table_state);
    }

    fn generate_table<'a>(
        &self,
        reader: &MemoryReader,
        formatters: &'a [Box<dyn ColumnFormatter>],
        annotations: &[Annotation],
    ) -> Table<'a> {
        let selected_row = self.selected_row();
        let selected_address = self.selected_address();

        let header_cells = formatters
            .iter()
            .map(|formatter| formatter.name())
            .map(|header| {
                Cell::from(header).style(
                    Style::default()
                        .fg(Color::LightCyan)
                        .add_modifier(Modifier::BOLD),
                )
            });

        let header = Row::new(header_cells)
            .style(Style::default().bg(Color::Blue))
            .height(1)
            .bottom_margin(1);

        let rows = self
            .region
            .iter()
            .iter_as::<MemoryValue<[u8; 8]>>()
            .enumerate()
            .map(|(i, row)| {
                let is_near_selected =
                    i.abs_diff(selected_row) < (self.num_rows_shown() as usize);

                let region = &self.region;
                let cells = formatters
                    .iter()
                    .filter(|_| is_near_selected)
                    .map(|formatter| {
                        formatter.formatted_cell(
                            reader,
                            region,
                            selected_address,
                            annotations,
                            &row,
                        )
                    })
                    .map(|line| {
                        if let Some(search) = self.search.as_ref() {
                            search.highlight_search_matches(line)
                        } else {
                            line.into()
                        }
                    });

                Row::new(cells).height(1).bottom_margin(0)
            });

        let address_width = self.region.as_range().suffix_hexadecimal_digits();

        let table = Table::new(
            rows,
            [
                Constraint::Min((address_width + 3) as u16),
                Constraint::Min((2 * MemoryRegion::POINTER_SIZE + 3) as u16),
                Constraint::Min((MemoryRegion::POINTER_SIZE + 1) as u16),
                Constraint::Percentage(100),
            ],
        )
        .header(header)
        .highlight_style(Style::default().add_modifier(Modifier::REVERSED))
        .highlight_symbol(">> ");

        table
    }
}

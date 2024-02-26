use crossterm::event::KeyEvent;
use ratatui::{
    layout::{Constraint, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Cell, Row, Table, TableState},
    Frame,
};

use memory_reader::extensions::*;
use memory_reader::{
    CollectBytes, MemoryReader, MemoryRegion, MemoryValue, Pointer,
};

use crate::{
    ColumnFormatter, KeyBindingMatch, NonEmptyVec, RunningLog, SearchCommand,
    SearchDirection, SearchItem, SearchWindow, VerticalBar,
};

use itertools::Itertools;

pub struct MemoryTable {
    view_stack: NonEmptyVec<ViewFrame>,
    formatters: Vec<Box<dyn ColumnFormatter>>,
}

struct ViewFrame {
    region: MemoryRegion,
    entry_point: Pointer,
    table_state: TableState,
    search: Option<SearchWindow<TableState>>,
    previous_height: Option<usize>,
}

impl MemoryTable {
    pub fn new(region: MemoryRegion, entry_point: Pointer) -> Self {
        use super::column_formatter::*;

        Self {
            view_stack: NonEmptyVec::new(ViewFrame::new(region, entry_point)),
            formatters: vec![
                Box::new(AddressColumn),
                Box::new(HexColumn),
                Box::new(AsciiColumn),
                Box::new(PointsToColumn),
            ],
        }
    }

    pub fn push_view(&mut self, region: MemoryRegion, entry_point: Pointer) {
        self.finalize_search();
        self.view_stack.push(ViewFrame::new(region, entry_point));
    }

    pub fn pop_view(&mut self) {
        if self.view_stack.len() > 1 {
            self.view_stack.pop();
        }
    }

    pub fn current_region(&self) -> &MemoryRegion {
        &self.active_view().region
    }

    pub fn selected_value(&self) -> MemoryValue<[u8; 8]> {
        self.active_view().selected_value()
    }

    fn move_selection_relative(&mut self, delta: i64) {
        self.active_view_mut().move_selection_relative(delta);
    }

    fn move_selection_absolute(&mut self, row: usize) {
        self.active_view_mut().select_row(row);
    }

    pub fn move_selection_start(&mut self) {
        self.move_selection_absolute(0);
    }

    pub fn move_selection_end(&mut self) {
        self.move_selection_absolute(self.active_view().num_table_rows() - 1);
    }

    pub fn move_selection_down(&mut self) {
        self.move_selection_relative(1);
    }

    pub fn move_selection_up(&mut self) {
        self.move_selection_relative(-1);
    }

    pub fn move_selection_page_down(&mut self) {
        self.move_selection_relative(self.displayed_rows() as i64);
    }

    pub fn move_selection_page_up(&mut self) {
        self.move_selection_relative(-(self.displayed_rows() as i64));
    }

    pub fn search_is_active(&self) -> bool {
        self.active_view().search.is_some()
    }

    pub fn finalize_search(&mut self) {
        self.active_view_mut().finalize_search();
    }

    pub fn search_forward(&mut self, reader: &MemoryReader) {
        self.apply_search_command(
            SearchCommand::NextResult(SearchDirection::Forward),
            reader,
        );
    }

    pub fn search_backward(&mut self, reader: &MemoryReader) {
        self.apply_search_command(
            SearchCommand::NextResult(SearchDirection::Reverse),
            reader,
        );
    }

    pub fn add_search_character(&mut self, c: char, reader: &MemoryReader) {
        self.apply_search_command(SearchCommand::AddChar(c), reader);
    }

    pub fn backspace_search_character(&mut self) {
        self.active_view_mut().undo_search_command();
    }

    pub fn cancel_search(&mut self) {
        self.active_view_mut().cancel_search();
    }

    fn apply_search_command(
        &mut self,
        command: SearchCommand,
        reader: &MemoryReader,
    ) {
        // Can't use `self.active_view_mut()` here, because that would
        // mutably borrow `self`, preventing the immutable borrow of
        // `self.formatters`.
        self.view_stack.last_mut().apply_search_command(
            command,
            reader,
            &self.formatters,
        );
    }

    fn displayed_rows(&self) -> usize {
        let non_data_rows = 5;
        self.active_view()
            .previous_height
            .map(|height| height - non_data_rows)
            .unwrap_or(1)
    }

    fn active_view(&self) -> &ViewFrame {
        self.view_stack.last()
    }

    fn active_view_mut(&mut self) -> &mut ViewFrame {
        self.view_stack.last_mut()
    }

    pub fn draw(
        &mut self,
        frame: &mut Frame,
        area: Rect,
        reader: &MemoryReader,
        border_style: Style,
    ) {
        let inner_area = self.view_stack.iter().with_position().fold(
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
            inner_area,
            reader,
            &self.formatters,
        );
    }

    pub fn apply_key_binding(
        &mut self,
        keystrokes: &[KeyEvent],
        reader: &MemoryReader,
        log: &mut RunningLog,
    ) -> KeyBindingMatch {
        KeyBindingMatch::Mismatch
            .or_else(|| {
                self.view_stack.last_mut().apply_key_binding(
                    keystrokes,
                    reader,
                    &self.formatters,
                )
            })
            .or_try_binding("C-g", keystrokes, || self.pop_view())
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
    pub fn new(region: MemoryRegion, entry_point: Pointer) -> Self {
        let mut frame = ViewFrame {
            region,
            entry_point,
            table_state: TableState::default(),
            search: None,
            previous_height: None,
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

    fn move_selection_relative(&mut self, delta: i64) {
        let n = self.num_table_rows();
        let row = match (self.table_state.selected(), delta.signum()) {
            // If no prior selection, moving down a line selects the
            // first element, but still allows a page down.
            (None, 1) => (delta as usize) - 1,

            // Wrapping to the end of the list selects the last
            // element, regardless of step size.
            (None | Some(0), -1) => n - 1,

            // Wrapping to the beginning of the list selects the first
            // element, regardless of step size.
            (Some(i), 1) if i == n - 1 => 0,

            // Otherwise, go in the direction specified, but capped at
            // the endpoint.
            (Some(i), -1) => ((i as i64) + delta).max(0) as usize,
            (Some(i), 1) => (i + (delta as usize)).min(n - 1),
            _ => panic!("This shouldn't happen"),
        };
        self.select_row(row);
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

    fn cancel_search(&mut self) {
        if let Some(search) = self.search.take() {
            self.table_state = search.pre_search_state;
        }
    }

    fn finalize_search(&mut self) {
        self.search = None;
    }

    fn undo_search_command(&mut self) {
        if let Some(active) = &mut self.search {
            active.pop_command();
            self.table_state
                .select(Some(active.recommended_row_selection()));
        }
    }

    pub fn apply_key_binding(
        &mut self,
        keystrokes: &[KeyEvent],
        reader: &MemoryReader,
        formatters: &[Box<dyn ColumnFormatter>],
    ) -> KeyBindingMatch {
        KeyBindingMatch::Mismatch
            .or_try_bindings(["<down>", "C-n"], keystrokes, || {
                self.move_selection_relative(1)
            })
            .or_try_bindings(["<up>", "C-p"], keystrokes, || {
                self.move_selection_relative(-1)
            })
            .or_try_bindings(["<pageup>", "C-v"], keystrokes, || {
                self.move_selection_relative(-self.page_up_down_distance())
            })
            .or_try_bindings(["<pagedown>", "M-v"], keystrokes, || {
                self.move_selection_relative(self.page_up_down_distance())
            })
            .or_try_bindings(["C-<home>", "M-<"], keystrokes, || {
                self.select_row(0)
            })
            .or_try_bindings(["C-<end>", "M->"], keystrokes, || {
                self.select_row(self.num_table_rows() - 1)
            })
            .or_try_binding("C-s", keystrokes, || {
                self.apply_search_command(
                    SearchCommand::NextResult(SearchDirection::Forward),
                    reader,
                    formatters,
                )
            })
            .or_try_binding("C-r", keystrokes, || {
                self.apply_search_command(
                    SearchCommand::NextResult(SearchDirection::Reverse),
                    reader,
                    formatters,
                )
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
    }

    fn page_up_down_distance(&self) -> i64 {
        self.previous_height.map(|height| height - 3).unwrap_or(1) as i64
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

    fn apply_search_command(
        &mut self,
        command: SearchCommand,
        reader: &MemoryReader,
        formatters: &[Box<dyn ColumnFormatter>],
    ) {
        use SearchCommand::*;

        let selected_address = self.selected_address();
        let table_size = self.num_table_rows();

        match (self.search.as_mut(), command) {
            (Some(active), _) => {
                let row_generator = Self::get_row_generator(
                    &reader,
                    &self.region,
                    selected_address,
                    formatters,
                );
                active.apply_command(command, table_size, row_generator);
                self.table_state
                    .select(Some(active.recommended_row_selection()));
            }
            (None, NextResult(_)) => {
                let initial_row = self.table_state.selected().unwrap_or(0);
                let initial_item = SearchItem {
                    command,
                    search_result: Some(initial_row),
                };
                self.search = Some(SearchWindow {
                    stack: NonEmptyVec::new(initial_item),
                    pre_search_state: self.table_state.clone(),
                });
            }
            (None, AddChar(_)) => {}
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
        let table_height = inner_area.height - search_area_height;
        let header_height = table_height.min(2);
        let scrollbar_width = inner_area.width.min(1);

        let search_area = Rect::new(
            inner_area.x,
            inner_area.bottom() - search_area_height,
            inner_area.width,
            search_area_height,
        );

        let scrollbar_area = Rect::new(
            inner_area.x,
            inner_area.y + header_height,
            scrollbar_width,
            table_height - header_height,
        );

        let table_area = Rect::new(
            inner_area.x + scrollbar_width,
            inner_area.y,
            inner_area.width - scrollbar_width,
            table_height,
        );

        if let Some(search) = self.search.as_ref() {
            search.draw(frame, search_area);
        }
        self.draw_scrollbar(frame, scrollbar_area);
        self.draw_table(frame, table_area, reader, formatters);
    }

    fn draw_scrollbar(&mut self, frame: &mut Frame, area: Rect) {
        let selected = self.selected_row();
        let table_size = self.num_table_rows();
        let rows_shown = area.height as usize;
        let (top_ratio, bottom_ratio) = if selected < rows_shown {
            (0.0, (rows_shown as f64) / (table_size as f64))
        } else if selected > table_size - rows_shown {
            (
                ((table_size - rows_shown) as f64) / (table_size as f64),
                1.0,
            )
        } else {
            (
                ((selected - rows_shown / 2) as f64) / (table_size as f64),
                ((selected + rows_shown / 2) as f64) / (table_size as f64),
            )
        };
        let bar = VerticalBar::default()
            .bar_top_ratio(top_ratio)
            .bar_bottom_ratio(bottom_ratio);
        frame.render_widget(bar, area);
    }

    fn draw_table(
        &mut self,
        frame: &mut Frame,
        area: Rect,
        reader: &MemoryReader,
        formatters: &[Box<dyn ColumnFormatter>],
    ) {
        self.previous_height = Some(area.height as usize);

        let selected_row = self.selected_row();
        let selected_address = self.selected_address();

        let selected_style = Style::default().add_modifier(Modifier::REVERSED);
        let normal_style = Style::default().bg(Color::Blue);
        let search_result_style = Style::default().bg(Color::Yellow);

        let search_string = self
            .search
            .as_ref()
            .map(|search_state| search_state.get_search_string(None));

        let highlight_search_matches = |line| {
            if search_string.as_ref().map(|s| s.is_empty()).unwrap_or(true) {
                return line;
            }

            let Line { spans, alignment } = line;
            let search_string: &str = search_string.as_ref().unwrap();

            // Not technically correct, as it doesn't handle cases
            // where the match crosses a border between spans.  But,
            // close enough for now.
            let spans = spans
                .into_iter()
                .flat_map(|span| {
                    span.content
                        .split(search_string)
                        .map(|s| Span::styled(s.to_string(), span.style))
                        .intersperse_with(|| {
                            Span::styled(
                                search_string.to_string(),
                                span.style.patch(search_result_style),
                            )
                        })
                        .collect::<Vec<_>>()
                        .into_iter()
                })
                .collect();

            Line { spans, alignment }
        };

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
            .style(normal_style)
            .height(1)
            .bottom_margin(1);

        let rows = self.region.iter_bytes().iter_byte_arr().enumerate().map(
            |(i, row): (_, MemoryValue<[u8; 8]>)| {
                let is_near_selected =
                    i.abs_diff(selected_row) < (area.height as usize);

                let region = &self.region;
                let cells = formatters
                    .iter()
                    .filter(|_| is_near_selected)
                    .map(|formatter| {
                        formatter.formatted_cell(
                            reader,
                            region,
                            selected_address,
                            &row,
                        )
                    })
                    .map(move |line| highlight_search_matches(line));

                Row::new(cells).height(1).bottom_margin(0)
            },
        );

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
        .highlight_style(selected_style)
        .highlight_symbol(">> ");

        frame.render_stateful_widget(table, area, &mut self.table_state);
    }
}

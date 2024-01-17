use ratatui::{
    layout::{Constraint, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Cell, Paragraph, Row, Table, TableState},
    Frame,
};

use memory_reader::extensions::*;
use memory_reader::{
    CollectBytes, MemoryReader, MemoryRegion, MemoryValue, Pointer,
};

use crate::{ColumnFormatter, NonEmptyVec, VerticalBar};

use itertools::{Either, Itertools};

pub struct MemoryTable {
    view_stack: NonEmptyVec<ViewFrame>,
    previous_height: Option<usize>,
    formatters: Vec<Box<dyn ColumnFormatter>>,
}

struct ViewFrame {
    region: MemoryRegion,
    entry_point: Pointer,
    table_state: TableState,
    search: Option<ActiveSearch>,
}

struct ActiveSearch {
    stack: NonEmptyVec<SearchItem>,
    pre_search_state: TableState,
}

struct SearchItem {
    command: SearchCommand,
    search_result: Option<usize>,
}

#[derive(Clone, Copy)]
enum SearchCommand {
    NextResult(Direction),
    AddChar(char),
}

#[derive(Clone, Copy)]
enum Direction {
    Forward,
    Reverse,
}

impl ActiveSearch {
    // Undo the most recent command, unless it was the initial command
    // that started the search.
    fn pop_command(&mut self, table_state: &mut TableState) {
        if self.stack.len() > 1 {
            self.stack.pop();
            self.update_table_selection(table_state);
        }
    }

    // Return the string being searched for.
    fn get_search_string(&self, last_char: Option<char>) -> String {
        self.stack
            .iter()
            .filter_map(|item| match item.command {
                SearchCommand::AddChar(c) => Some(c),
                _ => None,
            })
            .chain(last_char.iter().copied())
            .collect()
    }

    // Return a tuple of 2 strings, where the first string is the
    // portion of the search string that was found, and the second
    // string is the portion of the search string that wasn't found.
    fn get_search_string_parts(&self) -> (String, String) {
        let vals: Vec<(char, bool)> = self
            .stack
            .iter()
            .filter_map(|item| match item.command {
                SearchCommand::AddChar(c) => {
                    Some((c, item.search_result.is_some()))
                }
                _ => None,
            })
            .collect();

        let matching_part =
            vals.iter().filter(|(_, p)| *p).map(|(c, _)| c).collect();
        let non_matching_part =
            vals.iter().filter(|(_, p)| !*p).map(|(c, _)| c).collect();
        (matching_part, non_matching_part)
    }

    fn apply_command<F>(
        &mut self,
        command: SearchCommand,
        table_size: usize,
        row_generator: F,
        table_state: &mut TableState,
    ) where
        F: FnMut(usize) -> Vec<String>,
    {
        use Direction::*;
        use SearchCommand::*;

        let previous_result: Option<usize> = self.stack.last().search_result;

        let previous_direction = self
            .stack
            .iter()
            .rev()
            .find_map(|item| match item.command {
                NextResult(dir) => Some(dir),
                _ => None,
            })
            .expect(
                "If no others, \
                 first item in search stack should have direction",
            );

        let search_range = match (&command, previous_result, previous_direction)
        {
            (NextResult(Forward), None, _) => Some(Either::Left(0..table_size)),
            (NextResult(Forward), Some(prev), _) => {
                Some(Either::Left((prev + 1)..table_size))
            }
            (NextResult(Reverse), None, _) => {
                Some(Either::Right((0..table_size).rev()))
            }
            (NextResult(Reverse), Some(prev), _) => {
                Some(Either::Right((0..prev).rev()))
            }
            (AddChar(_), None, _) => None,
            (AddChar(_), Some(prev), Forward) => {
                Some(Either::Left(prev..table_size))
            }
            (AddChar(_), Some(prev), Reverse) => {
                Some(Either::Right((0..(prev + 1)).rev()))
            }
        };

        let new_char = match command {
            AddChar(c) => Some(c),
            _ => None,
        };

        let search_result: Option<usize> =
            search_range.and_then(|search_range| {
                self.search(search_range, new_char, row_generator)
            });

        self.stack.push(SearchItem {
            command,
            search_result,
        });
        self.update_table_selection(table_state);
    }

    fn search<F>(
        &mut self,
        search_range: impl IntoIterator<Item = usize>,
        last_char: Option<char>,
        mut row_generator: F,
    ) -> Option<usize>
    where
        F: FnMut(usize) -> Vec<String>,
    {
        let needle = self.get_search_string(last_char);
        search_range.into_iter().find(|row| {
            row_generator(*row)
                .iter()
                .any(|cell_text| cell_text.contains(&needle))
        })
    }

    // Select the most recent match if any exist, otherwise the
    // initial location of the search.
    fn update_table_selection(&mut self, table_state: &mut TableState) {
        let selected_row = self
            .stack
            .iter()
            .rev()
            .find_map(|item| item.search_result)
            .expect(
                "If no others, \
                 first item in search stack should be Some(row)",
            );
        table_state.select(Some(selected_row));
    }

    fn description(&self) -> String {
        use Direction::*;
        use SearchCommand::*;

        let is_failing_search = self.stack.last().search_result.is_none();

        let is_wrapped_search = self
            .stack
            .iter()
            .skip_while(|item| item.search_result.is_some())
            .skip(1)
            .any(|item| match item.command {
                NextResult(_) => true,
                AddChar(_) => false,
            });

        let direction = self
            .stack
            .iter()
            .rev()
            .find_map(|item| match item.command {
                NextResult(dir) => Some(dir),
                AddChar(_) => None,
            })
            .expect(
                "If no others, \
                 first item in search stack should have direction",
            );

        let desc = match (is_failing_search, is_wrapped_search, direction) {
            (false, false, Forward) => "I-search",
            (true, false, Forward) => "Failing I-search",
            (false, true, Forward) => "Wrapped I-search",
            (true, true, Forward) => "Failing wrapped I-search",
            (false, false, Reverse) => "I-search backward",
            (true, false, Reverse) => "Failing I-search backward",
            (false, true, Reverse) => "Wrapped I-search backward",
            (true, true, Reverse) => "Failing wrapped I-search backward",
        };
        desc.to_string()
    }
}

impl ViewFrame {
    pub fn new(region: MemoryRegion, entry_point: Pointer) -> Self {
        let mut frame = ViewFrame {
            region,
            entry_point,
            table_state: TableState::default(),
            search: None,
        };

        frame.select_address(entry_point);
        frame
    }

    fn num_table_rows(&self) -> usize {
        self.region.size_bytes() / MemoryRegion::POINTER_SIZE
    }

    fn select(&mut self, row: usize) {
        self.finalize_search();
        self.table_state.select(Some(row));
    }

    fn selected(&self) -> Option<usize> {
        self.table_state.selected()
    }

    fn selected_address(&self) -> Pointer {
        let row: usize = self.selected().unwrap_or(0);
        let byte_offset = row * MemoryRegion::POINTER_SIZE;
        self.region.at_offset(byte_offset)
    }

    fn selected_value(&self) -> MemoryValue<[u8; 8]> {
        let row: usize = self.selected().unwrap_or(0);
        let byte_offset = row * MemoryRegion::POINTER_SIZE;
        self.region.bytes_at_offset(byte_offset).unwrap_or_else(|| {
            panic!("Selected row {row} is outside of the MemoryRegion")
        })
    }

    fn select_address(&mut self, address: Pointer) {
        let row = (address - self.region.start()) / MemoryRegion::POINTER_SIZE;
        let row = row.clamp(0, self.num_table_rows() - 1);
        self.select(row);
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
            active.pop_command(&mut self.table_state);
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
                let region = &self.region;
                let row_generator = |row: usize| -> Vec<String> {
                    let row: MemoryValue<[u8; 8]> = region
                        .bytes_at_offset(row * MemoryRegion::POINTER_SIZE)
                        .unwrap_or_else(|| {
                            panic!("Row {row} is outside of the memory region")
                        });
                    formatters
                        .iter()
                        .map(|formatter| {
                            formatter.cell_text(
                                reader,
                                region,
                                selected_address,
                                &row,
                            )
                        })
                        .collect()
                };
                active.apply_command(
                    command,
                    table_size,
                    row_generator,
                    &mut self.table_state,
                );
            }
            (None, NextResult(_)) => {
                let initial_row = self.table_state.selected().unwrap_or(0);
                let initial_item = SearchItem {
                    command,
                    search_result: Some(initial_row),
                };
                self.search = Some(ActiveSearch {
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
}

impl MemoryTable {
    pub fn new(region: MemoryRegion, entry_point: Pointer) -> Self {
        use super::column_formatter::*;

        Self {
            view_stack: NonEmptyVec::new(ViewFrame::new(region, entry_point)),
            previous_height: None,
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
        let n = self.active_view().num_table_rows();
        let row =
            match (self.active_view().table_state.selected(), delta.signum()) {
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
        self.move_selection_absolute(row);
    }

    fn move_selection_absolute(&mut self, row: usize) {
        self.active_view_mut().select(row);
        self.cancel_search();
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
            SearchCommand::NextResult(Direction::Forward),
            reader,
        );
    }

    pub fn search_backward(&mut self, reader: &MemoryReader) {
        self.apply_search_command(
            SearchCommand::NextResult(Direction::Reverse),
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
        self.previous_height
            .map(|height| height - non_data_rows)
            .unwrap_or(1)
    }

    fn active_view(&self) -> &ViewFrame {
        self.view_stack.last()
    }

    fn active_view_mut(&mut self) -> &mut ViewFrame {
        self.view_stack.last_mut()
    }

    fn selected_row(&self) -> usize {
        self.active_view().selected().unwrap_or(0)
    }

    pub fn draw(
        &mut self,
        frame: &mut Frame,
        area: Rect,
        reader: &MemoryReader,
    ) {
        let borders: Vec<_> = self
            .view_stack
            .iter()
            .map(|view| {
                Block::default().borders(Borders::ALL).title(view.title())
            })
            .collect();

        let border_areas: Vec<Rect> = borders
            .iter()
            .scan(area, |state, border| {
                let prev = *state;
                *state = border.inner(prev);
                Some(prev)
            })
            .collect();

        let inner_area =
            borders.last().unwrap().inner(*border_areas.last().unwrap());

        // Layout.split puts all excess space into the last widget,
        // which I want to be a fixed size.  Doing the layout
        // explicitly, at least until there are more flexible
        // utilities.
        //
        // Should keep an eye on
        // https://github.com/fdehau/tui-rs/pull/596 and
        // https://github.com/fdehau/tui-rs/pull/519 to see if the
        // utilites improve.

        let search_area_height = if self.search_is_active() {
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

        borders.into_iter().zip(border_areas).for_each(
            |(border, border_area)| frame.render_widget(border, border_area),
        );
        self.draw_search_area(frame, search_area);
        self.draw_scrollbar(frame, scrollbar_area);
        self.draw_table(frame, table_area, reader);
    }

    fn draw_search_area(&mut self, frame: &mut Frame, area: Rect) {
        let search: Option<&ActiveSearch> = self.active_view().search.as_ref();

        let (matching_part, non_matching_part) = search
            .map(|search_state| search_state.get_search_string_parts())
            .unwrap_or_else(|| ("".to_string(), "".to_string()));
        let line: Line = vec![
            Span::raw(matching_part),
            Span::styled(non_matching_part, Style::default().bg(Color::Red)),
        ]
        .into();

        let title = search
            .map(|state| state.description())
            .unwrap_or("".to_string());

        let widget = Paragraph::new(line)
            .block(Block::default().borders(Borders::ALL).title(title));
        frame.render_widget(widget, area);
    }

    fn draw_scrollbar(&mut self, frame: &mut Frame, area: Rect) {
        let selected = self.selected_row();
        let table_size = self.active_view().num_table_rows();
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
    ) {
        self.previous_height = Some(area.height as usize);

        let selected_row = self.selected_row();
        let selected_address = self.active_view().selected_address();

        let selected_style = Style::default().add_modifier(Modifier::REVERSED);
        let normal_style = Style::default().bg(Color::Blue);
        let search_result_style = Style::default().bg(Color::Yellow);

        let search_string = self
            .active_view()
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

        let header_cells = self
            .formatters
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

        let active_view = self.view_stack.last_mut();

        let rows = active_view
            .region
            .iter_bytes()
            .iter_byte_arr()
            .enumerate()
            .map(|(i, row): (_, MemoryValue<[u8; 8]>)| {
                let is_near_selected =
                    i.abs_diff(selected_row) < (area.height as usize);

                let region = &active_view.region;
                let cells = self
                    .formatters
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
            });

        let address_width =
            active_view.region.as_range().suffix_hexadecimal_digits();

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

        frame.render_stateful_widget(table, area, &mut active_view.table_state);
    }
}

use tui::{
    backend::Backend,
    layout::{Constraint, Rect},
    style::{Color, Modifier, Style},
    text::{Span, Spans},
    widgets::{Block, Borders, Cell, Paragraph, Row, Table, TableState},
    Frame,
};

use crate::memory_reader::{CollectBytes, MemoryRegion, MemoryValue, Pointer};

use super::VerticalBar;

use itertools::Either;

pub struct MemoryTable {
    view_stack: Vec<ViewFrame>,
    previous_height: Option<usize>,
    search_state: Option<SearchState>,
}

struct ViewFrame {
    state: TableState,
    region: MemoryRegion,
    entry_point: Pointer,
}

struct SearchState {
    stack: Vec<SearchItem>,
    table_state: TableState,
    initial_item: SearchItem,
    table_size: usize,
}

struct SearchItem {
    command: SearchCommand,
    search_result: Option<usize>,
}

enum SearchCommand {
    NextResult(Direction),
    AddChar(char),
}

#[derive(Clone, Copy)]
enum Direction {
    Forward,
    Reverse,
}

const POINTER_SIZE: usize = 8;

impl SearchState {
    fn new(
        table_state: TableState,
        table_size: usize,
        initial_direction: Direction,
    ) -> Self {
        let initial_row = table_state.selected().unwrap_or(0);
        let initial_item = SearchItem {
            command: SearchCommand::NextResult(initial_direction),
            search_result: Some(initial_row),
        };
        Self {
            stack: Vec::new(),
            table_state,
            initial_item,
            table_size,
        }
    }

    fn stack(&self) -> impl DoubleEndedIterator<Item = &SearchItem> + '_ {
        std::iter::once(&self.initial_item).chain(self.stack.iter())
    }

    fn apply_command<F, const N: usize>(
        &mut self,
        command: SearchCommand,
        row_generator: F,
    ) where
        F: FnMut(usize) -> [String; N],
    {
        use Direction::*;
        use SearchCommand::*;

        let previous_result: Option<usize> =
            self.stack().last().unwrap().search_result;

        let previous_direction = self
            .stack()
            .rev()
            .find_map(|item| match item.command {
                NextResult(dir) => Some(dir),
                AddChar(_) => None,
            })
            .unwrap();

        let search_range = match (&command, previous_result, previous_direction)
        {
            (NextResult(Forward), None, _) => {
                Some(Either::Left(0..self.table_size))
            }
            (NextResult(Forward), Some(prev), _) => {
                Some(Either::Left((prev + 1)..self.table_size))
            }
            (NextResult(Reverse), None, _) => {
                Some(Either::Right((0..self.table_size).rev()))
            }
            (NextResult(Reverse), Some(prev), _) => {
                Some(Either::Right((0..prev).rev()))
            }
            (AddChar(_), None, _) => None,
            (AddChar(_), Some(prev), Forward) => {
                Some(Either::Left(prev..self.table_size))
            }
            (AddChar(_), Some(prev), Reverse) => {
                Some(Either::Right((0..(prev + 1)).rev()))
            }
        };

        let new_char = match command {
            NextResult(_) => None,
            AddChar(c) => Some(c),
        };

        let search_result: Option<usize> = search_range
            .map(|search_range| {
                self.search(search_range, new_char, row_generator)
            })
            .flatten();

        self.stack.push(SearchItem {
            command,
            search_result,
        });
        self.update_table_selection();
    }

    // Undo the most recent command.
    fn pop_command(&mut self) {
        self.stack.pop();
        self.update_table_selection();
    }

    // Return the string being searched for.
    fn get_search_string(&self, last_char: Option<char>) -> String {
        self.stack()
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
            .stack()
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

    fn search<F, const N: usize>(
        &mut self,
        search_range: impl Iterator<Item = usize>,
        last_char: Option<char>,
        mut row_generator: F,
    ) -> Option<usize>
    where
        F: FnMut(usize) -> [String; N],
    {
        let needle = self.get_search_string(last_char);
        search_range
            .filter(|row| {
                row_generator(*row)
                    .iter()
                    .any(|cell_text| cell_text.contains(&needle))
            })
            .next()
    }

    // Select the most recent match if any exist, otherwise the
    // initial location of the search.
    fn update_table_selection(&mut self) {
        let selected_row = self
            .stack()
            .rev()
            .find_map(|item| item.search_result)
            .unwrap();
        self.table_state.select(Some(selected_row));
    }

    fn description(&self) -> String {
        use Direction::*;
        use SearchCommand::*;

        let is_failing_search = self
            .stack()
            .last()
            .as_ref()
            .unwrap()
            .search_result
            .is_none();

        let is_wrapped_search = self
            .stack()
            .skip_while(|item| item.search_result.is_some())
            .skip(1)
            .any(|item| match item.command {
                NextResult(_) => true,
                AddChar(_) => false,
            });

        let direction = self
            .stack()
            .rev()
            .find_map(|item| match item.command {
                NextResult(dir) => Some(dir),
                AddChar(_) => None,
            })
            .unwrap();

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
    fn num_table_rows(&self) -> usize {
        self.region.size_bytes() / POINTER_SIZE
    }

    fn select_address(&mut self, address: Pointer) {
        let row = (address - self.region.start()) / POINTER_SIZE;
        let row = row.clamp(0, self.num_table_rows() - 1);
        self.state.select(Some(row));
    }

    fn value_at(&self, row_override: Option<usize>) -> MemoryValue<[u8; 8]> {
        let row: usize = row_override.or(self.state.selected()).unwrap_or(0);
        let byte_offset = row * POINTER_SIZE;
        self.region.bytes_at_offset(byte_offset)
    }

    fn title(&self, row_override: Option<usize>) -> String {
        let region_name = self
            .region
            .source
            .name
            .as_ref()
            .map(|x| &**x)
            .unwrap_or("[anon]");

        let selected = self.value_at(row_override).location;
        let entry_point = self.entry_point;

        let (sign, offset) = if selected >= entry_point {
            ("+", selected - entry_point)
        } else {
            ("-", entry_point - selected)
        };

        format!(
            "{region_name} @ {selected} ({entry_point} {sign} 0x{offset:x})"
        )
    }
}

impl MemoryTable {
    pub fn new(region: MemoryRegion, entry_point: Pointer) -> Self {
        let first_view = ViewFrame {
            state: TableState::default(),
            region,
            entry_point,
        };
        let mut out = Self {
            view_stack: vec![first_view],
            previous_height: None,
            search_state: None,
        };

        out.select_address(entry_point);

        out
    }

    pub fn select_address(&mut self, address: Pointer) {
        self.active_view_mut().select_address(address);
        self.cancel_search();
    }

    pub fn selected_value(&self) -> MemoryValue<[u8; 8]> {
        let byte_offset = self.selected_row() * POINTER_SIZE;
        self.active_view().region.bytes_at_offset(byte_offset)
    }

    fn move_selection_relative(&mut self, delta: i64) {
        let n = self.active_view().num_table_rows();
        let row = match (self.active_state().selected(), delta.signum()) {
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
        self.active_view_mut().state.select(Some(row));
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
        self.search_state.is_some()
    }

    pub fn search_forward(&mut self) {
        self.search(Direction::Forward);
    }

    pub fn search_backward(&mut self) {
        self.search(Direction::Reverse);
    }

    fn search(&mut self, direction: Direction) {
        let view = self.view_stack.last().unwrap();
        if let Some(search_state) = self.search_state.as_mut() {
            search_state
                .apply_command(SearchCommand::NextResult(direction), |row| {
                    Self::row_text(&view.region, row)
                });
        } else {
            self.search_state = Some(SearchState::new(
                view.state.clone(),
                view.num_table_rows(),
                direction,
            ));
        }
    }

    pub fn add_search_character(&mut self, c: char) {
        let region = &self.view_stack.last().unwrap().region;
        if let Some(search_state) = self.search_state.as_mut() {
            search_state.apply_command(SearchCommand::AddChar(c), |row| {
                Self::row_text(region, row)
            });
        }
    }

    pub fn backspace_search_character(&mut self) {
        if let Some(search_state) = self.search_state.as_mut() {
            search_state.pop_command();
        }
    }

    pub fn cancel_search(&mut self) {
        self.search_state = None;
    }

    fn displayed_rows(&self) -> usize {
        let non_data_rows = 5;
        self.previous_height
            .map(|height| height - non_data_rows)
            .unwrap_or(1)
    }

    fn row_text(region: &MemoryRegion, row: usize) -> [String; 2] {
        let arr: MemoryValue<[u8; 8]> =
            region.bytes_at_offset(row * POINTER_SIZE);
        let as_pointer: Pointer = arr.value.into();
        [format!("{}", arr.location), format!("{}", as_pointer)]
    }

    fn active_view(&self) -> &ViewFrame {
        self.view_stack.last().unwrap()
    }

    fn active_view_mut(&mut self) -> &mut ViewFrame {
        self.view_stack.last_mut().unwrap()
    }

    fn active_state(&self) -> &TableState {
        self.search_state
            .as_ref()
            .map_or(&self.active_view().state, |search_state| {
                &search_state.table_state
            })
    }

    fn active_state_mut(&mut self) -> &mut TableState {
        let view = self.view_stack.last_mut().unwrap();
        self.search_state
            .as_mut()
            .map_or(&mut view.state, |search_state| {
                &mut search_state.table_state
            })
    }

    fn selected_row(&self) -> usize {
        self.active_state().selected().unwrap_or(0)
    }

    pub fn draw<B: Backend>(&mut self, frame: &mut Frame<B>, area: Rect) {
        let borders: Vec<_> = self
            .view_stack
            .iter()
            .enumerate()
            .map(|(i, view)| {
                let is_innermost = i == self.view_stack.len() - 1;
                let row_override: Option<usize> = is_innermost
                    .then(|| {
                        self.search_state
                            .as_ref()
                            .map(|search_state| {
                                search_state.table_state.selected()
                            })
                            .flatten()
                    })
                    .flatten();
                Block::default()
                    .borders(Borders::ALL)
                    .title(view.title(row_override))
            })
            .collect();

        let inner_area =
            borders.iter().fold(area, |prev, border| border.inner(prev));

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

        borders
            .into_iter()
            .for_each(|border| frame.render_widget(border, area));
        if search_area_height > 0 {
            self.draw_search_area(frame, search_area);
        }
        self.draw_scrollbar(frame, scrollbar_area);
        self.draw_table(frame, table_area);
    }

    fn draw_search_area<B: Backend>(
        &mut self,
        frame: &mut Frame<B>,
        area: Rect,
    ) {
        let (matching_part, non_matching_part) = self
            .search_state
            .as_ref()
            .map(|search_state| search_state.get_search_string_parts())
            .unwrap_or_else(|| ("".to_string(), "".to_string()));
        let text = Spans::from(vec![
            Span::raw(matching_part),
            Span::styled(non_matching_part, Style::default().bg(Color::Red)),
        ]);

        let title = self
            .search_state
            .as_ref()
            .map(|state| state.description())
            .unwrap_or("".to_string());

        let widget = Paragraph::new(text)
            .block(Block::default().borders(Borders::ALL).title(title));
        frame.render_widget(widget, area);
    }

    fn draw_scrollbar<B: Backend>(&mut self, frame: &mut Frame<B>, area: Rect) {
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

    fn draw_table<B: Backend>(&mut self, frame: &mut Frame<B>, area: Rect) {
        self.previous_height = Some(area.height as usize);

        let selected_style = Style::default().add_modifier(Modifier::REVERSED);
        let normal_style = Style::default().bg(Color::Blue);
        let search_result_current_style = Style::default().bg(Color::Yellow);
        let search_result_other_style = Style::default().bg(Color::LightYellow);

        let search_string: String = self
            .search_state
            .as_ref()
            .map(|search_state| search_state.get_search_string(None))
            .unwrap_or_else(|| "".to_string());

        let format_text = |text: &str, is_selected: bool| -> Spans {
            let result_style = if is_selected {
                search_result_current_style
            } else {
                search_result_other_style
            };

            let mut spans = Vec::new();
            let mut haystack: &str = &text;
            if !search_string.is_empty() {
                while let Some(pos) = haystack.find(&search_string) {
                    if pos > 0 {
                        spans.push(Span::raw(haystack[..pos].to_string()));
                    }
                    let end = pos + search_string.len();
                    spans.push(Span::styled(
                        haystack[pos..end].to_string(),
                        result_style,
                    ));
                    haystack = &haystack[end..];
                }
            }
            if !haystack.is_empty() {
                spans.push(Span::raw(haystack.to_string()));
            }
            Spans::from(spans)
        };

        let header_cells = ["Address", "Hex"].iter().map(|h| {
            Cell::from(*h).style(
                Style::default()
                    .fg(Color::LightCyan)
                    .add_modifier(Modifier::BOLD),
            )
        });
        let header = Row::new(header_cells)
            .style(normal_style)
            .height(1)
            .bottom_margin(1);
        let rows = self
            .active_view()
            .region
            .iter_bytes()
            .iter_byte_arr()
            .enumerate()
            .map(|(i, arr): (_, MemoryValue<[u8; 8]>)| {
                let selected = self.selected_row();
                let dist_to_selected = if i > selected {
                    i - selected
                } else {
                    selected - i
                };
                let is_near_selected =
                    dist_to_selected < (area.height as usize);
                let is_selected = i == selected;

                // TODO: Use abs_diff, stabilized in 1.60
                //
                // let is_near_selected =
                //     self.state.selected().unwrap_or(0).abs_diff(i) < area.height;

                let cells = if is_near_selected {
                    let as_pointer: Pointer = arr.value.into();
                    let loc: Cell =
                        format_text(&format!("{}", arr.location), is_selected)
                            .into();
                    let value: Cell =
                        format_text(&format!("{}", as_pointer), is_selected)
                            .into();
                    vec![loc, value]
                } else {
                    vec![]
                }
                .into_iter();
                Row::new(cells).height(1).bottom_margin(0)
            });

        let table = Table::new(rows)
            .header(header)
            .highlight_style(selected_style)
            .highlight_symbol(">> ")
            .widths(&[
                Constraint::Min(19),
                Constraint::Percentage(50),
                Constraint::Min(5),
            ]);
        frame.render_stateful_widget(table, area, self.active_state_mut());
    }
}

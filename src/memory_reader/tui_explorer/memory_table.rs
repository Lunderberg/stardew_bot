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

pub struct MemoryTable {
    state: TableState,
    region: MemoryRegion,
    previous_height: Option<usize>,
    search_state: Option<SearchState>,
}

struct SearchState {
    stack: Vec<SearchItem>,
    table_state: TableState,
    initial_row: usize,
    table_size: usize,
}

struct SearchItem {
    command: SearchCommand,
    search_result: Option<usize>,
}

enum SearchCommand {
    NextResult,
    AddChar(char),
}

const POINTER_SIZE: usize = 8;

impl SearchState {
    fn new(table_state: TableState, table_size: usize) -> Self {
        let initial_row = table_state.selected().unwrap_or(0);
        Self {
            stack: Vec::new(),
            table_state,
            initial_row,
            table_size,
        }
    }

    fn advance_to_next_result<F, const N: usize>(&mut self, row_generator: F)
    where
        F: FnMut(usize) -> [String; N],
    {
        let command = SearchCommand::NextResult;

        let search_start: Option<usize> = self
            .stack
            .last()
            .map(|item| {
                // If the previous search didn't find anything, then
                // don't bother searching again.  Otherwise, start
                // just after it.
                item.search_result.map(|row| row + 1)
            })
            // But if no previous search exists, start at the
            // pre-search selection.
            .unwrap_or(Some(self.initial_row));

        let search_result = search_start
            .map(|row_num| self.search(row_num, None, row_generator))
            .flatten();

        self.stack.push(SearchItem {
            command,
            search_result,
        });
        self.update_table_selection();
    }

    fn add_char<F, const N: usize>(&mut self, c: char, row_generator: F)
    where
        F: FnMut(usize) -> [String; N],
    {
        let command = SearchCommand::AddChar(c);

        // Search starts at the default row, or just before the
        // previous match.
        let search_start: Option<usize> = self
            .stack
            .last()
            .map(|item| {
                // If the previous search didn't find anything, then
                // don't bother searching again.  Otherwise, start
                // at the same location.
                item.search_result
            })
            // But if no previous search exists, start at the
            // pre-search selection.
            .unwrap_or(Some(self.initial_row));

        let search_result = search_start
            .map(|row_num| self.search(row_num, Some(c), row_generator))
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

    fn search<F, const N: usize>(
        &mut self,
        search_start: usize,
        last_char: Option<char>,
        mut row_generator: F,
    ) -> Option<usize>
    where
        F: FnMut(usize) -> [String; N],
    {
        let needle = self.get_search_string(last_char);
        (search_start..self.table_size)
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
            .stack
            .iter()
            .rev()
            .find_map(|item| item.search_result)
            .unwrap_or(self.initial_row);
        self.table_state.select(Some(selected_row));
    }
}

impl MemoryTable {
    pub fn new(region: MemoryRegion) -> Self {
        Self {
            state: TableState::default(),
            region,
            previous_height: None,
            search_state: None,
        }
    }

    pub fn selected_value(&self) -> MemoryValue<[u8; 8]> {
        let byte_offset = self.selected_row() * POINTER_SIZE;
        self.region.bytes_at_offset(byte_offset)
    }

    fn move_selection_relative(&mut self, delta: i64) {
        let n = self.table_size();
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
        self.state.select(Some(row));
        self.cancel_search();
    }

    pub fn move_selection_start(&mut self) {
        self.move_selection_absolute(0);
    }

    pub fn move_selection_end(&mut self) {
        self.move_selection_absolute(self.table_size() - 1);
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
        let region = &self.region;
        if let Some(search_state) = self.search_state.as_mut() {
            search_state
                .advance_to_next_result(|row| Self::row_text(region, row));
        } else {
            self.search_state =
                Some(SearchState::new(self.state.clone(), self.table_size()));
        }
    }

    pub fn add_search_character(&mut self, c: char) {
        let region = &self.region;
        if let Some(search_state) = self.search_state.as_mut() {
            search_state.add_char(c, |row| Self::row_text(region, row));
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

    fn table_size(&self) -> usize {
        self.region.size_bytes() / POINTER_SIZE
    }

    fn row_text(region: &MemoryRegion, row: usize) -> [String; 2] {
        let arr: MemoryValue<[u8; 8]> =
            region.bytes_at_offset(row * POINTER_SIZE);
        let as_pointer: Pointer = arr.value.into();
        [format!("{}", arr.location), format!("{}", as_pointer)]
    }

    fn active_state(&self) -> &TableState {
        self.search_state
            .as_ref()
            .map_or(&self.state, |search_state| &search_state.table_state)
    }

    fn active_state_mut(&mut self) -> &mut TableState {
        self.search_state
            .as_mut()
            .map_or(&mut self.state, |search_state| {
                &mut search_state.table_state
            })
    }

    fn selected_row(&self) -> usize {
        self.active_state().selected().unwrap_or(0)
    }

    pub fn draw<B: Backend>(&mut self, frame: &mut Frame<B>, area: Rect) {
        let border =
            Block::default().borders(Borders::ALL).title("Memory table");

        let inner_area = border.inner(area);

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

        frame.render_widget(border, area);
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
        let widget = Paragraph::new(text)
            .block(Block::default().borders(Borders::ALL).title("Search"));
        frame.render_widget(widget, area);
    }

    fn draw_scrollbar<B: Backend>(&mut self, frame: &mut Frame<B>, area: Rect) {
        let selected = self.selected_row();
        let table_size = self.table_size();
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
        let rows = self.region.iter_bytes().iter_byte_arr().enumerate().map(
            |(i, arr): (_, MemoryValue<[u8; 8]>)| {
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
            },
        );

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

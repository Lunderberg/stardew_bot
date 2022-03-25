use std::iter;

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
    search_stack: Vec<SearchItem>,
    search_scroll_state: Option<TableState>,
}

struct SearchItem {
    command: SearchCommand,
    result: SearchResult,
}

enum SearchCommand {
    Initialize(usize),
    NextResult,
    AddChar(char),
}

enum SearchResult {
    Found(usize),
    NoMatches,
}

const POINTER_SIZE: usize = 8;

impl SearchCommand {
    fn as_char(&self) -> Option<char> {
        if let SearchCommand::AddChar(c) = &self {
            Some(*c)
        } else {
            None
        }
    }
}

impl MemoryTable {
    pub fn new(region: MemoryRegion) -> Self {
        Self {
            state: TableState::default(),
            region,
            previous_height: None,
            search_stack: Vec::new(),
            search_scroll_state: None,
        }
    }

    pub fn selected_value(&self) -> MemoryValue<[u8; 8]> {
        let byte_offset = self.selected_row() * POINTER_SIZE;
        self.region.bytes_at_offset(byte_offset)
    }

    fn move_selection(&mut self, delta: i64) {
        let n = self.table_size();
        let i = match (self.active_state().selected(), delta.signum()) {
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
        self.state.select(Some(i));
        self.cancel_search();
    }

    pub fn move_selection_start(&mut self) {
        self.state.select(Some(0));
        self.cancel_search();
    }

    pub fn move_selection_end(&mut self) {
        self.state.select(Some(self.table_size() - 1));
        self.cancel_search();
    }

    pub fn move_selection_down(&mut self) {
        self.move_selection(1);
    }

    pub fn move_selection_up(&mut self) {
        self.move_selection(-1);
    }

    pub fn move_selection_page_down(&mut self) {
        self.move_selection(self.displayed_rows() as i64);
    }

    pub fn move_selection_page_up(&mut self) {
        self.move_selection(-(self.displayed_rows() as i64));
    }

    pub fn search_is_active(&self) -> bool {
        !self.search_stack.is_empty()
    }

    pub fn search_forward(&mut self) {
        if self.search_stack.is_empty() {
            self.search_command(SearchCommand::Initialize(self.selected_row()));
        } else {
            self.search_command(SearchCommand::NextResult);
        }
        self.update_search_selection();
    }

    pub fn add_search_character(&mut self, c: char) {
        self.search_command(SearchCommand::AddChar(c));
        self.update_search_selection();
    }

    pub fn backspace_search_character(&mut self) {
        self.search_stack.pop();
        self.update_search_selection();
    }

    pub fn cancel_search(&mut self) {
        self.search_stack.clear();
        self.update_search_selection();
    }

    fn update_search_selection(&mut self) {
        let selection =
            self.search_stack
                .iter()
                .rev()
                .find_map(|item| match item.result {
                    SearchResult::Found(pos) => Some(pos),
                    SearchResult::NoMatches => None,
                });

        if let Some(selection) = selection {
            if self.search_scroll_state.is_none() {
                self.search_scroll_state = Some(self.state.clone());
            }
            self.search_scroll_state
                .as_mut()
                .unwrap()
                .select(Some(selection));
        } else {
            self.search_scroll_state = None;
        }
    }

    fn search_command(&mut self, command: SearchCommand) {
        let search_string: String = self
            .search_stack
            .iter()
            .map(|item| &item.command)
            .chain(iter::once(&command))
            .filter_map(|command| command.as_char())
            .collect();

        let previous_result = self
            .search_stack
            .iter()
            .filter_map(|item| {
                if let SearchResult::Found(loc) = item.result {
                    Some(loc)
                } else {
                    None
                }
            })
            .last();

        let result = match (previous_result, &command) {
            (None, SearchCommand::Initialize(loc)) => SearchResult::Found(*loc),
            (Some(prev), SearchCommand::NextResult) => {
                self.search_for(prev + 1, &search_string)
            }
            (Some(prev), SearchCommand::AddChar(_)) => {
                self.search_for(prev, &search_string)
            }
            (None, _) => panic!("Expected initialized stack"),
            (Some(_), SearchCommand::Initialize(_)) => {
                panic!("Expected initialization only at start")
            }
        };

        self.search_stack.push(SearchItem { command, result });
    }

    fn search_for(&self, start_pos: usize, needle: &str) -> SearchResult {
        (start_pos..self.table_size())
            .filter(|row| {
                self.row_text(*row)
                    .iter()
                    .any(|cell_text| cell_text.contains(needle))
            })
            .next()
            .map_or(SearchResult::NoMatches, |pos| SearchResult::Found(pos))
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

    fn row_text(&self, row: usize) -> [String; 2] {
        let arr: MemoryValue<[u8; 8]> =
            self.region.bytes_at_offset(row * POINTER_SIZE);
        let as_pointer: Pointer = arr.value.into();
        [format!("{}", arr.location), format!("{}", as_pointer)]
    }

    fn active_state(&self) -> &TableState {
        self.search_scroll_state.as_ref().unwrap_or(&self.state)
    }

    fn active_state_mut(&mut self) -> &mut TableState {
        self.search_scroll_state.as_mut().unwrap_or(&mut self.state)
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

        let search_area_height = if self.search_stack.is_empty() {
            0
        } else {
            inner_area.height.min(3)
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
        let text = Spans::from(
            self.search_stack
                .iter()
                .filter_map(|item| {
                    item.command.as_char().map(|c| {
                        let c_str = c.to_string();
                        if let SearchResult::NoMatches = item.result {
                            Span::styled(c_str, Style::default().bg(Color::Red))
                        } else {
                            Span::raw(c_str)
                        }
                    })
                })
                .collect::<Vec<_>>(),
        );
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
            .search_stack
            .iter()
            .filter_map(|item| item.command.as_char())
            .collect();

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

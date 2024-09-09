use ratatui::{
    buffer::Buffer,
    layout::Rect,
    style::{Color, Modifier, Style},
    text::Line,
    widgets::{Block, Borders, List, ListState, StatefulWidget, Widget},
};
use regex::RegexBuilder;

use super::{ScrollableState as _, WidgetSideEffects, WidgetWindow};
use crate::{extensions::*, TuiGlobals};

use crate::{KeyBindingMatch, KeySequence};

pub(crate) struct BufferSelection {
    prev_buffer_index: usize,
    list_state: ListState,
    selected_buffer: Option<usize>,
    prev_draw_height: usize,
    filter_string: String,

    /// The index of the currently selected buffer.  If the filter is
    /// empty, will be the same as `list_state.selected()`.
    currently_highlighted_buffer: usize,
}

pub(crate) struct DrawableBufferSelection<'a, 'b> {
    selector: &'a mut BufferSelection,
    buffers: &'a mut [Box<&'b mut dyn WidgetWindow>],
}

impl BufferSelection {
    pub(crate) fn new(prev_buffer_index: usize) -> Self {
        let list_state =
            ListState::default().with_selected(Some(prev_buffer_index));
        Self {
            prev_buffer_index,
            list_state,
            selected_buffer: None,
            prev_draw_height: 1,
            filter_string: Default::default(),
            currently_highlighted_buffer: prev_buffer_index,
        }
    }

    pub(crate) fn selected_buffer(&self) -> Option<usize> {
        self.selected_buffer
    }

    pub(crate) fn drawable<'a, 'b>(
        &'a mut self,
        buffers: &'a mut [Box<&'b mut dyn WidgetWindow>],
    ) -> DrawableBufferSelection<'a, 'b> {
        DrawableBufferSelection {
            selector: self,
            buffers,
        }
    }
}

impl<'a, 'b> WidgetWindow for DrawableBufferSelection<'a, 'b> {
    fn title(&self) -> std::borrow::Cow<str> {
        "Select buffer".into()
    }

    fn apply_key_binding(
        &mut self,
        keystrokes: &KeySequence,
        _globals: &TuiGlobals,
        _side_effects: &mut WidgetSideEffects,
    ) -> KeyBindingMatch {
        KeyBindingMatch::Mismatch
            .or_try_binding("C-g", keystrokes, || {
                self.selector.selected_buffer =
                    Some(self.selector.prev_buffer_index);
            })
            .or_else(|| {
                self.selector.list_state.apply_key_binding(
                    keystrokes,
                    self.buffers.len(),
                    self.selector.prev_draw_height - 3,
                )
            })
            .or_try_binding("<enter>", keystrokes, || {
                self.selector.selected_buffer =
                    Some(self.selector.currently_highlighted_buffer);
            })
            .or_try_binding("<backspace>", keystrokes, || {
                self.selector.filter_string.pop();
            })
            .or_else(|| {
                if let Some(c) = keystrokes.as_char() {
                    self.selector.filter_string.push(c);
                    KeyBindingMatch::Full
                } else {
                    KeyBindingMatch::Mismatch
                }
            })
    }

    fn draw(&mut self, _globals: &TuiGlobals, area: Rect, buf: &mut Buffer) {
        let area = {
            let filter_height = area.height.min(3);
            let (area, filter_area) = area.split_from_bottom(filter_height);
            let widget: Line = self.selector.filter_string.as_str().into();
            let border = Block::default()
                .borders(Borders::ALL)
                .title("Filter string");
            widget.render(border.inner(filter_area), buf);
            border.render(filter_area, buf);

            area
        };

        // Save the height for use in <pageup> and <pagedown> key
        // bindings.
        self.selector.prev_draw_height = area.height as usize;

        // Set up the filter.  Since
        // `self.selector.currently_highlighted_buffer` will need to
        // be mutably borrowed, must be careful not to borrow
        // `self.selector` anywhere.
        let filter_components: Vec<_> = {
            let filter_string = self.selector.filter_string.as_str();
            let is_case_sensitive =
                filter_string.chars().any(char::is_uppercase);
            (!filter_string.is_empty())
                .then(|| filter_string.split_whitespace())
                .into_iter()
                .flatten()
                .map(|filter| {
                    RegexBuilder::new(regex::escape(filter).as_str())
                        .case_insensitive(!is_case_sensitive)
                        .build()
                        .unwrap()
                })
                .collect()
        };

        let highlighted_list_entry = self.selector.list_state.selected();

        let items = self
            .buffers
            .iter()
            .map(|buffer| buffer.title())
            .enumerate()
            .filter(|(_, title)| {
                filter_components.iter().all(|regex| regex.is_match(title))
            })
            .enumerate()
            .inspect(|(i_display, (i_buffer, _))| {
                if Some(*i_display) == highlighted_list_entry {
                    self.selector.currently_highlighted_buffer = *i_buffer;
                }
            })
            .map(|(_, (_, title))| title)
            .map(Line::raw)
            .map(|line| {
                filter_components.iter().fold(line, |line, regex| {
                    line.style_regex_ref(
                        regex,
                        Style::default().fg(Color::LightRed),
                    )
                })
            });

        let buffer_list = List::new(items)
            .highlight_style(Style::default().add_modifier(Modifier::REVERSED))
            .highlight_symbol(">> ");

        StatefulWidget::render(
            buffer_list,
            area,
            buf,
            &mut self.selector.list_state,
        );
    }
}

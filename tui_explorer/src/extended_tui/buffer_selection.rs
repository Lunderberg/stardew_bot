use ratatui::{
    buffer::Buffer,
    layout::Rect,
    style::{Modifier, Style},
    widgets::{List, ListState, StatefulWidget, Widget},
};

use super::{
    ScrollableState as _, SearchDirection, SearchWindow, WidgetGlobals,
    WidgetSideEffects, WidgetWindow,
};
use crate::extensions::*;

use crate::{KeyBindingMatch, KeySequence};

pub(crate) struct BufferSelection {
    prev_buffer_index: usize,
    list_state: ListState,
    selected_buffer: Option<usize>,
    prev_draw_height: usize,
    search: Option<SearchWindow<ListState>>,
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
            search: None,
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

    fn start_search(&mut self, direction: SearchDirection) {
        self.search =
            Some(SearchWindow::new(direction, self.list_state.clone()));
    }

    fn cancel_search(&mut self) {
        if let Some(search) = self.search.take() {
            self.list_state = search.pre_search_state;
        }
    }

    fn finalize_search(&mut self) {
        self.search = None;
    }
}

impl<'a, 'b> WidgetWindow for DrawableBufferSelection<'a, 'b> {
    fn title(&self) -> std::borrow::Cow<str> {
        "Select buffer".into()
    }

    fn apply_key_binding(
        &mut self,
        keystrokes: &KeySequence,
        _globals: WidgetGlobals,
        _side_effects: &mut WidgetSideEffects,
    ) -> KeyBindingMatch {
        KeyBindingMatch::Mismatch
            .or_try_binding("C-g", keystrokes, || {
                self.selector.selected_buffer =
                    Some(self.selector.prev_buffer_index);
            })
            .or_else(|| {
                self.selector
                    .list_state
                    .apply_key_binding(
                        keystrokes,
                        self.buffers.len(),
                        self.selector.prev_draw_height - 3,
                    )
                    .then(|| self.selector.finalize_search())
                    .or_else(|| {
                        if let Some(search) = self.selector.search.as_mut() {
                            search
                                .apply_key_binding(
                                    keystrokes,
                                    self.buffers.len(),
                                    |i| vec![self.buffers[i].title().into()],
                                )
                                .then(|| {
                                    self.selector.list_state.select(Some(
                                        search.recommended_row_selection(),
                                    ))
                                })
                                .or_try_binding("C-g", keystrokes, || {
                                    self.selector.cancel_search()
                                })
                        } else {
                            KeyBindingMatch::Mismatch
                        }
                    })
            })
            .or_try_binding("C-s", keystrokes, || {
                self.selector.start_search(SearchDirection::Forward)
            })
            .or_try_binding("C-r", keystrokes, || {
                self.selector.start_search(SearchDirection::Reverse)
            })
            .or_try_binding("<enter>", keystrokes, || {
                self.selector.selected_buffer =
                    self.selector.list_state.selected();
            })
    }

    fn draw(&mut self, _globals: WidgetGlobals, area: Rect, buf: &mut Buffer) {
        self.selector.prev_draw_height = area.height as usize;

        let area = if let Some(search) = self.selector.search.as_ref() {
            let search_area_height = area.height.min(3);
            let (area, search_area) =
                area.split_from_bottom(search_area_height);
            search.render(search_area, buf);
            area
        } else {
            area
        };

        let items = self
            .buffers
            .iter()
            .map(|buffer| buffer.title())
            .map(ratatui::text::Line::raw);

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

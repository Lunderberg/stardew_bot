use crate::{
    extensions::SplitRect as _,
    inputs::{KeyBindingMatch, KeySequence},
    widgets::BufferSelection,
    TuiGlobals, WidgetSideEffects, WidgetWindow,
};
use crossterm::event::{MouseButton, MouseEvent};
use itertools::Itertools as _;
use ratatui::{
    layout::{Margin, Position, Rect},
    style::{Style, Stylize as _},
    widgets::{Block, Borders, Widget},
};

pub struct DynamicLayout {
    /// Index into the `windows` array.  Should reference a `Buffer`
    /// object, not a `Horizontal` or `Vertical` object.
    active_window: usize,

    /// Defines the nested windows.
    windows: Vec<NestedWindow>,

    /// Index into the `windows` array.  Should reference a
    /// `Horizontal` or `Vertical` object.  If present, indicates that
    /// a click/drag resizing of this divider is currently in
    /// progress.
    currently_resizing: Option<usize>,
}

pub struct DrawableDynamicLayout<'a, B> {
    layout: &'a mut DynamicLayout,
    buffers: &'a mut [B],
    globals: &'a TuiGlobals,
}

struct NestedWindow {
    kind: NestedWindowKind,
    parent: Option<usize>,

    /// The area into which this window was most recently drawn.  May
    /// change if the terminal window is resized, or if the
    /// Horizontal/Vertical splits have been resized.
    area: Option<Rect>,
}

enum ClickLocation {
    WithinWindow(usize),
    WindowBorder(usize),
}

/// The result produced when iterating over the layout.
struct WindowArea {
    /// The area into which this (possibly subdivided) window will be
    /// drawn.
    area: Rect,

    /// An index into the `Dynamiclayout.windows` array.
    window_index: usize,

    /// The index of the buffer displayed in this window.  For
    /// Horizontal/Vertical splits, will be None.
    buffer_index: Option<usize>,

    /// The area of the divider between the split.  For leaf
    /// NestedWindowKind::Buffer nodes, will be None.
    divider_area: Option<Rect>,
}

struct WindowIter<'a> {
    layout: &'a DynamicLayout,
    stack: Vec<usize>,
}

enum NestedWindowKind {
    /// An index into the `windows` vector.  Used for nested windows
    Horizontal {
        left_index: usize,
        right_index: usize,
        left_width: Option<u16>,
        right_width: Option<u16>,
    },

    Vertical {
        top_index: usize,
        bottom_index: usize,
        top_height: Option<u16>,
        bottom_height: Option<u16>,
    },

    /// An index into the `buffers` vector.  The `buffers` vector is
    /// not owned by the `DynamicLayout`, but is provided to the
    /// layout when drawing.
    Buffer(usize),

    /// A window that currently is selecting a new buffer to display.
    BufferSelection(BufferSelection),
}

impl DynamicLayout {
    pub fn new() -> Self {
        Self {
            active_window: 0,
            windows: vec![NestedWindow {
                kind: NestedWindowKind::Buffer(0),
                parent: None,
                area: None,
            }],
            currently_resizing: None,
        }
    }

    pub fn handle_mouse_event(&mut self, mouse: MouseEvent) {
        use crossterm::event::MouseEventKind as Kind;
        match mouse.kind {
            Kind::Down(MouseButton::Left) => {
                let Some(location) = self.clicked_location(Position {
                    x: mouse.column,
                    y: mouse.row,
                }) else {
                    return;
                };

                match location {
                    ClickLocation::WithinWindow(window_index) => {
                        self.active_window = window_index;
                    }
                    ClickLocation::WindowBorder(window_index) => {
                        self.currently_resizing = Some(window_index);
                    }
                }
            }
            Kind::Drag(MouseButton::Left) => {
                if let Some(resize_index) = self.currently_resizing {
                    let window = &mut self.windows[resize_index];
                    if let Some(area) = window.area {
                        match &mut window.kind {
                            NestedWindowKind::Horizontal {
                                left_width,
                                right_width,
                                ..
                            } => {
                                *left_width =
                                    Some(mouse.column.saturating_sub(area.x));
                                *right_width = Some(
                                    area.right().saturating_sub(mouse.column),
                                );
                            }
                            NestedWindowKind::Vertical {
                                top_height,
                                bottom_height,
                                ..
                            } => {
                                *top_height =
                                    Some(mouse.row.saturating_sub(area.y));
                                *bottom_height = Some(
                                    area.bottom().saturating_sub(mouse.row),
                                );
                            }

                            _ => {}
                        }
                    }
                }
            }
            Kind::Up(MouseButton::Left) => {
                self.currently_resizing = None;
            }
            _ => {}
        }
    }

    fn update_draw_areas(&mut self, area: Rect, window_index: usize) {
        self.windows[window_index].area = Some(area);

        match self.windows[window_index].kind {
            NestedWindowKind::Horizontal {
                left_index,
                right_index,
                left_width,
                right_width,
            } => {
                let left_columns = if area.width < 4 {
                    area.width / 2
                } else {
                    match (left_width, right_width) {
                        (Some(lw), Some(rw)) => lw * area.width / (lw + rw),
                        (None, Some(rw)) => area.width.saturating_sub(rw),
                        (Some(lw), None) => lw,
                        (None, None) => area.width / 2,
                    }
                    .clamp(2, area.width - 2)
                };

                let (area_left, area_right) =
                    area.split_from_left(left_columns);
                self.update_draw_areas(area_left, left_index);
                self.update_draw_areas(area_right, right_index);
            }
            NestedWindowKind::Vertical {
                top_index,
                bottom_index,
                top_height,
                bottom_height,
            } => {
                let top_rows = if area.height < 4 {
                    area.height / 2
                } else {
                    match (top_height, bottom_height) {
                        (Some(th), Some(bh)) => th * area.height / (th + bh),
                        (None, Some(bh)) => area.height.saturating_sub(bh),
                        (Some(th), None) => th,
                        (None, None) => area.height / 2,
                    }
                    .clamp(2, area.height - 2)
                };

                let (area_top, area_bottom) = area.split_from_top(top_rows);
                self.update_draw_areas(area_top, top_index);
                self.update_draw_areas(area_bottom, bottom_index);
            }
            _ => {}
        }
    }

    fn iter(&self) -> impl Iterator<Item = WindowArea> + '_ {
        WindowIter {
            layout: self,
            stack: vec![0],
        }
    }

    fn clicked_location(&self, pos: Position) -> Option<ClickLocation> {
        self.iter().find_map(|win| {
            if win.buffer_index.is_some() {
                let inner = win.area.inner(Margin {
                    horizontal: 1,
                    vertical: 1,
                });

                inner
                    .contains(pos)
                    .then(|| ClickLocation::WithinWindow(win.window_index))
            } else {
                let divider = win.divider_area.unwrap();
                divider
                    .contains(pos)
                    .then(|| ClickLocation::WindowBorder(win.window_index))
            }
        })
    }

    pub fn split_horizontally(
        &mut self,
        left_width: Option<u16>,
        right_width: Option<u16>,
    ) {
        let NestedWindowKind::Buffer(active_buffer) =
            self.windows[self.active_window].kind
        else {
            panic!("Internal error, active window should point to Buffer")
        };

        let left_index = self.windows.len();
        self.windows.push(NestedWindow {
            kind: NestedWindowKind::Buffer(active_buffer),
            parent: Some(self.active_window),
            area: None,
        });

        let right_index = self.windows.len();
        self.windows.push(NestedWindow {
            kind: NestedWindowKind::Buffer(active_buffer),
            parent: Some(self.active_window),
            area: None,
        });

        self.windows[self.active_window].kind = NestedWindowKind::Horizontal {
            left_index,
            right_index,
            left_width,
            right_width,
        };
        self.active_window = left_index;
    }

    pub fn split_vertically(
        &mut self,
        top_height: Option<u16>,
        bottom_height: Option<u16>,
    ) {
        let NestedWindowKind::Buffer(active_buffer) =
            self.windows[self.active_window].kind
        else {
            panic!("Internal error, active window should point to Buffer")
        };

        let top_index = self.windows.len();
        self.windows.push(NestedWindow {
            kind: NestedWindowKind::Buffer(active_buffer),
            parent: Some(self.active_window),
            area: None,
        });

        let bottom_index = self.windows.len();
        self.windows.push(NestedWindow {
            kind: NestedWindowKind::Buffer(active_buffer),
            parent: Some(self.active_window),
            area: None,
        });

        self.windows[self.active_window].kind = NestedWindowKind::Vertical {
            top_index,
            bottom_index,
            top_height,
            bottom_height,
        };
        self.active_window = top_index;
    }

    pub fn close_current_window(&mut self) {
        let current = self.active_window;
        let Some(parent) = self.windows[current].parent else {
            return;
        };
        let sibling = match self.windows[parent].kind {
            NestedWindowKind::Horizontal {
                left_index,
                right_index,
                ..
            } => left_index + right_index - current,
            NestedWindowKind::Vertical {
                top_index,
                bottom_index,
                ..
            } => top_index + bottom_index - current,
            _ => panic!("Window parent cannot be a leaf node"),
        };

        self.windows[sibling].parent = self.windows[parent].parent;

        self.windows.swap(sibling, parent);
        self.active_window = parent;
    }

    pub fn close_all_other_windows(&mut self) {
        self.windows.swap(self.active_window, 0);
        self.windows.shrink_to(1);
        self.active_window = 0;
        self.windows[0].parent = None;
    }

    pub fn switch_to_buffer(&mut self, index: usize) {
        let NestedWindowKind::Buffer(buffer_index) =
            &mut self.windows[self.active_window].kind
        else {
            panic!("Internal error, active window should point to leaf node")
        };

        *buffer_index = index;
    }

    pub fn start_buffer_selection(&mut self) {
        let prev_buffer_index = match self.windows[self.active_window].kind {
            NestedWindowKind::Buffer(index) => index,
            NestedWindowKind::BufferSelection(_) => {
                return;
            }
            _ => panic!("Internal error, active window must be leaf node."),
        };
        let selection_window = BufferSelection::new(prev_buffer_index);
        self.windows[self.active_window].kind =
            NestedWindowKind::BufferSelection(selection_window);
    }

    pub fn cycle_next(&mut self) {
        // Walk upward until either reaching the top or finding a node
        // where this is the left/top branch.
        let mut index = self.active_window;
        while let Some(parent) = self.windows[index].parent {
            match self.windows[parent].kind {
                NestedWindowKind::Horizontal {
                    left_index,
                    right_index,
                    ..
                } => {
                    if index == left_index {
                        index = right_index;
                        break;
                    } else {
                        index = parent;
                    }
                }
                NestedWindowKind::Vertical {
                    top_index,
                    bottom_index,
                    ..
                } => {
                    if index == top_index {
                        index = bottom_index;
                        break;
                    } else {
                        index = parent;
                    }
                }
                _ => {
                    panic!("Parent must be a branch node, not a leaf.")
                }
            }
        }

        // Then walk downward along the left/top branch until reaching
        // the bottom.
        loop {
            match self.windows[index].kind {
                NestedWindowKind::Horizontal { left_index, .. } => {
                    index = left_index;
                }
                NestedWindowKind::Vertical { top_index, .. } => {
                    index = top_index;
                }
                _ => {
                    break;
                }
            }
        }

        self.active_window = index;
    }

    pub fn drawable<'a, B>(
        &'a mut self,
        buffers: &'a mut [B],
        globals: &'a TuiGlobals,
    ) -> DrawableDynamicLayout<'a, B> {
        DrawableDynamicLayout {
            layout: self,
            buffers,
            globals,
        }
    }

    pub fn apply_key_binding<'a, 'b>(
        &'a mut self,
        keystrokes: &KeySequence,
        globals: &TuiGlobals,
        side_effects: &mut WidgetSideEffects,
        buffers: &'a mut [Box<&'b mut dyn WidgetWindow>],
    ) -> KeyBindingMatch {
        KeyBindingMatch::Mismatch
            .or_try_bindings(["C-x b", "C-x C-b"], keystrokes, || {
                self.start_buffer_selection()
            })
            .or_try_binding("C-x 2", keystrokes, || {
                self.split_vertically(None, None)
            })
            .or_try_binding("C-x 3", keystrokes, || {
                self.split_horizontally(None, None)
            })
            .or_try_binding("C-x 0", keystrokes, || self.close_current_window())
            .or_try_binding("C-x 1", keystrokes, || {
                self.close_all_other_windows()
            })
            .or_try_binding("C-x o", keystrokes, || {
                self.cycle_next();
            })
            .or_else(|| {
                let window_kind = &mut self.windows[self.active_window].kind;
                match window_kind {
                    NestedWindowKind::Buffer(buffer_index) => buffers
                        [*buffer_index]
                        .apply_key_binding(keystrokes, globals, side_effects),
                    NestedWindowKind::BufferSelection(selector) => {
                        let res = selector.drawable(buffers).apply_key_binding(
                            keystrokes,
                            globals,
                            side_effects,
                        );
                        if let Some(buffer_index) = selector.selected_buffer() {
                            *window_kind =
                                NestedWindowKind::Buffer(buffer_index);
                        }
                        res
                    }
                    _ => panic!("Active window must be leaf node"),
                }
            })
    }
}

impl<'a, 'b> Widget
    for DrawableDynamicLayout<'a, Box<&'b mut (dyn WidgetWindow)>>
{
    fn render(self, area: Rect, buf: &mut ratatui::prelude::Buffer)
    where
        Self: Sized,
    {
        let active_window = self.layout.active_window;

        self.layout.update_draw_areas(area, 0);

        self.layout
            .iter()
            .collect::<Vec<_>>()
            .into_iter()
            .for_each(|win| {
                let widget: &mut dyn WidgetWindow =
                    match &mut self.layout.windows[win.window_index].kind {
                        NestedWindowKind::Buffer(buffer_index) => {
                            *self.buffers[*buffer_index]
                        }
                        NestedWindowKind::BufferSelection(selector) => {
                            &mut selector.drawable(self.buffers)
                        }
                        _ => {
                            return;
                        }
                    };

                let border_style = if win.window_index == active_window {
                    Style::new().light_green()
                } else {
                    Style::new()
                };

                let title = widget.title();

                let border = Block::default()
                    .borders(Borders::ALL)
                    .border_style(border_style)
                    .title(ratatui::text::Line::raw(title));
                let inner_area = border.inner(win.area);
                border.render(win.area, buf);
                widget.draw(self.globals, inner_area, buf);
            });
    }
}

impl<'a> Iterator for WindowIter<'a> {
    type Item = WindowArea;

    fn next(&mut self) -> Option<Self::Item> {
        let window_index = self.stack.pop()?;
        let window = &self.layout.windows[window_index];
        let area = window
            .area
            .expect("Can't iterate over windows until after rendering");

        let item = match window.kind {
            NestedWindowKind::Horizontal {
                left_index,
                right_index,
                ..
            } => {
                let (area_left, area_right) = [left_index, right_index]
                    .into_iter()
                    .map(|index| self.layout.windows[index].area.as_ref())
                    .map(|opt_area| {
                        opt_area.expect(
                            "Can't iterate over windows until after rendering",
                        )
                    })
                    .cloned()
                    .collect_tuple()
                    .unwrap();
                assert!(area_left.width + area_right.width == area.width);

                self.stack.push(right_index);
                self.stack.push(left_index);

                let divider_area = Rect {
                    x: area_left.right() - 1,
                    width: 2,
                    ..area
                };

                WindowArea {
                    area,
                    window_index,
                    buffer_index: None,
                    divider_area: Some(divider_area),
                }
            }
            NestedWindowKind::Vertical {
                top_index,
                bottom_index,
                ..
            } => {
                let (area_top, area_bottom) = [top_index, bottom_index]
                    .into_iter()
                    .map(|index| self.layout.windows[index].area.as_ref())
                    .map(|opt_area| {
                        opt_area.expect(
                            "Can't iterate over windows until after rendering",
                        )
                    })
                    .cloned()
                    .collect_tuple()
                    .unwrap();
                assert!(area_top.height + area_bottom.height == area.height);

                self.stack.push(bottom_index);
                self.stack.push(top_index);

                let divider_area = Rect {
                    y: area_top.bottom() - 1,
                    height: 2,
                    ..area
                };

                WindowArea {
                    area,
                    window_index,
                    buffer_index: None,
                    divider_area: Some(divider_area),
                }
            }
            NestedWindowKind::Buffer(buffer_index) => WindowArea {
                area,
                window_index,
                buffer_index: Some(buffer_index),
                divider_area: None,
            },
            NestedWindowKind::BufferSelection(_) => WindowArea {
                area,
                window_index,
                buffer_index: None,
                divider_area: None,
            },
        };

        Some(item)
    }
}

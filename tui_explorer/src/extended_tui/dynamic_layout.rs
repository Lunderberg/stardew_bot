use crate::extensions::*;
use crossterm::event::{MouseButton, MouseEvent};
use itertools::Itertools as _;
use ratatui::{
    layout::{Margin, Position, Rect},
    style::{Style, Stylize as _},
    widgets::{Block, Borders, Widget},
};

use super::WidgetWindow;

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
}

#[derive(Clone)]
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

#[derive(Clone)]
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
                        (Some(th), Some(bh)) => th * area.width / (th + bh),
                        (None, Some(bh)) => area.width.saturating_sub(bh),
                        (Some(th), None) => th,
                        (None, None) => area.width / 2,
                    }
                    .clamp(2, area.height - 2)
                };

                let (area_top, area_bottom) = area.split_from_top(top_rows);
                self.update_draw_areas(area_top, top_index);
                self.update_draw_areas(area_bottom, bottom_index);
            }
            NestedWindowKind::Buffer(_) => {}
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

        let child = NestedWindow {
            kind: NestedWindowKind::Buffer(active_buffer),
            parent: Some(self.active_window),
            area: None,
        };

        let left_index = self.windows.len();
        self.windows.push(child.clone());

        let right_index = self.windows.len();
        self.windows.push(child);

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

        let child = NestedWindow {
            kind: NestedWindowKind::Buffer(active_buffer),
            parent: Some(self.active_window),
            area: None,
        };

        let top_index = self.windows.len();
        self.windows.push(child.clone());

        let bottom_index = self.windows.len();
        self.windows.push(child);

        self.windows[self.active_window].kind = NestedWindowKind::Vertical {
            top_index,
            bottom_index,
            top_height,
            bottom_height,
        };
        self.active_window = top_index;
    }

    pub fn switch_to_buffer(&mut self, index: usize) {
        let NestedWindowKind::Buffer(buffer_index) =
            &mut self.windows[self.active_window].kind
        else {
            panic!("Internal error, active window should point to leaf node")
        };

        *buffer_index = index;
    }

    pub fn active_buffer(&self) -> usize {
        match self.windows[self.active_window].kind {
            NestedWindowKind::Buffer(buffer_index) => buffer_index,
            _ => panic!(
                "Internal error, active window should point to leaf node"
            ),
        }
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
                NestedWindowKind::Buffer(_) => {
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
                NestedWindowKind::Buffer(_) => {
                    break;
                }
            }
        }

        self.active_window = index;
    }

    pub fn drawable<'a, B>(
        &'a mut self,
        buffers: &'a mut [B],
    ) -> DrawableDynamicLayout<'a, B> {
        DrawableDynamicLayout {
            layout: self,
            buffers,
        }
    }
}

impl<'a, 'b> Widget for DrawableDynamicLayout<'a, Box<dyn WidgetWindow + 'b>> {
    fn render(self, area: Rect, buf: &mut ratatui::prelude::Buffer)
    where
        Self: Sized,
    {
        let active_window = self.layout.active_window;

        self.layout.update_draw_areas(area, 0);

        self.layout
            .iter()
            .filter(|win| win.buffer_index.is_some())
            .for_each(|win| {
                let widget = &mut self.buffers[win.buffer_index.unwrap()];

                let border_style = if win.window_index == active_window {
                    Style::new().light_green()
                } else {
                    Style::new()
                };

                let border = Block::default()
                    .borders(Borders::ALL)
                    .border_style(border_style)
                    .title(widget.title());
                let inner_area = border.inner(win.area);
                border.render(win.area, buf);
                widget.mut_render(inner_area, buf);
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
        };

        Some(item)
    }
}

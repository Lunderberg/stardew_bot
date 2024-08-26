use std::borrow::Cow;

use ratatui::{buffer::Buffer, layout::Rect, widgets::Widget};

pub trait WidgetWindow: Widget {
    /// The title of the widget.  This is used for the title in the
    /// border, and for the widget's name when selecting which widget
    /// to display.
    fn title(&self) -> Cow<str>;

    fn mut_render(&mut self, area: Rect, buf: &mut Buffer);
}

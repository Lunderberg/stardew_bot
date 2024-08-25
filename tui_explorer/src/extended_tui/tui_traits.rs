use ratatui::{buffer::Buffer, layout::Rect, widgets::Widget};

pub trait WidgetWindow: Widget {
    fn title(&self) -> String;

    fn mut_render(&mut self, area: Rect, buf: &mut Buffer);
}

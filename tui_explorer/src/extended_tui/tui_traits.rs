use ratatui::widgets::Widget;

pub trait WidgetWindow: Widget {
    fn title(&self) -> String;
}

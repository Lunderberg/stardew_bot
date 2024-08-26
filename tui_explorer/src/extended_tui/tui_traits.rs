use std::borrow::Cow;

use memory_reader::MemoryReader;

use crate::Annotation;

pub trait WidgetWindow {
    /// The title of the widget.  This is used for the title in the
    /// border, and for the widget's name when selecting which widget
    /// to display.
    fn title(&self) -> Cow<str>;

    /// Draw the window.  This may be able to be simplified once
    /// ratatui's WidgetRef trait is stabilized.
    fn draw<'a>(
        &'a mut self,
        globals: WidgetGlobals<'a>,
        area: ratatui::layout::Rect,
        buf: &mut ratatui::prelude::Buffer,
    );
}

#[derive(Clone, Copy)]
pub(crate) struct WidgetGlobals<'a> {
    pub(crate) reader: &'a MemoryReader,
    pub(crate) annotations: &'a [Annotation],
}

use ratatui::widgets::StatefulWidget;

use crate::extended_tui::WithScrollbar;

pub trait WidgetWithScrollbar: Sized {
    fn with_scrollbar(self, num_rows: usize) -> WithScrollbar<Self>;
}

impl<W: StatefulWidget> WidgetWithScrollbar for W {
    fn with_scrollbar(self, num_rows: usize) -> WithScrollbar<Self> {
        WithScrollbar::new(self, num_rows)
    }
}

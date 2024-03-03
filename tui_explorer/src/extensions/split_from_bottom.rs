use ratatui::layout::Rect;

pub trait SplitFromBottom {
    fn split_from_bottom(self, lines: u16) -> (Rect, Rect);
}

impl SplitFromBottom for Rect {
    fn split_from_bottom(self, lines: u16) -> (Rect, Rect) {
        let lines = lines.min(self.height);

        let top = Rect::new(self.x, self.y, self.width, self.height - lines);

        let bottom =
            Rect::new(self.x, self.bottom() - lines, self.width, lines);

        (top, bottom)
    }
}

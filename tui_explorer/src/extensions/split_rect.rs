use ratatui::layout::Rect;

pub trait SplitRect {
    #![allow(dead_code)]

    fn split_from_bottom(self, lines: u16) -> (Rect, Rect);

    fn split_from_top(self, lines: u16) -> (Rect, Rect);

    fn split_from_left(self, columns: u16) -> (Rect, Rect);

    fn split_from_right(self, columns: u16) -> (Rect, Rect);
}

impl SplitRect for Rect {
    fn split_from_bottom(self, lines: u16) -> (Rect, Rect) {
        let lines = lines.min(self.height);

        let top = Rect::new(self.x, self.y, self.width, self.height - lines);

        let bottom =
            Rect::new(self.x, self.bottom() - lines, self.width, lines);

        (top, bottom)
    }

    fn split_from_top(self, lines: u16) -> (Rect, Rect) {
        let lines = lines.min(self.height);

        let top = Rect::new(self.x, self.y, self.width, lines);

        let bottom =
            Rect::new(self.x, self.y + lines, self.width, self.height - lines);

        (top, bottom)
    }

    fn split_from_left(self, columns: u16) -> (Rect, Rect) {
        let columns = columns.min(self.width);

        let left = Rect::new(self.x, self.y, columns, self.height);

        let right = Rect::new(
            self.x + columns,
            self.y,
            self.width - columns,
            self.height,
        );

        (left, right)
    }

    fn split_from_right(self, columns: u16) -> (Rect, Rect) {
        let columns = columns.min(self.width);

        let left = Rect::new(self.x, self.y, self.width - columns, self.height);

        let right =
            Rect::new(self.right() - columns, self.y, columns, self.height);

        (left, right)
    }
}

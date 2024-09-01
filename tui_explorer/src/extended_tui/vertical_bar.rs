use ratatui::{buffer::Buffer, layout::Rect, style::Style, widgets::Widget};

pub struct VerticalBar {
    bar_top_ratio: f64,
    bar_bottom_ratio: f64,
    line_set: line::Set,
}

impl Default for VerticalBar {
    fn default() -> Self {
        Self {
            bar_top_ratio: 0.0,
            bar_bottom_ratio: 0.0,
            line_set: line::NORMAL,
        }
    }
}

impl VerticalBar {
    pub fn bar_top_ratio(mut self, ratio: f64) -> Self {
        self.bar_top_ratio = ratio.clamp(0.0, 1.0);
        self
    }

    pub fn bar_bottom_ratio(mut self, ratio: f64) -> Self {
        self.bar_bottom_ratio = ratio.clamp(0.0, 1.0);
        self
    }

    #[allow(dead_code)]
    pub fn line_set(mut self, set: line::Set) -> Self {
        self.line_set = set;
        self
    }
}

impl Widget for VerticalBar {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let x = area.left() + area.width / 2;

        // Subtract 1 from area.height, because the Rect is inclusive
        // on the top, but exclusive on the bottom.
        let height = (area.height - 1) as f64;

        let pos_top = (area.top() as f64) + self.bar_top_ratio * height;
        let pos_bottom = (area.top() as f64) + self.bar_bottom_ratio * height;

        let y_top = pos_top.floor() as u16;
        let y_bottom = pos_bottom.ceil() as u16;

        (y_top..=y_bottom).for_each(|y| {
            let yf = y as f64;
            let char = if (0.5..1.0).contains(&(pos_top - yf)) {
                self.line_set.down
            } else if (0.5..1.0).contains(&(yf - pos_bottom)) {
                self.line_set.up
            } else {
                self.line_set.vertical
            };
            buf.set_string(x, y, char, Style::default());
        });
    }
}

// Because the definition in in tui::symbols doesn't include the
// UP/DOWN characters.
#[allow(dead_code)]
pub mod line {
    pub const VERTICAL: &str = "│";
    pub const THICK_VERTICAL: &str = "┃";

    pub const UP: &str = "╵";
    pub const THICK_UP: &str = "╹";

    pub const DOWN: &str = "╷";
    pub const THICK_DOWN: &str = "╻";

    #[derive(Debug, Clone)]
    pub struct Set {
        pub vertical: &'static str,
        pub up: &'static str,
        pub down: &'static str,
    }

    pub const NORMAL: Set = Set {
        vertical: VERTICAL,
        up: UP,
        down: DOWN,
    };

    pub const THICK: Set = Set {
        vertical: THICK_VERTICAL,
        up: THICK_UP,
        down: THICK_DOWN,
    };
}

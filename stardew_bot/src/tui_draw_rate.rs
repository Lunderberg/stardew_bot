use ratatui::{text::Text, widgets::Widget as _};
use tui_utils::WidgetWindow;

use crate::RateCounter;

pub struct TuiDrawRate {
    draw_rate: RateCounter,
}

impl TuiDrawRate {
    pub fn new() -> Self {
        Self {
            draw_rate: RateCounter::new()
                .max_history(std::time::Duration::from_secs(5)),
        }
    }
}

impl WidgetWindow for TuiDrawRate {
    fn title(&self) -> std::borrow::Cow<str> {
        "TUI Draw FPS".into()
    }

    fn periodic_update<'a>(
        &mut self,
        _globals: &'a tui_utils::TuiGlobals,
        _side_effects: &'a mut tui_utils::WidgetSideEffects,
    ) -> Result<(), tui_utils::Error> {
        self.draw_rate.record_event();
        Ok(())
    }

    fn draw<'a>(
        &'a mut self,
        _globals: &'a tui_utils::TuiGlobals,
        area: ratatui::layout::Rect,
        buf: &mut ratatui::prelude::Buffer,
    ) {
        let rate = self.draw_rate.rate_per_second();
        let widget = Text::raw(format!("{rate:.1} FPS"));
        widget.render(area, buf);
    }
}

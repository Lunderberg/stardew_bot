use std::collections::VecDeque;

use ratatui::{text::Text, widgets::Widget as _};
use tui_utils::WidgetWindow;

use crate::{Error, FrameTimingStatistics, RateCounter};

const MAX_FRAMES_HISTORY: usize = 120;
pub struct TuiDrawRate {
    draw_rate: RateCounter,
    history: VecDeque<FrameTimingStatistics>,
}

impl TuiDrawRate {
    pub fn new() -> Self {
        Self {
            draw_rate: RateCounter::new()
                .max_history(std::time::Duration::from_secs(5)),
            history: VecDeque::new(),
        }
    }
}

impl WidgetWindow<Error> for TuiDrawRate {
    fn title(&self) -> std::borrow::Cow<str> {
        "TUI Draw FPS".into()
    }

    fn periodic_update<'a>(
        &mut self,
        _globals: &'a tui_utils::TuiGlobals,
        side_effects: &'a mut tui_utils::WidgetSideEffects,
    ) -> Result<(), Error> {
        self.draw_rate.record_event();
        for frame in side_effects.into_iter::<FrameTimingStatistics>() {
            self.history.push_back(frame);
            while self.history.len() > MAX_FRAMES_HISTORY {
                self.history.pop_front();
            }
        }
        Ok(())
    }

    fn draw<'a>(
        &'a mut self,
        _globals: &'a tui_utils::TuiGlobals,
        area: ratatui::layout::Rect,
        buf: &mut ratatui::prelude::Buffer,
    ) {
        let rate = self.draw_rate.rate_per_second();

        let time_active = self
            .history
            .iter()
            .map(|frame| frame.main_loop_active)
            .sum::<std::time::Duration>();

        let time_handle_input = self
            .history
            .iter()
            .map(|frame| frame.handle_input)
            .sum::<std::time::Duration>();

        let time_periodic_update = self
            .history
            .iter()
            .map(|frame| frame.periodic_update)
            .sum::<std::time::Duration>();

        let time_draw = self
            .history
            .iter()
            .map(|frame| frame.draw)
            .sum::<std::time::Duration>();

        let total_time = self
            .history
            .iter()
            .map(|frame| frame.total_frame_time)
            .sum::<std::time::Duration>();

        let percent_active = 100.0 * time_active.div_duration_f32(total_time);
        let percent_handle_input =
            100.0 * time_handle_input.div_duration_f32(total_time);
        let percent_periodic_update =
            100.0 * time_periodic_update.div_duration_f32(total_time);
        let percent_draw = 100.0 * time_draw.div_duration_f32(total_time);

        let widget = Text::raw(format!(
            "{rate:>3.0} FPS, \
             {percent_active:.0}% active \
             ({percent_handle_input:.0}% input handling, \
             {percent_periodic_update:.0}% periodic updates, \
             {percent_draw:.0}% draw)"
        ));
        widget.render(area, buf);
    }
}

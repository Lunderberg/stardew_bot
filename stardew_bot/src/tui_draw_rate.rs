use std::collections::VecDeque;

use itertools::Itertools as _;
use ratatui::{
    layout::Constraint,
    style::{Modifier, Style},
    text::Text,
    widgets::{Cell, Row, Table, Widget as _},
};
use tui_utils::{extensions::SplitRect as _, WidgetWindow};

use crate::{
    Error, FrameTimingStatistics, RateCounter, WidgetTimingStatistics,
};

const MAX_FRAMES_HISTORY: usize = 120;
pub struct TuiDrawRate {
    draw_rate: RateCounter,
    frame_history: VecDeque<FrameTimingStatistics>,
    widget_history: VecDeque<Vec<WidgetTimingStatistics>>,
}

impl TuiDrawRate {
    pub fn new() -> Self {
        Self {
            draw_rate: RateCounter::new()
                .max_history(std::time::Duration::from_secs(5)),
            frame_history: VecDeque::new(),
            widget_history: VecDeque::new(),
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
            self.frame_history.push_back(frame);
            while self.frame_history.len() > MAX_FRAMES_HISTORY {
                self.frame_history.pop_front();
            }
        }
        self.widget_history.push_back(
            side_effects.into_iter::<WidgetTimingStatistics>().collect(),
        );
        while self.widget_history.len() > MAX_FRAMES_HISTORY {
            self.widget_history.pop_front();
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
            .frame_history
            .iter()
            .map(|frame| frame.main_loop_active)
            .sum::<std::time::Duration>();

        let time_handle_input = self
            .frame_history
            .iter()
            .map(|frame| frame.handle_input)
            .sum::<std::time::Duration>();

        let time_update_per_frame_values = self
            .frame_history
            .iter()
            .map(|frame| frame.update_per_frame_values)
            .sum::<std::time::Duration>();

        let time_bot_logic_per_frame_values = self
            .frame_history
            .iter()
            .map(|frame| frame.update_bot_logic)
            .sum::<std::time::Duration>();

        let time_periodic_update = self
            .frame_history
            .iter()
            .map(|frame| frame.periodic_update)
            .sum::<std::time::Duration>();

        let time_draw = self
            .frame_history
            .iter()
            .map(|frame| frame.draw)
            .sum::<std::time::Duration>();

        let total_time = self
            .frame_history
            .iter()
            .map(|frame| frame.total_frame_time)
            .sum::<std::time::Duration>();

        let percent_active = 100.0 * time_active.div_duration_f32(total_time);
        let percent_handle_input =
            100.0 * time_handle_input.div_duration_f32(total_time);
        let percent_update_per_frame_values =
            100.0 * time_update_per_frame_values.div_duration_f32(total_time);
        let percent_bot_logic_per_frame_values = 100.0
            * time_bot_logic_per_frame_values.div_duration_f32(total_time);
        let percent_periodic_update =
            100.0 * time_periodic_update.div_duration_f32(total_time);
        let percent_draw = 100.0 * time_draw.div_duration_f32(total_time);

        let widget_table_headers: Vec<_> = std::iter::once(Cell::default())
            .chain(
                self.widget_history
                    .iter()
                    .flatten()
                    .map(|timer| timer.step)
                    .unique()
                    .sorted()
                    .map(Cell::new),
            )
            .collect();

        let widget_table = Table::new(
            self.widget_history
                .iter()
                .flatten()
                .map(|timer| ((timer.step, &timer.widget_name), timer.duration))
                .into_grouping_map()
                .sum()
                .into_iter()
                .map(|((step, name), duration)| (name, (step, duration)))
                .into_group_map()
                .into_iter()
                .sorted_by_key(|(name, _)| *name)
                .map(|(name, steps)| {
                    let cells = std::iter::once(Cell::new(name.as_str()))
                        .chain(
                            steps.iter().sorted_by_key(|(step, _)| step).map(
                                |(_, duration)| {
                                    let percent = 100.0
                                        * duration.div_duration_f32(total_time);
                                    Cell::new(format!("{percent:.0}%"))
                                },
                            ),
                        );
                    Row::new(cells)
                }),
            vec![
                Constraint::Ratio(1, widget_table_headers.len() as u32);
                widget_table_headers.len()
            ],
        )
        .header(
            Row::new(widget_table_headers)
                .style(Style::default().add_modifier(Modifier::BOLD)),
        );

        let (summary_area, table_area) = area.split_from_top(2);

        let summary = Text::raw(format!(
            "{rate:>3.0} FPS, \
             {percent_active:.0}% active \
             ({percent_handle_input:.0}% input handling, \
             {percent_update_per_frame_values:.0}% read values, \
             {percent_bot_logic_per_frame_values:.0}% bot logic, \
             {percent_periodic_update:.0}% periodic updates, \
             {percent_draw:.0}% draw)"
        ));
        summary.render(summary_area, buf);

        widget_table.render(table_area, buf);
    }
}

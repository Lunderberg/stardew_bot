use itertools::Itertools;

use dotnet_debugger::{
    PhysicalAccessChain, RuntimePrimValue, SymbolicAccessChain,
};
use ratatui::{
    layout::{Alignment, Constraint},
    style::{Modifier, Style},
    text::{Line, Text},
    widgets::{Row, StatefulWidget as _, Table, TableState},
};
use tui_utils::{
    inputs::{KeyBindingMatch, KeySequence},
    widgets::ScrollableState as _,
    TuiGlobals, WidgetSideEffects, WidgetWindow,
};

pub struct LiveVariableDisplay {
    table_state: TableState,
    live_variables: Vec<LiveVariable>,
    prev_draw_height: usize,
}

struct LiveVariable {
    symbolic_chain: SymbolicAccessChain,
    physical_chain: PhysicalAccessChain,
    most_recent_value: Option<RuntimePrimValue>,
}

impl LiveVariableDisplay {
    pub fn new() -> Self {
        Self {
            live_variables: Vec::new(),
            table_state: Default::default(),
            prev_draw_height: 1,
        }
    }
}

impl WidgetWindow for LiveVariableDisplay {
    fn title(&self) -> std::borrow::Cow<str> {
        "LiveVariableDisplay".into()
    }

    fn apply_key_binding(
        &mut self,
        keystrokes: &KeySequence,
        _globals: &TuiGlobals,
        _side_effects: &mut WidgetSideEffects,
    ) -> KeyBindingMatch {
        KeyBindingMatch::Mismatch.or_else(|| {
            self.table_state.apply_key_binding(
                keystrokes,
                self.live_variables.len(),
                self.prev_draw_height - 3,
            )
        })
    }

    fn periodic_update<'a>(
        &mut self,
        globals: &'a TuiGlobals,
        _side_effects: &'a mut WidgetSideEffects,
    ) -> Result<(), tui_utils::Error> {
        for live_var in self.live_variables.iter_mut() {
            let value =
                live_var.physical_chain.read(globals.cached_reader())?;
            live_var.most_recent_value = value;
        }

        Ok(())
    }

    fn apply_side_effects<'a>(
        &'a mut self,
        globals: &'a TuiGlobals,
        side_effects: &'a mut WidgetSideEffects,
    ) -> Result<(), tui_utils::Error> {
        side_effects
            .into_iter::<SymbolicAccessChain>()
            .try_for_each(|symbolic_chain| {
                let reader = globals.cached_reader();
                let physical_chain =
                    PhysicalAccessChain::derive(&symbolic_chain, reader)?;
                let live_var = LiveVariable {
                    symbolic_chain,
                    physical_chain,
                    most_recent_value: None,
                };
                self.live_variables.push(live_var);
                Ok(())
            })
    }

    fn draw<'a>(
        &'a mut self,
        _globals: &'a TuiGlobals,
        area: ratatui::layout::Rect,
        buf: &mut ratatui::prelude::Buffer,
    ) {
        self.prev_draw_height = area.height as usize;

        let width_right = 10;
        let width_left = (area.width as usize).saturating_sub(width_right);

        let exprs = self
            .live_variables
            .iter()
            .map(|live_var| format!("{}", live_var.symbolic_chain))
            .collect::<Vec<_>>();

        let rows = self.live_variables.iter().zip(exprs.iter()).map(
            |(live_var, expr)| {
                let name_lines: Vec<_> = std::iter::empty()
                    .chain(std::iter::once(0))
                    .chain(
                        expr.match_indices("\u{200B}").map(|(index, _)| index),
                    )
                    .chain(std::iter::once(expr.len()))
                    .tuple_windows()
                    .peekable()
                    .batching(|iter| {
                        let (start, mut end) = iter.next()?;
                        while let Some((_, new_end)) =
                            iter.next_if(|(_, b)| b - start < width_left)
                        {
                            end = new_end;
                        }
                        Some((start, end))
                    })
                    .map(|(start, end)| &expr[start..end])
                    .map(|line| Line::from(line).alignment(Alignment::Right))
                    .collect();

                let height = name_lines.len();

                Row::new([
                    Text::from(name_lines),
                    Text::from(match &live_var.most_recent_value {
                        Some(value) => format!("{value}"),
                        None => String::default(),
                    }),
                ])
                .height(height as u16)
            },
        );

        let table = Table::new(
            rows,
            [
                Constraint::Percentage(100),
                Constraint::Min(width_right as u16),
            ],
        );

        let table = table
            .highlight_style(Style::default().add_modifier(Modifier::REVERSED))
            .highlight_symbol(">> ");

        table.render(area, buf, &mut self.table_state);
    }
}

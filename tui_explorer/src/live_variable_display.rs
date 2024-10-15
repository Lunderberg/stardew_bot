use dotnet_debugger::{
    PhysicalAccessChain, RuntimePrimValue, SymbolicAccessChain,
};
use ratatui::{
    layout::Constraint,
    style::{Modifier, Style},
    widgets::{Row, StatefulWidget as _, Table, TableState},
};

use crate::{
    extended_tui::{ScrollableState as _, WidgetSideEffects, WidgetWindow},
    Error, KeyBindingMatch, KeySequence, TuiGlobals,
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

    fn add_live_variable<'a>(
        &'a mut self,
        globals: &'a TuiGlobals,
        _side_effects: &'a mut WidgetSideEffects,
        symbolic_chain: &'a SymbolicAccessChain,
    ) -> Result<(), Error> {
        let reader = globals.cached_reader();
        let symbolic_chain = symbolic_chain.clone();
        let physical_chain =
            PhysicalAccessChain::derive(&symbolic_chain, reader)?;
        let live_var = LiveVariable {
            symbolic_chain,
            physical_chain,
            most_recent_value: None,
        };
        self.live_variables.push(live_var);
        Ok(())
    }

    fn periodic_update<'a>(
        &mut self,
        globals: &'a crate::TuiGlobals,
        _side_effects: &'a mut crate::extended_tui::WidgetSideEffects,
    ) -> Result<(), crate::Error> {
        for live_var in self.live_variables.iter_mut() {
            let value = live_var.physical_chain.read(&globals.reader)?;
            live_var.most_recent_value = Some(value);
        }

        Ok(())
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

    fn draw<'a>(
        &'a mut self,
        _globals: &'a crate::TuiGlobals,
        area: ratatui::layout::Rect,
        buf: &mut ratatui::prelude::Buffer,
    ) {
        self.prev_draw_height = area.height as usize;

        let rows = self
            .live_variables
            .iter()
            // .flat_map(|live_var| {
            // [
            //     [format!("{}", live_var.symbolic_chain)],
            //     [format!("{}", live_var.physical_chain)],
            // ]
            //})
            .map(|live_var| {
                [
                    format!("{}", live_var.symbolic_chain),
                    match &live_var.most_recent_value {
                        Some(value) => format!("{value}"),
                        None => String::default(),
                    },
                ]
            })
            .map(Row::new);

        let table = Table::new(
            rows,
            [Constraint::Percentage(100), Constraint::Min(10)],
        );

        let table = table
            .highlight_style(Style::default().add_modifier(Modifier::REVERSED))
            .highlight_symbol(">> ");

        table.render(area, buf, &mut self.table_state);
    }
}

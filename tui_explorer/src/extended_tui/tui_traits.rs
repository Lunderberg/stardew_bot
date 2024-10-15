use std::{borrow::Cow, ops::Range};

use dotnet_debugger::SymbolicAccessChain;
use memory_reader::Pointer;

use crate::{Error, KeyBindingMatch, KeySequence, TuiGlobals};

use super::Annotation;

pub trait WidgetWindow {
    /// The title of the widget.  This is used for the title in the
    /// border, and for the widget's name when selecting which widget
    /// to display.
    fn title(&self) -> Cow<str>;

    /// Check whether window-specific key bindings should have an
    /// effect.
    fn apply_key_binding<'a>(
        &'a mut self,
        _keystrokes: &'a KeySequence,
        _globals: &'a TuiGlobals,
        _side_effects: &'a mut WidgetSideEffects,
    ) -> KeyBindingMatch {
        KeyBindingMatch::Mismatch
    }

    fn periodic_update<'a>(
        &mut self,
        _globals: &'a TuiGlobals,
        _side_effects: &'a mut WidgetSideEffects,
    ) -> Result<(), Error> {
        Ok(())
    }

    fn change_address<'a>(
        &'a mut self,
        _globals: &'a TuiGlobals,
        _side_effects: &'a mut WidgetSideEffects,
        _address: Pointer,
    ) {
    }

    fn add_live_variable<'a>(
        &'a mut self,
        _globals: &'a TuiGlobals,
        _side_effects: &'a mut WidgetSideEffects,
        _access_chain: &'a SymbolicAccessChain,
    ) -> Result<(), Error> {
        Ok(())
    }

    /// Draw the window.  This may be able to be simplified once
    /// ratatui's WidgetRef trait is stabilized.
    fn draw<'a>(
        &'a mut self,
        globals: &'a TuiGlobals,
        area: ratatui::layout::Rect,
        buf: &mut ratatui::prelude::Buffer,
    );
}

#[derive(Default)]
pub struct WidgetSideEffects {
    pub(crate) change_address: Option<Pointer>,
    pub(crate) log_messages: Vec<String>,
    pub(crate) annotations: Vec<Annotation>,
    pub(crate) live_variable: Option<SymbolicAccessChain>,
}

impl WidgetSideEffects {
    pub(crate) fn change_address(&mut self, ptr: Pointer) {
        self.change_address = Some(ptr);
    }

    pub(crate) fn add_log(&mut self, message: impl Into<String>) {
        self.log_messages.push(message.into());
    }
}

impl dll_unpacker::Annotator for WidgetSideEffects {
    fn range(
        &mut self,
        range: Range<Pointer>,
    ) -> &mut impl dll_unpacker::Annotation {
        self.annotations.range(range)
    }
}

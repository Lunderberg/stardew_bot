use std::borrow::Cow;

use memory_reader::{MemoryReader, MemoryRegion, Pointer};

use crate::{Annotation, Error, KeyBindingMatch, KeySequence};

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
        _globals: WidgetGlobals<'a>,
        _side_effects: &'a mut WidgetSideEffects,
    ) -> KeyBindingMatch {
        KeyBindingMatch::Mismatch
    }

    fn periodic_update<'a>(
        &mut self,
        _globals: WidgetGlobals<'a>,
        _side_effects: &'a mut WidgetSideEffects,
    ) -> Result<(), Error> {
        Ok(())
    }

    fn change_address<'a>(
        &'a mut self,
        _globals: WidgetGlobals<'a>,
        _side_effects: &'a mut WidgetSideEffects,
        _address: Pointer,
    ) {
    }

    /// Draw the window.  This may be able to be simplified once
    /// ratatui's WidgetRef trait is stabilized.
    fn draw<'a>(
        &'a mut self,
        globals: WidgetGlobals<'a>,
        area: ratatui::layout::Rect,
        buf: &mut ratatui::prelude::Buffer,
    );
}

#[derive(Clone, Copy)]
pub(crate) struct WidgetGlobals<'a> {
    pub(crate) reader: &'a MemoryReader,
    pub(crate) current_region: &'a MemoryRegion,
    pub(crate) annotations: &'a [Annotation],
}

#[derive(Default)]
pub struct WidgetSideEffects {
    pub(crate) change_address: Option<Pointer>,
    pub(crate) log_messages: Vec<String>,
}

impl WidgetSideEffects {
    pub(crate) fn change_address(&mut self, ptr: Pointer) {
        self.change_address = Some(ptr);
    }

    pub(crate) fn add_log(&mut self, message: impl Into<String>) {
        self.log_messages.push(message.into());
    }
}

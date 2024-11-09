use std::{any::Any, borrow::Cow};

use dotnet_debugger::{CachedReader, StaticValueCache};
use memory_reader::MemoryReader;

use crate::inputs::{KeyBindingMatch, KeySequence};

/// Contains information that may be required by more than one widget.
/// A read-only reference is provided to each widget during input
/// handling and rendering.
pub struct TuiGlobals {
    reader: MemoryReader,
    static_value_cache: StaticValueCache,
    type_map: anymap::Map,
}

#[derive(Default)]
pub struct WidgetSideEffects {
    broadcast: Vec<Box<dyn Any>>,
}

pub struct LogMessage(pub String);

pub trait WidgetWindow<E> {
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

    fn apply_side_effects<'a>(
        &'a mut self,
        _globals: &'a TuiGlobals,
        _side_effects: &'a mut WidgetSideEffects,
    ) -> Result<(), E> {
        Ok(())
    }

    fn periodic_update<'a>(
        &mut self,
        _globals: &'a TuiGlobals,
        _side_effects: &'a mut WidgetSideEffects,
    ) -> Result<(), E> {
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

impl TuiGlobals {
    pub fn new(reader: MemoryReader) -> Self {
        Self {
            reader,
            static_value_cache: StaticValueCache::new(),
            type_map: anymap::Map::new(),
        }
    }

    pub fn reader(&self) -> &MemoryReader {
        &self.reader
    }

    pub fn cached_reader(&self) -> CachedReader {
        self.static_value_cache.cached_reader(&self.reader)
    }

    pub fn insert<T: Any>(&mut self, obj: T) {
        self.type_map.insert(obj);
    }

    pub fn get<T: Any>(&self) -> Option<&T> {
        self.type_map.get()
    }

    pub fn get_mut<T: Any>(&mut self) -> Option<&mut T> {
        self.type_map.get_mut()
    }

    pub fn get_or_default<T: Any + Default>(&mut self) -> &mut T {
        self.type_map.entry::<T>().or_insert_with(T::default)
    }
}

impl WidgetSideEffects {
    pub fn add_log(&mut self, message: impl Into<String>) {
        self.broadcast(LogMessage(message.into()));
    }

    pub fn broadcast(&mut self, obj: impl Any) {
        self.broadcast.push(Box::new(obj));
    }

    pub fn iter<T: Any>(&self) -> impl Iterator<Item = &T> + '_ {
        self.broadcast
            .iter()
            .filter_map(|any| any.downcast_ref::<T>())
    }

    pub fn into_iter<T: Any>(&mut self) -> impl Iterator<Item = T> + '_ {
        let mut offset = 0;
        (0..self.broadcast.len()).filter_map(move |i| {
            let i = i - offset;
            self.broadcast[i].is::<T>().then(|| {
                let any = self.broadcast.swap_remove(i);
                let obj = any
                    .downcast::<T>()
                    .expect("Unreachable due to previous Any::is check");
                offset += 1;
                *obj
            })
        })
    }
}

use std::cmp::Reverse;

use itertools::Itertools as _;
use ratatui::{
    layout::{Constraint, Layout, Rect},
    text::Text,
    widgets::Widget,
};

use memory_reader::{MemoryRegion, Pointer};
use tui_utils::{
    extensions::SplitRect as _, TuiGlobals, WidgetSideEffects, WidgetWindow,
};

use crate::{Annotation, ChangeAddress, Error, InfoFormatter};

pub struct DetailView {
    values: Vec<(String, String)>,
    formatters: Vec<Box<dyn InfoFormatter>>,
}

impl DetailView {
    pub(crate) fn new(formatters: Vec<Box<dyn InfoFormatter>>) -> Self {
        Self {
            values: Vec::new(),
            formatters,
        }
    }

    pub(crate) fn update_details(
        &mut self,
        globals: &TuiGlobals,
        pointer: Pointer,
    ) {
        let from_annotations = globals
            .get::<Vec<Annotation>>()
            .into_iter()
            .flatten()
            .filter(|ann| {
                ann.range.start < pointer + MemoryRegion::POINTER_SIZE
                    && pointer < ann.range.end
            })
            .sorted_by_key(|ann| (ann.range.start, Reverse(ann.range.end)))
            .map(|ann| (ann.name.clone(), ann.value.clone()));

        let current_region = globals
            .get::<MemoryRegion>()
            .expect("Should be initialized with a memory region");

        let from_formatters = self.formatters.iter().filter_map(|formatter| {
            formatter
                .format(globals.reader(), &current_region, pointer)
                .map(|text| (formatter.name().to_string(), text))
        });

        self.values = from_annotations.chain(from_formatters).collect();
    }
}

impl<'a> Widget for &'a DetailView {
    fn render(self, area: Rect, buf: &mut ratatui::prelude::Buffer)
    where
        Self: Sized,
    {
        self.values
            .iter()
            .map(|(_, value)| value.chars().filter(|c| *c == '\n').count() + 1)
            .map(|height| -> u16 { height.try_into().unwrap() })
            .scan(area, |remaining_area, height| {
                if remaining_area.is_empty() {
                    None
                } else {
                    let (row, below_row) =
                        remaining_area.split_from_top(height);
                    *remaining_area = below_row;
                    Some(row)
                }
            })
            .zip(self.values.iter())
            .for_each(|(row_area, (key, value))| {
                let key: Text = key.as_str().into();

                if value.is_empty() {
                    key.render(row_area, buf)
                } else {
                    let value: Text = value.as_str().into();
                    let (key_area, value_area) = Layout::horizontal([
                        Constraint::Min(15),
                        Constraint::Percentage(100),
                    ])
                    .spacing(1)
                    .split(row_area)
                    .into_iter()
                    .cloned()
                    .collect_tuple()
                    .unwrap();

                    key.render(key_area, buf);
                    value.render(value_area, buf);
                }
            });
    }
}

impl WidgetWindow<Error> for DetailView {
    fn title(&self) -> std::borrow::Cow<str> {
        "Detail View".into()
    }

    fn apply_side_effects<'a>(
        &'a mut self,
        globals: &'a TuiGlobals,
        side_effects: &'a mut WidgetSideEffects,
    ) -> Result<(), Error> {
        side_effects
            .iter::<ChangeAddress>()
            .for_each(|change| self.update_details(globals, change.0));
        Ok(())
    }

    fn draw<'a>(
        &'a mut self,
        _: &'a TuiGlobals,
        area: ratatui::layout::Rect,
        buf: &mut ratatui::prelude::Buffer,
    ) {
        self.render(area, buf)
    }
}

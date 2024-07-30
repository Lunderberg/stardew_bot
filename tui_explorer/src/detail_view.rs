use std::cmp::Reverse;

use itertools::Itertools as _;
use ratatui::{
    layout::{Constraint, Layout, Rect},
    text::Text,
    widgets::{Block, Borders},
    Frame,
};

use memory_reader::{MemoryReader, MemoryRegion, Pointer};

use crate::extensions::*;
use crate::{Annotation, InfoFormatter};

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
        reader: &MemoryReader,
        region: &MemoryRegion,
        annotations: &[Annotation],
        pointer: Pointer,
    ) {
        let from_annotations = annotations
            .iter()
            .filter(|ann| {
                ann.range.start < pointer + MemoryRegion::POINTER_SIZE
                    && pointer < ann.range.end
            })
            .sorted_by_key(|ann| (ann.range.start, Reverse(ann.range.end)))
            .map(|ann| (ann.name.clone(), ann.value.clone()));

        let from_formatters = self.formatters.iter().filter_map(|formatter| {
            formatter
                .format(reader, region, pointer)
                .map(|text| (formatter.name().to_string(), text))
        });

        self.values = from_annotations.chain(from_formatters).collect();
    }

    pub(crate) fn draw(&mut self, frame: &mut Frame, area: Rect) {
        let border =
            Block::default().borders(Borders::ALL).title("Detail View");
        let inner_area = border.inner(area);
        frame.render_widget(border, area);
        let area = inner_area;

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
                    frame.render_widget(key, row_area);
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

                    frame.render_widget(key, key_area);
                    frame.render_widget(value, value_area);
                }
            });
    }
}

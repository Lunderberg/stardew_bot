use std::cmp::Reverse;

use ratatui::{
    layout::{Constraint, Rect},
    widgets::{Cell, StatefulWidget, Table, TableState, Widget},
};

use memory_reader::{MemoryReader, MemoryRegion, MemoryValue, Pointer};

use crate::{
    extended_tui::{WidgetGlobals, WidgetWindow},
    extensions::*,
};

pub struct StackFrameTable {
    stack_frames: Vec<StackFrame>,
    table_state: TableState,
}

struct StackFrame {
    frame_pointer: MemoryValue<Pointer>,
    return_address: MemoryValue<Pointer>,
}

pub struct DrawableStackFrameTable<'a> {
    table: &'a mut StackFrameTable,
    reader: &'a MemoryReader,
}

impl StackFrameTable {
    pub(crate) fn new(reader: &MemoryReader, stack: &MemoryRegion) -> Self {
        let stack_frames: Vec<_> = stack
            .stack_pointers(reader)
            .map(|frame_pointer| {
                let return_address = stack
                    .bytes_at_pointer(
                        frame_pointer.location + MemoryRegion::POINTER_SIZE,
                    )
                    .expect(
                        "Return address was out of bounds of the memory region",
                    )
                    .map(Into::into);

                StackFrame {
                    frame_pointer,
                    return_address,
                }
            })
            .collect();
        Self {
            stack_frames,
            table_state: TableState::default(),
        }
    }

    pub(crate) fn select_address(&mut self, address: Pointer) {
        let frame_containing_address = self
            .stack_frames
            .binary_search_by_key(&Reverse(address), |frame| {
                Reverse(frame.frame_pointer.location)
            })
            .unwrap_or_else(|i| i);

        let row = self.stack_frames.len() - frame_containing_address;

        self.table_state.select(Some(row));
    }
}

impl<'a> Widget for DrawableStackFrameTable<'a> {
    fn render(self, area: Rect, buf: &mut ratatui::prelude::Buffer)
    where
        Self: Sized,
    {
        let table: Table = self
            .table
            .stack_frames
            .iter()
            .rev()
            .map(|frame| {
                [
                    Cell::new(format!("{}", frame.frame_pointer.location)),
                    Cell::new(
                        self.reader
                            .find_containing_region(frame.return_address.value)
                            .map(|reg| reg.short_name())
                            .unwrap_or(""),
                    ),
                ]
                .collect_row()
            })
            .chain(std::iter::once(["Prelude"].collect_row()))
            .collect_table()
            .widths([Constraint::Min(20), Constraint::Percentage(100)])
            .highlight_symbol(">> ");

        StatefulWidget::render(table, area, buf, &mut self.table.table_state);
    }
}

impl WidgetWindow for StackFrameTable {
    fn title(&self) -> std::borrow::Cow<str> {
        "Stack Frames".into()
    }

    fn draw<'a>(
        &'a mut self,
        globals: WidgetGlobals<'a>,
        area: ratatui::layout::Rect,
        buf: &mut ratatui::prelude::Buffer,
    ) {
        DrawableStackFrameTable {
            table: self,
            reader: globals.reader,
        }
        .render(area, buf)
    }
}

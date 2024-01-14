use std::cmp::Reverse;

use ratatui::{
    layout::{Constraint, Rect},
    widgets::{Block, Borders, Cell, Table, TableState},
    Frame,
};

use crate::{
    memory_reader::{MemoryRegion, MemoryValue, Pointer},
    MemoryReader,
};

use super::extensions::*;

pub struct StackFrameTable {
    stack_frames: Vec<StackFrame>,
    table_state: TableState,
}

struct StackFrame {
    frame_pointer: MemoryValue<Pointer>,
    return_address: MemoryValue<Pointer>,
}

impl StackFrameTable {
    pub fn new(reader: &MemoryReader, stack: &MemoryRegion) -> Self {
        let stack_frames: Vec<_> = stack
            .stack_pointers(reader.libc_address_ranges())
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

    pub fn select_address(&mut self, address: Pointer) {
        let frame_containing_address = self
            .stack_frames
            .binary_search_by_key(&Reverse(address), |frame| {
                Reverse(frame.frame_pointer.location)
            })
            .unwrap_or_else(|i| i);

        let row = self.stack_frames.len() - frame_containing_address;

        self.table_state.select(Some(row));
    }

    pub fn draw(
        &mut self,
        frame: &mut Frame,
        area: Rect,
        reader: &MemoryReader,
    ) {
        let table: Table = self
            .stack_frames
            .iter()
            .rev()
            .map(|frame| {
                [
                    Cell::new(format!("{}", frame.frame_pointer.location)),
                    Cell::new(
                        reader
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
            .highlight_symbol(">> ")
            .block(
                Block::default().borders(Borders::ALL).title("Stack Frames"),
            );

        frame.render_stateful_widget(table, area, &mut self.table_state);
    }
}

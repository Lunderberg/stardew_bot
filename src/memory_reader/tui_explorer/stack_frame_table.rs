use std::cmp::Reverse;

use ratatui::{
    layout::Rect,
    widgets::{Block, Borders, List, ListState},
    Frame,
};

use crate::{
    memory_reader::{MemoryRegion, Pointer},
    MemoryReader,
};

use super::extensions::*;

pub struct StackFrameTable {
    stack_pointers: Vec<Pointer>,
    list_state: ListState,
}

impl StackFrameTable {
    pub fn new(reader: &MemoryReader, stack: &MemoryRegion) -> Self {
        let stack_pointers: Vec<_> =
            stack.stack_pointers(reader.libc_address_ranges()).collect();
        Self {
            stack_pointers,
            list_state: ListState::default(),
        }
    }

    pub fn select_address(&mut self, address: Pointer) {
        let frame_containing_address = self
            .stack_pointers
            .binary_search_by_key(&Reverse(address), |&p| Reverse(p))
            .unwrap_or_else(|i| i);

        let row = self.stack_pointers.len() - frame_containing_address;

        self.list_state.select(Some(row));
    }

    pub fn draw(&mut self, frame: &mut Frame, area: Rect) {
        let stack_list: List = self
            .stack_pointers
            .iter()
            .rev()
            .map(|p| format!("{p}"))
            .chain(std::iter::once("Prelude".to_string()))
            .collect_list()
            .highlight_symbol(">> ")
            .block(
                Block::default().borders(Borders::ALL).title("Stack Frames"),
            );

        frame.render_stateful_widget(stack_list, area, &mut self.list_state);
    }
}

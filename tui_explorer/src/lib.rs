// This warning triggers for use of `Itertools::intersperse`.  Because
// stabilizing `Iterator::intersperse` would break a large number of
// crates [0], it is instead waiting for RFC#2845 [1] to land [2]
// before stabilization.  At that point, the warning won't be needed
// anymore.  So, disabling the warning, because it is warning about a
// potential future breakage that is going to be avoided anyways.
//
// [0] https://github.com/rust-lang/rust/issues/88967
// [1] https://github.com/rust-lang/rfcs/pull/2845
// [2] https://github.com/rust-lang/rust/issues/89151
#![allow(unstable_name_collisions)]

mod tui_explorer;
pub use tui_explorer::*;

mod error;
pub use error::Error;

mod key_sequence;
pub use key_sequence::{KeyBindingMatch, KeySequence};

mod terminal_context;
pub use terminal_context::*;

mod sigint_handler;
pub use sigint_handler::*;

mod search_window;
pub use search_window::*;

mod running_log;
pub use running_log::*;

mod stack_frame_table;
pub use stack_frame_table::*;

mod memory_table;
pub use memory_table::*;

mod detail_view;
pub use detail_view::*;

mod vertical_bar;
pub use vertical_bar::*;

pub mod column_formatter;
pub use column_formatter::ColumnFormatter;

pub mod info_formatter;
pub use info_formatter::InfoFormatter;

pub(crate) mod extensions;

mod non_empty_vec;
pub(crate) use non_empty_vec::NonEmptyVec;

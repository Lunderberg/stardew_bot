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

mod annotation;
pub(crate) use annotation::Annotation;

mod running_log;
pub use running_log::*;

mod stack_frame_table;
pub use stack_frame_table::*;

mod memory_table;
pub use memory_table::*;

mod detail_view;
pub use detail_view::*;

mod runtime_module_view;
pub use runtime_module_view::*;

mod object_explorer;
pub use object_explorer::*;

mod metadata_display;
pub use metadata_display::*;

pub mod column_formatter;
pub(crate) use column_formatter::ColumnFormatter;

pub mod info_formatter;
pub use info_formatter::InfoFormatter;

mod user_config;
pub(crate) use user_config::UserConfig;

mod user_config_editor;
pub(crate) use user_config_editor::UserConfigEditor;

mod live_variable_display;
pub(crate) use live_variable_display::LiveVariableDisplay;

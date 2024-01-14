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

mod memory_reader;
pub use memory_reader::MemoryReader;

mod error;
pub use error::{Error, Result};

mod memory_map_region;
pub use memory_map_region::*;

mod memory_region;
pub use memory_region::*;

mod pointer;
pub use pointer::*;

mod memory_value;
pub use memory_value::*;

mod tui_explorer;
pub use tui_explorer::*;

mod sigint_handler;
pub use sigint_handler::*;

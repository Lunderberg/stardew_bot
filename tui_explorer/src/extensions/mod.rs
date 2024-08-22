#![allow(unused_imports)]

mod split_into_tuple;
pub use split_into_tuple::SplitIntoTuple as _;

mod collect_ratatui;
// pub use collect_ratatui::CollectRatatuiList as _;
pub use collect_ratatui::CollectRatatuiRow as _;
pub use collect_ratatui::CollectRatatuiTable as _;

mod with_scrollbar;
pub use with_scrollbar::WidgetWithScrollbar as _;

mod split_rect;
pub use split_rect::SplitRect as _;

mod split_span;
pub use split_span::SplitSpan as _;

mod highlight_ratatui;
pub use highlight_ratatui::HighlightLine as _;

mod try_find;
pub use try_find::TryFind as _;

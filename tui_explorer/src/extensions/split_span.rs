use std::borrow::Cow;

use ratatui::text::Span;

pub trait SplitSpan: Sized {
    fn split_at(self, index: usize) -> (Self, Self);
}

impl<'a> SplitSpan for Span<'a> {
    fn split_at(self, index: usize) -> (Self, Self) {
        let Self { content, style } = self;
        let (a, b) = match content {
            Cow::Borrowed(borrowed) => {
                let (a, b) = borrowed.split_at(index);
                (Cow::Borrowed(a), Cow::Borrowed(b))
            }
            Cow::Owned(mut owned) => {
                let rhs = owned.split_off(index);
                (Cow::Owned(owned), Cow::Owned(rhs))
            }
        };
        (Self::styled(a, style), Self::styled(b, style))
    }
}

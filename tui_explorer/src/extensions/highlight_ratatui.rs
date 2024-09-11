use std::borrow::{Borrow, Cow};
use std::fmt::Debug;

use ratatui::prelude::*;
use regex::Regex;

use crate::extensions::*;

pub trait HighlightLine {
    fn style_regex_ref(
        self,
        regex: impl Borrow<Regex>,
        style: impl Into<Style>,
    ) -> Self;

    fn style_regex<R>(self, regex: R, style: impl Into<Style>) -> Self
    where
        Self: Sized,
        R: TryInto<Regex>,
        R::Error: Debug,
    {
        let regex = regex.try_into().expect("Invalid regex");
        self.style_regex_ref(&regex, style)
    }

    #[allow(dead_code)]
    fn style_substring(self, substring: &str, style: impl Into<Style>) -> Self
    where
        Self: Sized,
    {
        if substring.is_empty() {
            self
        } else {
            self.style_regex(regex::escape(substring).as_str(), style)
        }
    }
}

impl<'a> HighlightLine for Line<'a> {
    fn style_regex_ref(
        self,
        regex: impl Borrow<Regex>,
        style: impl Into<Style>,
    ) -> Self {
        let text: String =
            self.spans.iter().map(|span| span.content.clone()).collect();

        let regex = regex.borrow();
        let style: Style = style.into();

        let mut iter_match_range = regex
            .captures_iter(&text)
            .map(|regex_capture| {
                regex_capture
                    .name("highlight")
                    .unwrap_or_else(|| regex_capture.get(0).unwrap())
            })
            .map(|regex_match| regex_match.range());
        let mut iter_span = self.spans.into_iter();
        let mut i_span_begin = 0;

        let mut spans = Vec::new();
        let mut opt_span = None;
        let mut opt_range = None;

        loop {
            opt_range = opt_range.or_else(|| iter_match_range.next());
            let Some(range) = opt_range.take() else {
                break;
            };

            opt_span = opt_span.or_else(|| iter_span.next());
            let Some(span) = opt_span.take() else {
                break;
            };

            let i_span_end = i_span_begin + span.content.len();

            if range.start < i_span_end && i_span_begin < range.start {
                let i_from_span = range.start - i_span_begin;
                let (before_match, remainder) = span.split_at(i_from_span);
                i_span_begin += before_match.content.len();
                spans.push(before_match);

                opt_range = Some(range);
                opt_span = Some(remainder);
            } else if range.start < i_span_end {
                let i_from_start = usize::min(i_span_end, range.end);
                let i_from_span = i_from_start - i_span_begin;
                let (span_to_style, remainder) = span.split_at(i_from_span);

                i_span_begin += span_to_style.content.len();
                spans.push(span_to_style.patch_style(style));

                if remainder.content.is_empty() {
                    opt_range = Some(range);
                } else {
                    opt_span = Some(remainder);
                }
            } else {
                opt_range = Some(range);
                spans.push(span);
                i_span_begin = i_span_end;
            }
        }

        opt_span
            .into_iter()
            .chain(iter_span)
            .for_each(|span| spans.push(span));

        Line {
            spans,
            alignment: self.alignment,
            ..Default::default()
        }
    }
}

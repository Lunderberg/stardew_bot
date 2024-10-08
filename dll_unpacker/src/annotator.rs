use std::{fmt::Display, ops::Range};

use memory_reader::{Pointer, UnpackedValue};

pub trait Annotator {
    fn range(&mut self, range: Range<Pointer>) -> &mut impl Annotation;

    fn value<T: Display>(
        &mut self,
        value: UnpackedValue<T>,
    ) -> &mut impl Annotation {
        self.range(value.loc()).value(value.value())
    }

    fn opt_value<T: Display>(
        &mut self,
        value: UnpackedValue<Option<T>>,
    ) -> &mut impl Annotation {
        let annotation = self.range(value.loc());
        match value.value() {
            Some(value) => annotation.value(value),
            None => annotation.value("(null)"),
        }
    }

    fn group(
        &mut self,
        range: impl Into<Range<Pointer>>,
    ) -> &mut impl Annotation {
        self.range(range.into()).disable_highlight()
    }
}

pub trait Annotation: Sized {
    fn name(&mut self, name: impl Into<String>) -> &mut Self;

    fn current_value(&self) -> &str;

    fn value(&mut self, value: impl Display) -> &mut Self;

    fn disable_highlight(&mut self) -> &mut Self;

    fn opt_value(&mut self, opt_value: Option<impl Display>) -> &mut Self {
        if let Some(value) = opt_value {
            self.value(value)
        } else {
            self.value("None")
        }
    }

    fn append_value(&mut self, value: impl Display) -> &mut Self {
        // Not the most efficient if done in a loop, but convenient
        // enough for now.
        let value = format!("{}\n{}", self.current_value(), value);
        self.value(value)
    }
}

impl<T> Annotator for Vec<T>
where
    T: Annotation,
    T: From<Range<Pointer>>,
{
    fn range(&mut self, range: Range<Pointer>) -> &mut impl Annotation {
        self.push(range.into());
        self.last_mut().unwrap()
    }
}

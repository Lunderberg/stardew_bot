use std::ops::Range;

use memory_reader::Pointer;

pub struct Annotation {
    pub range: Range<Pointer>,
    pub name: String,
    pub value: String,

    /// If true, highlight this annotation in the MemoryTable.  If
    /// false, show the annotation in the DetailView, but do not
    /// highlight.  This allows hierarchical annotations while only
    /// the inner-most annotation is highlighted.
    pub highlight_range: bool,
}

impl Annotation {
    pub fn sort_key(&self) -> impl Ord {
        (self.highlight_range, self.range.end, self.range.start)
    }
}

impl dll_unpacker::Annotation for Annotation {
    fn name(&mut self, name: impl Into<String>) -> &mut Self {
        self.name = name.into();
        self
    }

    fn current_value(&self) -> &str {
        self.value.as_str()
    }

    fn value(&mut self, value: impl std::fmt::Display) -> &mut Self {
        self.value = format!("{value}");
        self
    }

    fn disable_highlight(&mut self) -> &mut Self {
        self.highlight_range = false;
        self
    }
}

impl From<Range<Pointer>> for Annotation {
    fn from(range: Range<Pointer>) -> Self {
        Annotation {
            range,
            name: String::default(),
            value: String::default(),
            highlight_range: true,
        }
    }
}

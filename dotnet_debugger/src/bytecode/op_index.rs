use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct OpIndex(pub(crate) usize);

impl OpIndex {
    pub(crate) fn new(index: usize) -> Self {
        Self(index)
    }
}

impl Display for OpIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]", self.0)
    }
}

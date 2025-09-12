use crate::SymbolicGraph;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct OpIndex(pub usize);

impl OpIndex {
    pub fn new(index: usize) -> Self {
        Self(index)
    }

    pub fn pprint(self, graph: &SymbolicGraph) -> IndexPrinter<'_> {
        IndexPrinter::new(self, graph)
    }
}

#[derive(Clone, Copy)]
pub struct IndexPrinter<'a> {
    graph: &'a SymbolicGraph,
    index: OpIndex,
    requires_name_prefix: bool,
}

impl std::fmt::Display for OpIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]", self.0)
    }
}

impl<'a> IndexPrinter<'a> {
    fn new(index: OpIndex, graph: &'a SymbolicGraph) -> Self {
        Self {
            graph,
            index,
            requires_name_prefix: true,
        }
    }

    pub fn requires_name_prefix(self, requires_name_prefix: bool) -> Self {
        Self {
            requires_name_prefix,
            ..self
        }
    }
}

impl std::fmt::Display for IndexPrinter<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let index = self.index.0;
        if let Some(name) = self.graph[self.index].name.as_ref() {
            if self.requires_name_prefix {
                write!(f, "_{index}_{name}")
            } else {
                write!(f, "{name}")
            }
        } else {
            write!(f, "_{index}")
        }
    }
}

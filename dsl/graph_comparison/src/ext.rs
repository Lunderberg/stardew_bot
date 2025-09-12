use dsl_ir::SymbolicGraph;

use crate::GraphComparison;

pub trait GraphComparisonExt {
    fn graph_comparison<'a>(&'a self, rhs: &'a Self) -> GraphComparison<'a>;
}
impl GraphComparisonExt for SymbolicGraph {
    fn graph_comparison<'a>(&'a self, rhs: &'a Self) -> GraphComparison<'a> {
        GraphComparison {
            lhs: self,
            rhs,
            order_dependent: false,
            compare_names: false,
        }
    }
}

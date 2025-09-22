use crate::{ExprKind, OpIndex, SymbolicGraph, SymbolicValue};

impl ExprKind {
    pub fn iter_input_values(
        &self,
    ) -> impl Iterator<Item = SymbolicValue> + '_ {
        let (static_inputs, dynamic_inputs_a, dynamic_inputs_b): (
            _,
            Option<&[SymbolicValue]>,
            Option<_>,
        ) = match self {
            // No upstream inputs
            ExprKind::None
            | ExprKind::NativeFunction(_)
            | ExprKind::FunctionArg(_)
            | ExprKind::StaticField(_) => ([None, None, None], None, None),

            // Dynamic number of upstream inputs
            ExprKind::Function { params, output } => {
                ([Some(*output), None, None], Some(params), None)
            }
            ExprKind::FunctionCall { func, args } => {
                ([Some(*func), None, None], Some(args), None)
            }
            ExprKind::Tuple(items) => ([None, None, None], Some(items), None),
            ExprKind::IndexAccess { obj, indices } => {
                ([Some(*obj), None, None], Some(indices), None)
            }
            ExprKind::ReadBytes(regions) => (
                [None, None, None],
                None,
                Some(
                    regions
                        .iter()
                        .flat_map(|region| [region.ptr, region.num_bytes]),
                ),
            ),

            // One upstream input
            ExprKind::Range { extent: value }
            | ExprKind::First { iterator: value }
            | ExprKind::Collect { iterator: value }
            | ExprKind::FieldAccess { obj: value, .. }
            | ExprKind::SymbolicDowncast { obj: value, .. }
            | ExprKind::NumArrayElements { array: value }
            | ExprKind::PointerCast { ptr: value, .. }
            | ExprKind::IsSome(value)
            | ExprKind::Not { arg: value }
            | ExprKind::PrimCast { value, .. }
            | ExprKind::IsSubclassOf {
                method_table_ptr: value,
                ..
            }
            | ExprKind::ReadPrim { ptr: value, .. }
            | ExprKind::ReadString { ptr: value } => {
                ([Some(*value), None, None], None, None)
            }

            // Two upstreams inputs
            ExprKind::Map {
                iterator,
                map: func,
            }
            | ExprKind::Filter {
                iterator,
                filter: func,
            }
            | ExprKind::Find {
                iterator,
                condition: func,
            }
            | ExprKind::FindMap {
                iterator,
                condition: func,
            } => ([Some(*iterator), Some(*func), None], None, None),
            &ExprKind::Chain(iter_a, iter_b) => {
                ([Some(iter_a), Some(iter_b), None], None, None)
            }
            &ExprKind::ArrayExtent { array, dim } => {
                ([Some(array), Some(dim), None], None, None)
            }
            &ExprKind::CastBytes { bytes, offset, .. } => {
                ([Some(bytes), Some(offset), None], None, None)
            }

            // Binary operators
            &ExprKind::And { lhs, rhs }
            | &ExprKind::Or { lhs, rhs }
            | &ExprKind::Equal { lhs, rhs }
            | &ExprKind::NotEqual { lhs, rhs }
            | &ExprKind::LessThan { lhs, rhs }
            | &ExprKind::GreaterThan { lhs, rhs }
            | &ExprKind::LessThanOrEqual { lhs, rhs }
            | &ExprKind::GreaterThanOrEqual { lhs, rhs }
            | &ExprKind::Add { lhs, rhs }
            | &ExprKind::Sub { lhs, rhs }
            | &ExprKind::Mul { lhs, rhs }
            | &ExprKind::Div { lhs, rhs }
            | &ExprKind::Mod { lhs, rhs } => {
                ([Some(lhs), Some(rhs), None], None, None)
            }

            // Three upstreams inputs
            &ExprKind::Reduce {
                initial,
                iterator,
                reduction,
            } => ([Some(initial), Some(iterator), Some(reduction)], None, None),
            &ExprKind::SimpleReduce {
                initial,
                extent,
                reduction,
            } => ([Some(initial), Some(extent), Some(reduction)], None, None),
            &ExprKind::IfElse {
                condition,
                if_branch,
                else_branch,
            } => (
                [Some(condition), Some(if_branch), Some(else_branch)],
                None,
                None,
            ),
        };

        let iter_static = static_inputs.into_iter().flatten();
        let iter_dynamic_a = dynamic_inputs_a.into_iter().flatten().cloned();
        let iter_dynamic_b = dynamic_inputs_b.into_iter().flatten();

        std::iter::empty()
            .chain(iter_static)
            .chain(iter_dynamic_a)
            .chain(iter_dynamic_b)
    }

    pub fn iter_input_nodes(&self) -> impl Iterator<Item = OpIndex> + '_ {
        self.iter_input_values()
            .filter_map(|value| value.as_op_index())
    }
}

impl SymbolicGraph {
    /// Determine which expressions are used by some expression.
    ///
    /// Given a set of initial expressions, returns a boolean vector
    /// of size `self.num_operations()`.  If one or more of the
    /// initial expressions depends on an operation, the boolean
    /// vector will contain `true` for that element.  Otherwise, the
    /// boolean vector will contain `false`.
    pub fn reachable(
        &self,
        initial: impl IntoIterator<Item = OpIndex>,
    ) -> Vec<bool> {
        let mut to_visit: Vec<_> = initial.into_iter().collect();

        let mut reachable = vec![false; self.num_operations()];
        for index in &to_visit {
            reachable[index.0] = true;
        }

        while let Some(visiting) = to_visit.pop() {
            self[visiting].iter_input_nodes().for_each(|upstream| {
                if !reachable[upstream.0] {
                    reachable[upstream.0] = true;
                    to_visit.push(upstream);
                }
            });
        }

        reachable
    }
}

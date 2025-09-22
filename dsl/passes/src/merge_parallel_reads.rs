use itertools::Itertools as _;
use std::collections::{HashMap, HashSet};

use dsl_ir::{
    ByteRegion, ExprKind, OpIndex, RuntimePrimType, SymbolicGraph,
    SymbolicValue,
};
use dsl_rewrite_utils::{GraphRewrite, SymbolicGraphSubstitute as _};

use crate::Error;

pub struct MergeParallelReads;

struct PrimCast {
    op: OpIndex,
    offset: SymbolicValue,
    prim_type: RuntimePrimType,
}
struct GroupedRead {
    op: OpIndex,
    regions: Vec<ByteRegion>,
    casts: Vec<PrimCast>,
}

impl GraphRewrite for MergeParallelReads {
    type Error = Error;

    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
        _name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, Error> {
        let ExprKind::Function { params, output } = expr else {
            return Ok(None);
        };

        let subgraph =
            graph.collect_subgraph(params.iter().cloned(), Some(*output));

        let mut previous_groups = HashSet::<OpIndex>::new();

        let mut valid_in_group: HashSet<OpIndex> =
            params.iter().filter_map(|p| p.as_op_index()).collect();
        let mut reads = HashMap::<OpIndex, GroupedRead>::new();

        loop {
            for op in subgraph.iter().cloned() {
                let is_valid = graph[op]
                    .kind
                    .iter_input_nodes()
                    .all(|input_node| valid_in_group.contains(&input_node));

                match &graph[op].kind {
                    ExprKind::ReadBytes(regions)
                        if is_valid && !previous_groups.contains(&op) =>
                    {
                        reads.insert(
                            op,
                            GroupedRead {
                                op,
                                regions: regions.clone(),
                                casts: Vec::new(),
                            },
                        );
                    }

                    &ExprKind::CastBytes {
                        bytes: SymbolicValue::Result(bytes),
                        offset,
                        prim_type,
                    } if reads.contains_key(&bytes) => {
                        reads
                            .get_mut(&bytes)
                            .expect("Protected by .contains_key()")
                            .casts
                            .push(PrimCast {
                                op,
                                offset,
                                prim_type,
                            });
                    }

                    _ if is_valid => {
                        valid_in_group.insert(op);
                    }
                    _ => {}
                }
            }

            if reads.is_empty() {
                break;
            }
            if reads.len() == 1 {
                reads.drain().for_each(|(op_index, _)| {
                    previous_groups.insert(op_index);
                    valid_in_group.insert(op_index);
                });
                continue;
            }

            let reads: Vec<_> =
                reads.into_values().sorted_by_key(|read| read.op).collect();

            let new_read = graph.read_byte_regions(
                reads.iter().flat_map(|read| read.regions.iter()).cloned(),
            );

            let mut cumulative_offset: SymbolicValue = 0usize.into();
            let mut replacements = HashMap::<OpIndex, SymbolicValue>::new();

            for read in reads {
                for cast in read.casts {
                    let new_offset = graph.add(cumulative_offset, cast.offset);
                    let new_cast =
                        graph.cast_bytes(new_read, new_offset, cast.prim_type);
                    if let Some(name) = &graph[cast.op].name {
                        let name = name.to_string();
                        graph
                            .name(new_cast, name)
                            .expect("Previous name must be valid");
                    }
                    replacements.insert(cast.op, new_cast);
                }
                for region in read.regions {
                    cumulative_offset =
                        graph.add(cumulative_offset, region.num_bytes);
                }
            }

            let new_func =
                graph.substitute(replacements, *output)?.map(|new_output| {
                    graph.function_def(params.clone(), new_output)
                });

            return Ok(new_func);
        }

        Ok(None)
    }
}

#[cfg(test)]
mod test {
    use dsl_graph_comparison::GraphComparisonExt as _;
    use dsl_rewrite_utils::SymbolicGraphRewrite as _;

    use super::*;

    fn check_before_expected(
        before: &str,
        expected: &str,
    ) -> Result<(), Error> {
        let before = {
            let mut graph = SymbolicGraph::new();
            graph.parse(before)?;
            graph
        };

        let expected = {
            let mut graph = SymbolicGraph::new();
            graph.parse(expected)?;
            graph
        };

        println!(
            "----------- Before -------------\n{}",
            before
                .printer()
                .expand_all_expressions()
                .number_all_expressions()
        );

        let rewriter = MergeParallelReads
            .then(crate::ConstantFold)
            .apply_recursively();

        let after = before.rewrite(rewriter)?;

        println!(
            "----------- After -------------\n{}",
            after
                .printer()
                .expand_all_expressions()
                .number_all_expressions()
        );

        println!(
            "----------- Expected -------------\n{}",
            expected
                .printer()
                .expand_all_expressions()
                .number_all_expressions()
        );

        assert!(after.graph_comparison(&expected).apply());

        Ok(())
    }

    #[test]
    fn merge_two_reads() -> Result<(), Error> {
        check_before_expected(
            stringify! {
                pub fn main(a_ptr: Ptr, b_ptr: Ptr) {
                    let a_bytes = a_ptr.read_bytes(8);
                    let a_value = a_bytes.cast_bytes::<usize>(0);

                    let b_bytes = b_ptr.read_bytes(8);
                    let b_value = b_bytes.cast_bytes::<usize>(0);

                    a_value + b_value
                }
            },
            stringify! {
                pub fn main(a_ptr: Ptr, b_ptr: Ptr) {
                    let bytes = read_bytes(a_ptr, 8, b_ptr, 8);
                    let a_value = bytes.cast_bytes::<usize>(0);
                    let b_value = bytes.cast_bytes::<usize>(8);

                    a_value + b_value
                }
            },
        )
    }

    #[test]
    fn merge_three_reads() -> Result<(), Error> {
        check_before_expected(
            stringify! {
                pub fn main(a_ptr: Ptr, b_ptr: Ptr, c_ptr: Ptr) {
                    let a_bytes = a_ptr.read_bytes(8);
                    let a_value = a_bytes.cast_bytes::<usize>(0);

                    let b_bytes = b_ptr.read_bytes(8);
                    let b_value = b_bytes.cast_bytes::<usize>(0);

                    let c_bytes = c_ptr.read_bytes(8);
                    let c_value = c_bytes.cast_bytes::<usize>(0);

                    a_value + b_value + c_value
                }
            },
            stringify! {
                pub fn main(a_ptr: Ptr, b_ptr: Ptr, c_ptr: Ptr) {
                    let bytes = read_bytes(a_ptr, 8, b_ptr, 8, c_ptr, 8);
                    let a_value = bytes.cast_bytes::<usize>(0);
                    let b_value = bytes.cast_bytes::<usize>(8);
                    let c_value = bytes.cast_bytes::<usize>(16);

                    a_value + b_value + c_value
                }
            },
        )
    }

    #[test]
    fn merge_two_reads_with_already_merged_read() -> Result<(), Error> {
        check_before_expected(
            stringify! {
                pub fn main(a_ptr: Ptr, b_ptr: Ptr, c_ptr: Ptr) {
                    let ab_bytes = read_bytes(a_ptr, 8, b_ptr, 8);
                    let a_value = ab_bytes.cast_bytes::<usize>(0);
                    let b_value = ab_bytes.cast_bytes::<usize>(8);

                    let c_bytes = c_ptr.read_bytes(8);
                    let c_value = c_bytes.cast_bytes::<usize>(0);

                    a_value + b_value + c_value
                }
            },
            stringify! {
                pub fn main(a_ptr: Ptr, b_ptr: Ptr, c_ptr: Ptr) {
                    let bytes = read_bytes(a_ptr, 8, b_ptr, 8, c_ptr, 8);
                    let a_value = bytes.cast_bytes::<usize>(0);
                    let b_value = bytes.cast_bytes::<usize>(8);
                    let c_value = bytes.cast_bytes::<usize>(16);

                    a_value + b_value + c_value
                }
            },
        )
    }

    #[test]
    fn merge_sequential_steps() -> Result<(), Error> {
        check_before_expected(
            stringify! {
                pub fn main(a: Ptr, b: Ptr) {
                    let a = a.read_bytes(8).cast_bytes::<Ptr>(0);
                    let a = a + 16;
                    let a = a.read_bytes(8).cast_bytes::<usize>(0);

                    let b = b.read_bytes(8).cast_bytes::<Ptr>(0);
                    let b = b + 16;
                    let b = b.read_bytes(8).cast_bytes::<usize>(0);

                    a + b
                }
            },
            stringify! {
                pub fn main(a: Ptr, b: Ptr) {
                    let bytes = read_bytes(a, 8, b, 8);
                    let a = bytes.cast_bytes::<Ptr>(0);
                    let b = bytes.cast_bytes::<Ptr>(8);

                    let a = a + 16;
                    let b = b + 16;

                    let bytes = read_bytes(a, 8, b, 8);
                    let a = bytes.cast_bytes::<usize>(0);
                    let b = bytes.cast_bytes::<usize>(8);

                    a + b
                }
            },
        )
    }
}

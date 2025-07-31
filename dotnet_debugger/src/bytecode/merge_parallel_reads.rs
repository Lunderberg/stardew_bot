use std::collections::{HashMap, HashSet};

use itertools::Itertools as _;

use crate::{Error, RuntimePrimType};

use super::{
    expr::ByteRegion, ExprKind, GraphRewrite, OpIndex, SymbolicGraph,
    SymbolicValue,
};

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

            if reads.len() == 0 {
                break;
            }
            if reads.len() == 1 {
                reads.drain().for_each(|(op_index, _)| {
                    previous_groups.insert(op_index);
                    valid_in_group.insert(op_index);
                });
                continue;
            }

            let reads: Vec<_> = reads
                .into_iter()
                .map(|(_, read)| read)
                .sorted_by_key(|read| read.op)
                .collect();

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

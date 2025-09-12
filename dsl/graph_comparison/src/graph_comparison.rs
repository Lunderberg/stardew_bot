use std::collections::HashMap;

use dsl_ir::{ExprKind, OpIndex, StaticField, SymbolicGraph, SymbolicValue};

pub struct GraphComparison<'a> {
    pub(crate) lhs: &'a SymbolicGraph,
    pub(crate) rhs: &'a SymbolicGraph,
    pub(crate) order_dependent: bool,
    pub(crate) compare_names: bool,
}

impl<'a> GraphComparison<'a> {
    pub fn order_dependent(self, order_dependent: bool) -> Self {
        Self {
            order_dependent,
            ..self
        }
    }

    pub fn compare_names(self, compare_names: bool) -> Self {
        Self {
            compare_names,
            ..self
        }
    }

    pub fn apply(&self) -> bool {
        if self.order_dependent {
            self.order_dependent_comparison()
        } else {
            self.order_independent_comparison()
        }
    }

    fn order_dependent_comparison(&self) -> bool {
        let lhs = self.lhs;
        let rhs = self.rhs;
        lhs.num_operations() == rhs.num_operations()
            && lhs.num_extern_funcs() == rhs.num_extern_funcs()
            && lhs.iter_ops().zip(rhs.iter_ops()).all(|((_, a), (_, b))| {
                a.kind == b.kind && (!self.compare_names || a.name == b.name)
            })
            && lhs
                .iter_extern_funcs()
                .zip(rhs.iter_extern_funcs())
                .all(|(a, b)| a == b)
    }

    fn order_independent_comparison(&self) -> bool {
        let lhs = self.lhs;
        let rhs = self.rhs;

        if lhs.num_extern_funcs() != rhs.num_extern_funcs() {
            return false;
        }

        let mut lhs_index_to_rhs = HashMap::<OpIndex, OpIndex>::new();
        let mut rhs_index_to_lhs = HashMap::<OpIndex, OpIndex>::new();

        let mut to_visit = Vec::<(OpIndex, OpIndex)>::new();

        macro_rules! equivalent_value {
            ($lhs:expr,$rhs:expr) => {{
                let lhs: &SymbolicValue = $lhs;
                let rhs: &SymbolicValue = $rhs;

                match (*lhs, *rhs) {
                    (
                        SymbolicValue::Result(lhs_index),
                        SymbolicValue::Result(rhs_index),
                    ) => {
                        if let Some(prev_rhs) = lhs_index_to_rhs.get(&lhs_index)
                        {
                            rhs_index == *prev_rhs
                        } else if let Some(prev_lhs) =
                            rhs_index_to_lhs.get(&rhs_index)
                        {
                            lhs_index == *prev_lhs
                        } else {
                            to_visit.push((lhs_index, rhs_index));
                            lhs_index_to_rhs.insert(lhs_index, rhs_index);
                            rhs_index_to_lhs.insert(rhs_index, lhs_index);
                            true
                        }
                    }

                    (other_lhs, other_rhs) => other_lhs == other_rhs,
                }
            }};
        }

        for (lhs_extern_func, rhs_extern_func) in
            lhs.iter_extern_funcs().zip(rhs.iter_extern_funcs())
        {
            let lhs_extern_func: SymbolicValue = lhs_extern_func.into();
            let rhs_extern_func: SymbolicValue = rhs_extern_func.into();
            if !equivalent_value!(&lhs_extern_func, &rhs_extern_func) {
                return false;
            }
        }

        while let Some((lhs_index, rhs_index)) = to_visit.pop() {
            if self.compare_names && lhs[lhs_index].name != rhs[rhs_index].name
            {
                return false;
            }

            let lhs_kind = &lhs[lhs_index].kind;
            let rhs_kind = &rhs[rhs_index].kind;

            macro_rules! handle_binary_op {
                ($variant:ident, $lhs:expr, $rhs:expr) => {{
                    let lhs_lhs = $lhs;
                    let lhs_rhs = $rhs;
                    match rhs_kind {
                        ExprKind::$variant {
                            lhs: rhs_lhs,
                            rhs: rhs_rhs,
                        } => {
                            equivalent_value!(lhs_lhs, rhs_lhs)
                                && equivalent_value!(lhs_rhs, rhs_rhs)
                        }
                        _ => false,
                    }
                }};
            }

            let is_match = match lhs_kind {
                ExprKind::None => matches!(rhs_kind, ExprKind::None),

                ExprKind::Function {
                    params: lhs_params,
                    output: lhs_output,
                } => match rhs_kind {
                    ExprKind::Function {
                        params: rhs_params,
                        output: rhs_output,
                    } => {
                        lhs_params.len() == rhs_params.len()
                            && lhs_params.iter().zip(rhs_params).all(
                                |(lhs_param, rhs_param)| {
                                    equivalent_value!(lhs_param, rhs_param)
                                },
                            )
                            && equivalent_value!(lhs_output, rhs_output)
                    }
                    _ => false,
                },
                ExprKind::FunctionArg(lhs_ty) => match rhs_kind {
                    ExprKind::FunctionArg(rhs_ty) => lhs_ty == rhs_ty,
                    _ => false,
                },
                ExprKind::FunctionCall {
                    func: lhs_func,
                    args: lhs_args,
                } => match rhs_kind {
                    ExprKind::FunctionCall {
                        func: rhs_func,
                        args: rhs_args,
                    } => {
                        equivalent_value!(lhs_func, rhs_func)
                            && lhs_args.len() == rhs_args.len()
                            && lhs_args.iter().zip(rhs_args).all(
                                |(lhs_arg, rhs_arg)| {
                                    equivalent_value!(lhs_arg, rhs_arg)
                                },
                            )
                    }
                    _ => false,
                },
                ExprKind::Range { extent: lhs_extent } => match rhs_kind {
                    ExprKind::Range { extent: rhs_extent } => {
                        equivalent_value!(lhs_extent, rhs_extent)
                    }
                    _ => false,
                },
                ExprKind::Map {
                    iterator: lhs_iterator,
                    map: lhs_map,
                } => match rhs_kind {
                    ExprKind::Map {
                        iterator: rhs_iterator,
                        map: rhs_map,
                    } => {
                        equivalent_value!(lhs_iterator, rhs_iterator)
                            && equivalent_value!(lhs_map, rhs_map)
                    }
                    _ => false,
                },
                ExprKind::Filter {
                    iterator: lhs_iterator,
                    filter: lhs_filter,
                } => match rhs_kind {
                    ExprKind::Filter {
                        iterator: rhs_iterator,
                        filter: rhs_filter,
                    } => {
                        equivalent_value!(lhs_iterator, rhs_iterator)
                            && equivalent_value!(lhs_filter, rhs_filter)
                    }
                    _ => false,
                },
                ExprKind::Find {
                    iterator: lhs_iterator,
                    condition: rhs_condition,
                } => match rhs_kind {
                    ExprKind::Find {
                        iterator: rhs_iterator,
                        condition: lhs_condition,
                    } => {
                        equivalent_value!(lhs_iterator, rhs_iterator)
                            && equivalent_value!(lhs_condition, rhs_condition)
                    }
                    _ => false,
                },
                ExprKind::FindMap {
                    iterator: lhs_iterator,
                    condition: lhs_condition,
                } => match rhs_kind {
                    ExprKind::FindMap {
                        iterator: rhs_iterator,
                        condition: rhs_condition,
                    } => {
                        equivalent_value!(lhs_iterator, rhs_iterator)
                            && equivalent_value!(lhs_condition, rhs_condition)
                    }
                    _ => false,
                },
                ExprKind::Chain(lhs_iter_a, lhs_iter_b) => match rhs_kind {
                    ExprKind::Chain(rhs_iter_a, rhs_iter_b) => {
                        equivalent_value!(lhs_iter_a, rhs_iter_a)
                            && equivalent_value!(lhs_iter_b, rhs_iter_b)
                    }
                    _ => false,
                },

                ExprKind::First {
                    iterator: lhs_iterator,
                } => match rhs_kind {
                    ExprKind::First {
                        iterator: rhs_iterator,
                    } => {
                        equivalent_value!(lhs_iterator, rhs_iterator)
                    }
                    _ => false,
                },

                ExprKind::Collect {
                    iterator: lhs_iterator,
                } => match rhs_kind {
                    ExprKind::Collect {
                        iterator: rhs_iterator,
                    } => {
                        equivalent_value!(lhs_iterator, rhs_iterator)
                    }
                    _ => false,
                },

                ExprKind::Reduce {
                    initial: lhs_initial,
                    iterator: lhs_iterator,
                    reduction: lhs_reduction,
                } => match rhs_kind {
                    ExprKind::Reduce {
                        initial: rhs_initial,
                        iterator: rhs_iterator,
                        reduction: rhs_reduction,
                    } => {
                        equivalent_value!(lhs_initial, rhs_initial)
                            && equivalent_value!(lhs_iterator, rhs_iterator)
                            && equivalent_value!(lhs_reduction, rhs_reduction)
                    }
                    _ => false,
                },
                ExprKind::SimpleReduce {
                    initial: lhs_initial,
                    extent: lhs_extent,
                    reduction: lhs_reduction,
                } => match rhs_kind {
                    ExprKind::SimpleReduce {
                        initial: rhs_initial,
                        extent: rhs_extent,
                        reduction: rhs_reduction,
                    } => {
                        equivalent_value!(lhs_initial, rhs_initial)
                            && equivalent_value!(lhs_extent, rhs_extent)
                            && equivalent_value!(lhs_reduction, rhs_reduction)
                    }
                    _ => false,
                },
                ExprKind::NativeFunction(lhs_func) => match rhs_kind {
                    ExprKind::NativeFunction(rhs_func) => lhs_func == rhs_func,
                    _ => false,
                },
                ExprKind::Tuple(lhs_tuple) => match rhs_kind {
                    ExprKind::Tuple(rhs_tuple) => {
                        lhs_tuple.len() == rhs_tuple.len()
                            && lhs_tuple.iter().zip(rhs_tuple).all(
                                |(lhs_element, rhs_element)| {
                                    equivalent_value!(lhs_element, rhs_element)
                                },
                            )
                    }
                    _ => false,
                },
                ExprKind::StaticField(StaticField {
                    class: lhs_class,
                    field_name: lhs_field,
                }) => match rhs_kind {
                    ExprKind::StaticField(StaticField {
                        class: rhs_class,
                        field_name: rhs_field,
                    }) => lhs_class == rhs_class && lhs_field == rhs_field,
                    _ => false,
                },
                ExprKind::FieldAccess {
                    obj: lhs_obj,
                    field: lhs_field,
                } => match rhs_kind {
                    ExprKind::FieldAccess {
                        obj: rhs_obj,
                        field: rhs_field,
                    } => {
                        equivalent_value!(lhs_obj, rhs_obj)
                            && lhs_field == rhs_field
                    }
                    _ => false,
                },
                ExprKind::SymbolicDowncast {
                    obj: lhs_obj,
                    ty: lhs_ty,
                } => match rhs_kind {
                    ExprKind::SymbolicDowncast {
                        obj: rhs_obj,
                        ty: rhs_ty,
                    } => {
                        equivalent_value!(lhs_obj, rhs_obj) && lhs_ty == rhs_ty
                    }
                    _ => false,
                },
                ExprKind::IndexAccess {
                    obj: lhs_obj,
                    indices: lhs_indices,
                } => match rhs_kind {
                    ExprKind::IndexAccess {
                        obj: rhs_obj,
                        indices: rhs_indices,
                    } => {
                        equivalent_value!(lhs_obj, rhs_obj)
                            && lhs_indices.len() == rhs_indices.len()
                            && lhs_indices.iter().zip(rhs_indices.iter()).all(
                                |(lhs_index, rhs_index)| {
                                    equivalent_value!(lhs_index, rhs_index)
                                },
                            )
                    }
                    _ => false,
                },
                ExprKind::NumArrayElements { array: lhs_array } => {
                    match rhs_kind {
                        ExprKind::NumArrayElements { array: rhs_array } => {
                            equivalent_value!(lhs_array, rhs_array)
                        }
                        _ => false,
                    }
                }
                ExprKind::ArrayExtent {
                    array: lhs_array,
                    dim: lhs_dim,
                } => match rhs_kind {
                    ExprKind::ArrayExtent {
                        array: rhs_array,
                        dim: rhs_dim,
                    } => {
                        equivalent_value!(lhs_array, rhs_array)
                            && equivalent_value!(lhs_dim, rhs_dim)
                    }
                    _ => false,
                },
                ExprKind::PointerCast {
                    ptr: lhs_ptr,
                    ty: lhs_ty,
                } => match rhs_kind {
                    ExprKind::PointerCast {
                        ptr: rhs_ptr,
                        ty: rhs_ty,
                    } => {
                        equivalent_value!(lhs_ptr, rhs_ptr) && lhs_ty == rhs_ty
                    }
                    _ => false,
                },
                ExprKind::IsSome(lhs_value) => match rhs_kind {
                    ExprKind::IsSome(rhs_value) => {
                        equivalent_value!(lhs_value, rhs_value)
                    }
                    _ => false,
                },
                ExprKind::IfElse {
                    condition: lhs_condition,
                    if_branch: lhs_if_branch,
                    else_branch: lhs_else_branch,
                } => match rhs_kind {
                    ExprKind::IfElse {
                        condition: rhs_condition,
                        if_branch: rhs_if_branch,
                        else_branch: rhs_else_branch,
                    } => {
                        equivalent_value!(lhs_condition, rhs_condition)
                            && equivalent_value!(lhs_if_branch, rhs_if_branch)
                            && equivalent_value!(
                                lhs_else_branch,
                                rhs_else_branch
                            )
                    }
                    _ => false,
                },

                ExprKind::And { lhs, rhs } => {
                    handle_binary_op!(And, lhs, rhs)
                }
                ExprKind::Or { lhs, rhs } => {
                    handle_binary_op!(Or, lhs, rhs)
                }
                ExprKind::Not { arg: lhs_arg } => match rhs_kind {
                    ExprKind::Not { arg: rhs_arg } => {
                        equivalent_value!(lhs_arg, rhs_arg)
                    }
                    _ => false,
                },

                ExprKind::Equal { lhs, rhs } => {
                    handle_binary_op!(Equal, lhs, rhs)
                }
                ExprKind::NotEqual { lhs, rhs } => {
                    handle_binary_op!(NotEqual, lhs, rhs)
                }
                ExprKind::GreaterThan { lhs, rhs } => {
                    handle_binary_op!(GreaterThan, lhs, rhs)
                }
                ExprKind::LessThan { lhs, rhs } => {
                    handle_binary_op!(LessThan, lhs, rhs)
                }
                ExprKind::GreaterThanOrEqual { lhs, rhs } => {
                    handle_binary_op!(GreaterThanOrEqual, lhs, rhs)
                }
                ExprKind::LessThanOrEqual { lhs, rhs } => {
                    handle_binary_op!(LessThanOrEqual, lhs, rhs)
                }

                ExprKind::Add { lhs, rhs } => handle_binary_op!(Add, lhs, rhs),
                ExprKind::Sub { lhs, rhs } => handle_binary_op!(Sub, lhs, rhs),
                ExprKind::Mul { lhs, rhs } => handle_binary_op!(Mul, lhs, rhs),
                ExprKind::Div { lhs, rhs } => handle_binary_op!(Div, lhs, rhs),
                ExprKind::Mod { lhs, rhs } => handle_binary_op!(Mod, lhs, rhs),

                ExprKind::PrimCast {
                    value: lhs_value,
                    prim_type: lhs_prim_type,
                } => match rhs_kind {
                    ExprKind::PrimCast {
                        value: rhs_value,
                        prim_type: rhs_prim_type,
                    } => {
                        equivalent_value!(lhs_value, rhs_value)
                            && lhs_prim_type == rhs_prim_type
                    }
                    _ => false,
                },
                ExprKind::PhysicalDowncast {
                    obj: lhs_obj,
                    ty: lhs_ty,
                } => match rhs_kind {
                    ExprKind::PhysicalDowncast {
                        obj: rhs_obj,
                        ty: rhs_ty,
                    } => {
                        equivalent_value!(lhs_obj, rhs_obj) && lhs_ty == rhs_ty
                    }
                    _ => false,
                },
                ExprKind::IsSubclassOf {
                    method_table_ptr: lhs_method_table_ptr,
                    ty: lhs_ty,
                } => match rhs_kind {
                    ExprKind::IsSubclassOf {
                        method_table_ptr: rhs_method_table_ptr,
                        ty: rhs_ty,
                    } => {
                        equivalent_value!(
                            lhs_method_table_ptr,
                            rhs_method_table_ptr
                        ) && lhs_ty == rhs_ty
                    }
                    _ => false,
                },
                ExprKind::ReadPrim {
                    ptr: lhs_ptr,
                    prim_type: lhs_prim_type,
                } => match rhs_kind {
                    ExprKind::ReadPrim {
                        ptr: rhs_ptr,
                        prim_type: rhs_prim_type,
                    } => {
                        equivalent_value!(lhs_ptr, rhs_ptr)
                            && lhs_prim_type == rhs_prim_type
                    }
                    _ => false,
                },
                ExprKind::ReadBytes(lhs_regions) => match rhs_kind {
                    ExprKind::ReadBytes(rhs_regions) => {
                        lhs_regions.len() == rhs_regions.len()
                            && lhs_regions.iter().zip(rhs_regions.iter()).all(
                                |(lhs_region, rhs_region)| {
                                    equivalent_value!(
                                        &lhs_region.ptr,
                                        &rhs_region.ptr
                                    ) && equivalent_value!(
                                        &lhs_region.num_bytes,
                                        &rhs_region.num_bytes
                                    )
                                },
                            )
                    }
                    _ => false,
                },
                ExprKind::CastBytes {
                    bytes: lhs_bytes,
                    offset: lhs_offset,
                    prim_type: lhs_prim_type,
                } => match rhs_kind {
                    ExprKind::CastBytes {
                        bytes: rhs_bytes,
                        offset: rhs_offset,
                        prim_type: rhs_prim_type,
                    } => {
                        equivalent_value!(lhs_bytes, rhs_bytes)
                            && equivalent_value!(lhs_offset, rhs_offset)
                            && lhs_prim_type == rhs_prim_type
                    }
                    _ => false,
                },
                ExprKind::ReadString { ptr: lhs_ptr } => match rhs_kind {
                    ExprKind::ReadString { ptr: rhs_ptr } => {
                        equivalent_value!(lhs_ptr, rhs_ptr)
                    }
                    _ => false,
                },
            };

            if !is_match {
                return false;
            }
        }

        true
    }
}

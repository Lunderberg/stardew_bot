use std::collections::{BTreeMap, HashMap};
use std::fmt::Debug;

use dsl_ir::OpIndex;
use dsl_vm::StackIndex;

use crate::Error;

#[derive(Debug, Clone, Default)]
pub struct IndexTracking {
    /// The next index to allocate, if there are no dead indices that
    /// can be re-used.
    next_free_index: usize,

    /// Indices that used to contain a value, but the value is no
    /// longer required.  These indices may be re-used.
    ///
    /// The `OpIndex` stored in the map is the most recent value
    /// stored at this location, used for error messages.
    dead_indices: BTreeMap<StackIndex, Option<OpIndex>>,

    expr_to_reserved_location: HashMap<OpIndex, StackIndex>,

    current_location: HashMap<OpIndex, StackIndex>,
    num_aliasing: HashMap<StackIndex, usize>,

    previously_consumed: HashMap<OpIndex, OpIndex>,
}

pub(crate) enum AllocIndexDebug {
    NewIndex,
    PreviouslyUsedForUnknown,
    PreviouslyUsedFor(OpIndex),
}

pub(crate) enum GetOutputDebug {
    FromReserved,
    FromAlloc(AllocIndexDebug),
}

impl IndexTracking {
    pub fn assert_empty(&self) {
        assert!(self.current_location.is_empty());
    }

    pub fn alloc_index(&mut self) -> (StackIndex, AllocIndexDebug) {
        if let Some((index, opt_prev_expr)) = self.dead_indices.pop_first() {
            (
                index,
                opt_prev_expr
                    .map(|prev_expr| {
                        AllocIndexDebug::PreviouslyUsedFor(prev_expr)
                    })
                    .unwrap_or(AllocIndexDebug::PreviouslyUsedForUnknown),
            )
        } else {
            let index = StackIndex(self.next_free_index);
            self.next_free_index += 1;
            (index, AllocIndexDebug::NewIndex)
        }
    }

    pub fn free_index(&mut self, stack_index: StackIndex) {
        self.dead_indices.insert(stack_index, None);
    }

    pub fn get_output_index(
        &mut self,
        op_index: OpIndex,
    ) -> (StackIndex, GetOutputDebug) {
        if let Some(&index) = self.expr_to_reserved_location.get(&op_index) {
            (index, GetOutputDebug::FromReserved)
        } else {
            let (index, debug) = self.alloc_index();
            (index, GetOutputDebug::FromAlloc(debug))
        }
    }

    pub fn expr_to_location(
        &self,
        op_index: OpIndex,
    ) -> Result<Option<StackIndex>, Error> {
        if self.previously_consumed.contains_key(&op_index) {
            return Err(Error::AttemptedUseOfConsumedValue);
        }

        Ok(self.current_location.get(&op_index).cloned())
    }

    pub fn expr_to_reserved_location(
        &self,
        op_index: OpIndex,
    ) -> Option<StackIndex> {
        self.expr_to_reserved_location.get(&op_index).cloned()
    }

    pub fn reserve_index(
        &mut self,
        op_index: OpIndex,
        stack_index: StackIndex,
    ) {
        {
            let previous_reserved_location =
                self.expr_to_reserved_location.get(&op_index);
            assert!(
                previous_reserved_location.is_none(),
                "Attempted to reserve {stack_index} to store {op_index}, \
                 but {} is already reserved for {op_index}",
                previous_reserved_location.unwrap(),
            );
        }

        self.expr_to_reserved_location.insert(op_index, stack_index);
        self.dead_indices.remove(&stack_index);
        *self.num_aliasing.entry(stack_index).or_insert(0) += 1;
    }

    pub fn release_reservation(&mut self, op_index: OpIndex) -> StackIndex {
        let Some(stack_index) =
            self.expr_to_reserved_location.remove(&op_index)
        else {
            unreachable!(
                "Internal error: \
                 Attempted to release reservation for {op_index}, \
                 but no such reservation existed."
            )
        };

        assert!(
            self.current_location.contains_key(&op_index),
            "Releasing reservation of {stack_index} for {op_index}, \
             but the reservation had never been used."
        );

        let Some(num_aliasing) = self.num_aliasing.get_mut(&stack_index) else {
            panic!(
                "Internal inconsistency: \
                 When releasing reservation of {stack_index} for {op_index}, \
                 num_aliasing map had no entry for {stack_index}."
            );
        };
        assert!(
            *num_aliasing >= 2,
            "Internal inconsistency: \
                 When releasing reservation of {stack_index} for {op_index}, \
                 the alias count should be at least two.  \
                 (One for the reservation, \
                 and one for the value stored at the reserved index.)"
        );
        *num_aliasing -= 1;

        stack_index
    }

    pub fn define_contents(&mut self, expr: OpIndex, loc: StackIndex) {
        {
            let previous_contents = self.dead_indices.get(&loc);
            assert!(
                previous_contents.is_none(),
                "Attempted to define {loc} as containing {expr}, \
                 but {loc} is currently listed as a dead index \
                 that previously contained {}.  \
                 Before defining the contents of a location, \
                 that location must be allocated \
                 using `ExpressionTranslater::alloc_index`.",
                previous_contents
                    .unwrap()
                    .map(|prev_expr| format!("{prev_expr}"))
                    .unwrap_or_else(|| {
                        "a value that doesn't correspond to an OpIndex".into()
                    }),
            );
        }
        {
            let current_loc = self.current_location.get(&expr);
            assert!(
                current_loc.is_none(),
                "Attempted to define {loc} as containing {expr}, \
                 but {expr} is already stored in {}.",
                current_loc.unwrap(),
            );
        }

        self.current_location.insert(expr, loc);
        *self.num_aliasing.entry(loc).or_insert(0) += 1;
    }

    /// Drop any tracking of the expression, returning the location at
    /// which the expression had previously been stored.
    ///
    /// Does not return the stack index to the pool of dead indices,
    /// and does not clean up any alias tracking.
    pub fn drop_expr(&mut self, expr: OpIndex) -> Option<StackIndex> {
        self.current_location.remove(&expr)
    }

    /// Mark an expression as having been consumed in the process of
    /// producing an output expression.  For example, a native
    /// function with signature `Fn(&mut usize)` is represented in the
    /// IR as `Fn(usize) -> usize`, where the value is moved-out on
    /// the first usage.
    pub fn consume_expr(&mut self, consumed: OpIndex, consumed_by: OpIndex) {
        self.previously_consumed.insert(consumed, consumed_by);
    }

    pub fn release_expr(&mut self, expr: OpIndex) -> Option<StackIndex> {
        let Some(stack_index) = self.drop_expr(expr) else {
            return None;
        };

        let Some(num_aliasing) = self.num_aliasing.get_mut(&stack_index) else {
            panic!(
                "Internal inconsistency: \
                 When removing {expr} from {stack_index}, \
                 num_aliasing map had no entry for {stack_index}."
            );
        };

        assert!(*num_aliasing > 0);

        *num_aliasing -= 1;

        if *num_aliasing == 0 {
            // The location-to-expr map points to the current
            // expression.  The current expression owns the stack
            // location, and so the stack location can be reused.
            self.num_aliasing.remove(&stack_index);
            self.dead_indices.insert(stack_index, Some(expr));
        } else {
            // The location-to-expr map points to a different
            // expression.  The current expression is an alias,
            // and the original expression may still use the stack
            // location.
        }

        Some(stack_index)
    }

    pub fn merge_conditional_branches(&mut self, mut other: Self) {
        self.next_free_index = self.next_free_index.max(other.next_free_index);

        // After the conditional, dead indices marked as dead
        // while following either branch are considered
        // dead.
        self.dead_indices.append(&mut other.dead_indices);

        // Likewise, only values that are currently stored
        // at the end of both branches have a known
        // storage location after the conditional.
        self.current_location = other
            .current_location
            .into_iter()
            .filter(|(op_index, _)| {
                self.current_location.contains_key(op_index)
            })
            .collect();

        // A value is considered consumed if either branch consumed
        // it.
        for (consumed, consumed_by) in other.previously_consumed {
            self.previously_consumed.insert(consumed, consumed_by);
        }

        // All reservations should expire after the instruction that
        // produced them.  Both branches of a conditional should
        // conclude with the same set of reserved output locations.
        assert_eq!(
            self.expr_to_reserved_location,
            other.expr_to_reserved_location
        );
    }
}

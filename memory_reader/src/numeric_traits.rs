//! Numeric traits
//!
//! These are used instead of `num_traits`, as its corresponding
//! traits require that `RHS` and `Output` are `Self`.

pub trait CheckedAdd<RHS = Self> {
    type Output;
    fn checked_add(self, rhs: RHS) -> Option<Self::Output>;
}

pub trait CheckedSub<RHS = Self> {
    type Output;
    fn checked_sub(self, rhs: RHS) -> Option<Self::Output>;
}

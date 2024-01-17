use std::ops::Range;

use crate::Pointer;

/// Extension trait to
pub trait HexadecimalAddressPrefix {
    type Prefix;
    fn prefix(&self) -> Self::Prefix;

    fn suffix_hexadecimal_digits(&self) -> u32;
}

impl HexadecimalAddressPrefix for Range<Pointer> {
    type Prefix = Pointer;

    fn prefix(&self) -> Self::Prefix {
        let pow = (16_usize).pow(self.suffix_hexadecimal_digits());
        let suffix_mask = pow - 1;
        let prefix_mask = !suffix_mask;
        (self.start.address & prefix_mask).into()
    }

    fn suffix_hexadecimal_digits(&self) -> u32 {
        let start = self.start.address;
        let end = self.end.address;
        let first_bit_diff = (start ^ end).ilog2();
        first_bit_diff.div_ceil(4)
    }
}

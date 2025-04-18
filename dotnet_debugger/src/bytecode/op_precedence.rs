#[derive(PartialEq, Debug)]
pub(crate) enum OpPrecedence {
    MinPrecedence,
    MaybeTuple,
    TupleElement,
    BooleanAnd,
    BooleanOr,
    ComparisonOperator,
    RangeExtent,
    Addition,
    MulDiv,
    BooleanNot,
    MaxPrecedence,
}

impl PartialOrd for OpPrecedence {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use std::cmp::Ordering as Order;

        let is_less_than = |a: &Self, b: &Self| {
            match (a, b) {
                (Self::MinPrecedence, _) => true,
                (_, Self::MaxPrecedence) => true,
                (
                    Self::MaybeTuple,
                    Self::TupleElement
                    | Self::BooleanAnd
                    | Self::BooleanOr
                    | Self::ComparisonOperator
                    | Self::RangeExtent
                    | Self::Addition
                    | Self::MulDiv
                    | Self::BooleanNot,
                ) => true,
                (
                    Self::TupleElement,
                    Self::BooleanAnd
                    | Self::BooleanOr
                    | Self::ComparisonOperator
                    | Self::RangeExtent
                    | Self::Addition
                    | Self::MulDiv
                    | Self::BooleanNot,
                ) => true,

                // This is an intentional deviation from the usual
                // operator precedence rules.  Most languages have AND
                // be higher precedence than OR.  The argument is that
                // since booleans form a field of size two, the OR
                // operator is equivalent to addition, and the AND
                // operator is equivalent to multiplication.
                //
                // While booleans form a field of size two, the
                // operations on that field are not the boolean AND/OR
                // operators.
                //
                // Starting with two elements, we can fill out the
                // truth table for the addition operation.  Of the two
                // elements, we'll call the identity element under
                // addition "a", and the other element "b".
                //
                //    Addition
                //     | a | b |
                // ----+---+---+
                //   a | a | b |
                // ----+---+---+
                //   b | b |   |
                // ----+---+---+
                //
                // This gets three of the four output values.  To get
                // the fourth, we need to use another property of
                // fields: Every value has an additive inverse.  That
                // is, for every X, there must exist Y such that "X+Y"
                // produces the zero element.
                //
                //    Addition
                //     | a | b |
                // ----+---+---+
                //   a | a | b |
                // ----+---+---+
                //   b | b | a |
                // ----+---+---+
                //
                // Now, let's figure out multiplication.  We must have
                // an identity element under multiplication, and it
                // can't be the same as the identity element under
                // addition.
                //
                // Multiplication
                //     | a | b |
                // ----+---+---+
                //   a |   | a |
                // ----+---+---+
                //   b | a | b |
                // ----+---+---+
                //
                // To find the last output value, we need to use
                // distributivity:  "X*(Y+Z) == X*Y + X*Z"
                //
                //      let X = a;
                //      let Y = b;
                //      let Z = b;
                //            X*(Y+Z) == X*Y + X*Z
                //         => a*(b+b) == a*b + a*b
                //         =>     a*a == a + a
                //         =>     a*a == a
                //
                // Multiplication
                //     | a | b |
                // ----+---+---+
                //   a | a | a |
                // ----+---+---+
                //   b | a | b |
                // ----+---+---+
                //
                //
                // So far, I've been using generic values "a" and "b",
                // and have been careful not to call either value
                // "true" or "false".  Because the assignment is
                // arbitrary.
                //
                //  a: true, b: false     |   a: false, b: true
                //                        |
                //      Addition          |      Addition
                //        XOR             |       XNOR
                //       | t | f |        |       | f | t |
                //   ----+---+---+        |   ----+---+---+
                //     t | t | f |        |     f | f | t |
                //   ----+---+---+        |   ----+---+---+
                //     f | f | t |        |     t | t | f |
                //   ----+---+---+        |   ----+---+---+
                //                        |
                //   Multiplication       |   Multiplication
                //        AND             |       NAND
                //       | t | f |        |       | f | t |
                //   ----+---+---+        |   ----+---+---+
                //     t | t | t |        |     f | f | f |
                //   ----+---+---+        |   ----+---+---+
                //     f | t | f |        |     t | f | t |
                //   ----+---+---+        |   ----+---+---+
                //
                // So, by the argument that boolean values form a
                // field, and that the addition and multiplication
                // operations of that field should follow the same
                // precedence as normal addition and multiplication,
                // we find that AND should have stronger precedence
                // than XOR, and that NAND should have stronger
                // precedence than XNOR.
                //
                // But neither of these rules say anything about the
                // relative precedence of the AND and OR operations.
                // The most fundamental relationship would be de
                // Morgan's laws.
                //
                //     NOT (X AND Y) == (NOT X) OR  (NOT Y)
                //     NOT (X OR  Y) == (NOT X) AND (NOT Y)
                //
                // These are symmetrical, and so whatever precedence
                // we choose should be symmetric between the two.
                //
                // We therefore have two options: Either the AND and
                // OR operations must have the same precedence, or
                // they must be incomparable.  Having them be the same
                // precedence would be valid, but would silently be
                // interpreted differently from in other languages.
                // Therefore, the two operations are incomparable.
                // Mixed usage of AND and OR must always have
                // parentheses to specify their precedence.
                (Self::BooleanAnd, Self::BooleanOr) => false,
                (Self::BooleanOr, Self::BooleanAnd) => false,

                (
                    Self::BooleanAnd | Self::BooleanOr,
                    Self::ComparisonOperator
                    | Self::RangeExtent
                    | Self::Addition
                    | Self::MulDiv
                    | Self::BooleanNot,
                ) => true,

                (
                    Self::ComparisonOperator,
                    Self::RangeExtent | Self::MulDiv | Self::Addition,
                ) => true,

                (Self::RangeExtent, Self::MulDiv | Self::Addition) => true,

                (Self::Addition, Self::MulDiv) => true,
                _ => false,
            }
        };

        if self == other {
            Some(Order::Equal)
        } else if is_less_than(self, other) {
            Some(Order::Less)
        } else if is_less_than(other, self) {
            Some(Order::Greater)
        } else {
            None
        }
    }
}

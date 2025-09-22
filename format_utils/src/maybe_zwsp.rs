pub struct MaybeZeroWidthSpace(pub bool);

impl std::fmt::Display for MaybeZeroWidthSpace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 {
            write!(f, "\u{200B}")
        } else {
            Ok(())
        }
    }
}

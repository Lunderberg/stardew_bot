/// A utility class to indent using spaces.
#[derive(Clone, Copy)]
pub struct Indent(pub usize);

impl std::fmt::Display for Indent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{: >indent$}", "", indent = self.0)
    }
}

impl std::ops::Add<usize> for Indent {
    type Output = Indent;

    fn add(self, rhs: usize) -> Self::Output {
        Self(self.0 + rhs)
    }
}

impl std::ops::Sub<usize> for Indent {
    type Output = Indent;

    fn sub(self, rhs: usize) -> Self::Output {
        Self(self.0 - rhs)
    }
}

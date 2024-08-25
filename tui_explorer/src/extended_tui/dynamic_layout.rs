pub struct DynamicLayout {
    windows: Vec<NestedWindow>,
}

enum NestedWindow {
    /// An index into the `windows` vector.  Used for nested windows
    Window(usize),

    /// An index into the `buffers` vector.  The `buffers` vector is
    /// not owned by the `DynamicLayout`, but is provided to the
    /// layout when drawing.
    Buffer(usize),
}

impl DynamicLayout {
    pub fn new() -> Self {
        Self {
            windows: Vec::new(),
        }
    }
}

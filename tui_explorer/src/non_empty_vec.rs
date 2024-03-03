pub struct NonEmptyVec<T> {
    first: T,
    rest: Vec<T>,
}

impl<T> NonEmptyVec<T> {
    pub fn new(first: T) -> Self {
        Self {
            first,
            rest: Vec::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.rest.len() + 1
    }

    pub fn push(&mut self, item: T) {
        self.rest.push(item);
    }

    pub fn pop(&mut self) -> Option<T> {
        self.rest.pop()
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &T> + '_ {
        std::iter::once(&self.first).chain(&self.rest)
    }

    pub fn first(&self) -> &T {
        &self.first
    }

    pub fn first_mut(&mut self) -> &mut T {
        &mut self.first
    }

    pub fn last(&self) -> &T {
        self.rest.last().unwrap_or(&self.first)
    }

    pub fn last_mut(&mut self) -> &mut T {
        self.rest.last_mut().unwrap_or(&mut self.first)
    }
}

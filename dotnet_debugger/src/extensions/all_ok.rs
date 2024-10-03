pub trait AllOk: Iterator {
    fn all_ok<Err, Pred>(&mut self, predicate: Pred) -> Result<bool, Err>
    where
        Self: Sized,
        Pred: FnMut(Self::Item) -> Result<bool, Err>;
}

impl<Iter: Iterator> AllOk for Iter {
    fn all_ok<Err, Pred>(&mut self, mut predicate: Pred) -> Result<bool, Err>
    where
        Self: Sized,
        Pred: FnMut(Self::Item) -> Result<bool, Err>,
    {
        for item in self {
            let result = predicate(item)?;
            if !result {
                return Ok(false);
            }
        }
        Ok(true)
    }
}

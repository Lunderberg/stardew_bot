pub trait FindOk: Iterator {
    fn find_ok<Err, Pred>(
        &mut self,
        predicate: Pred,
    ) -> Result<Option<Self::Item>, Err>
    where
        Self: Sized,
        Pred: FnMut(&Self::Item) -> Result<bool, Err>;
}

impl<Iter: Iterator> FindOk for Iter {
    fn find_ok<Err, Pred>(
        &mut self,
        mut predicate: Pred,
    ) -> Result<Option<Self::Item>, Err>
    where
        Self: Sized,
        Pred: FnMut(&Self::Item) -> Result<bool, Err>,
    {
        self.find_map(|item| -> Option<Result<Self::Item, Err>> {
            match predicate(&item) {
                Ok(true) => Some(Ok(item)),
                Ok(false) => None,
                Err(err) => Some(Err(err)),
            }
        })
        .transpose()
    }
}

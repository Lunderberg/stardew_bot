pub trait TryFind: Iterator<Item = Result<Self::T, Self::Error>> {
    type T;
    type Error;
    fn try_find<E>(
        self,
        predicate: impl FnMut(&Self::T) -> Result<bool, E>,
    ) -> Result<Option<Self::T>, E>
    where
        E: From<Self::Error>;
}

impl<Iter, T, Error> TryFind for Iter
where
    Iter: Iterator<Item = Result<T, Error>>,
{
    type T = T;

    type Error = Error;

    fn try_find<E>(
        self,
        mut predicate: impl FnMut(&Self::T) -> Result<bool, E>,
    ) -> Result<Option<Self::T>, E>
    where
        E: From<Self::Error>,
    {
        for item in self {
            let item = item?;
            if predicate(&item)? {
                return Ok(Some(item));
            }
        }
        Ok(None)
    }
}

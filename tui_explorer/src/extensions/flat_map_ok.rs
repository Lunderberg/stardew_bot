use itertools::Either;

pub trait FlatMapOk<T, E>: Iterator<Item = Result<T, E>> {
    fn flat_map_ok<Func, OutIter>(
        self,
        mut func: Func,
    ) -> impl Iterator<Item = Result<OutIter::Item, E>>
    where
        Self: Sized,
        Func: FnMut(T) -> Result<OutIter, E>,
        OutIter: IntoIterator,
    {
        self.flat_map(move |res_item: Result<T, E>| {
            let res_iter: Result<OutIter, E> =
                res_item.and_then(|item| func(item));
            match res_iter {
                Ok(iter) => Either::Left(iter.into_iter().map(Ok)),
                Err(err) => Either::Right(std::iter::once(Err(err))),
            }
        })
    }
}

impl<Iter, T, E> FlatMapOk<T, E> for Iter where
    Iter: Iterator<Item = Result<T, E>>
{
}

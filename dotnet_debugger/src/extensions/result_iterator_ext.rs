use itertools::Either;

pub trait ResultIteratorExt<T, E>:
    Iterator<Item = Result<T, E>> + Sized
{
    fn and_map_ok<Func, U>(
        self,
        mut func: Func,
    ) -> impl Iterator<Item = Result<U, E>>
    where
        Self: Sized,
        Self: Iterator<Item = Result<T, E>>,
        Func: FnMut(T) -> Result<U, E>,
    {
        self.map(move |res| res.and_then(|item| func(item)))
    }

    // TODO: Move this into a separate extension crate, so that it
    // doesn't need to be a duplicate of
    // `tui_explorer/src/extensions/flat_map_ok.rs`.
    fn flat_map_ok<Func, OutIter, U>(
        self,
        mut func: Func,
    ) -> impl Iterator<Item = Result<U, E>>
    where
        Self: Sized,
        Func: FnMut(T) -> Result<OutIter, E>,
        OutIter: IntoIterator<Item = U>,
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

    fn and_flat_map_ok<Func, OutIter, U>(
        self,
        func: Func,
    ) -> impl Iterator<Item = Result<U, E>>
    where
        Self: Sized,
        Func: FnMut(T) -> Result<OutIter, E>,
        OutIter: IntoIterator<Item = Result<U, E>>,
    {
        self.flat_map_ok(func).map(|res| res?)
    }
}

impl<Iter, T, E> ResultIteratorExt<T, E> for Iter where
    Iter: Iterator<Item = Result<T, E>>
{
}

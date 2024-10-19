use itertools::Either;

pub trait ResultIteratorExt: Iterator + Sized {
    fn and_map_ok<Func, T, U, E>(
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

    fn and_filter_ok<Func, T, E>(
        self,
        mut func: Func,
    ) -> impl Iterator<Item = Result<T, E>>
    where
        Self: Sized,
        Self: Iterator<Item = Result<T, E>>,
        Func: FnMut(&T) -> Result<bool, E>,
    {
        self.filter_map(move |res| match res {
            Ok(value) => match func(&value) {
                Ok(true) => Some(Ok(value)),
                Ok(false) => None,
                Err(err) => Some(Err(err)),
            },
            Err(err) => Some(Err(err)),
        })
    }

    fn flat_map_ok<Func, OutIter, T, U, E>(
        self,
        mut func: Func,
    ) -> impl Iterator<Item = Result<U, E>>
    where
        Self: Sized,
        Self: Iterator<Item = Result<T, E>>,
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

    fn and_flat_map_ok<Func, OutIter, T, U, E>(
        self,
        func: Func,
    ) -> impl Iterator<Item = Result<U, E>>
    where
        Self: Sized,
        Self: Iterator<Item = Result<T, E>>,
        Func: FnMut(T) -> Result<OutIter, E>,
        OutIter: IntoIterator<Item = Result<U, E>>,
    {
        self.flat_map_ok(func).map(|res| res?)
    }

    fn and_find<Func, E>(
        &mut self,
        mut func: Func,
    ) -> Result<Option<Self::Item>, E>
    where
        Func: FnMut(&Self::Item) -> Result<bool, E>,
    {
        self.find_map(|item| match func(&item) {
            Ok(true) => Some(Ok(item)),
            Ok(false) => None,
            Err(err) => Some(Err(err)),
        })
        .transpose()
    }

    fn and_find_ok<Pred, T, E>(self, predicate: Pred) -> Result<Option<T>, E>
    where
        // I have no idea why this indirection is necessary for type
        // inference.  However, if I were inline the
        // `and_find_ok_impl`, then the predicate must have its
        // argument annotated at the callsite, rather than being
        // inferred from the iterator type.
        //
        //     // What I want to write
        //     iter.and_find_ok(|val| ...)
        //
        //     // What I need to write without `and_find_ok_impl`
        //     iter.and_find_ok(|val: &SomeType| ...)
        //
        // The same thing happens if I add `E=E` to the projection
        // bounds on `ResultIteratorExt2`.  So, something weird is
        // probably happening where additional information allows for
        // a generic type to be substituted in, but that substitution
        // throws out a relationship with the predicate's argument.
        Self: AndFindOkImpl<T = T>,
        Pred: FnMut(&T) -> Result<bool, E>,
        E: From<<Self as AndFindOkImpl>::E>,
    {
        <Self as AndFindOkImpl>::and_find_ok_impl(self, predicate)
    }

    fn and_all<Pred, E>(&mut self, mut predicate: Pred) -> Result<bool, E>
    where
        Self: Sized,
        Pred: FnMut(Self::Item) -> Result<bool, E>,
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

impl<Iter> ResultIteratorExt for Iter where Iter: Iterator {}

pub trait AndFindOkImpl: Iterator<Item = Result<Self::T, Self::E>> {
    type T;
    type E;

    fn and_find_ok_impl<Error>(
        self,
        mut predicate: impl FnMut(&Self::T) -> Result<bool, Error>,
    ) -> Result<Option<Self::T>, Error>
    where
        Self: Sized,
        Error: From<Self::E>,
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

impl<Iter, T, Error> AndFindOkImpl for Iter
where
    Iter: Iterator<Item = Result<T, Error>>,
{
    type T = T;

    type E = Error;
}

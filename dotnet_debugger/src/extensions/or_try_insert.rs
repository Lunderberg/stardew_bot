use std::collections::hash_map::Entry;

pub trait OrTryInsert<'a, K: 'a, V: 'a> {
    fn or_try_insert<F, E>(self, default: F) -> Result<&'a mut V, E>
    where
        F: FnOnce(&K) -> Result<V, E>;
}

impl<'a, K: 'a, V: 'a> OrTryInsert<'a, K, V> for Entry<'a, K, V> {
    fn or_try_insert<F, E>(self, default: F) -> Result<&'a mut V, E>
    where
        F: FnOnce(&K) -> Result<V, E>,
    {
        match self {
            Entry::Occupied(occupied) => Ok(occupied.into_mut()),
            Entry::Vacant(vacant) => {
                let value = default(vacant.key())?;
                Ok(vacant.insert(value))
            }
        }
    }
}

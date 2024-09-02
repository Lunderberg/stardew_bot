use std::hash::Hash;

use elsa::FrozenMap;

pub trait TryInsert {
    type K;
    type V;
    fn try_insert<Func, Err>(
        &self,
        key: Self::K,
        func: Func,
    ) -> Result<&Self::V, Err>
    where
        Func: FnOnce() -> Result<Self::V, Err>;
}

impl<K, V> TryInsert for FrozenMap<K, Box<V>>
where
    K: Eq + Hash,
{
    type K = K;
    type V = V;

    fn try_insert<Func, Err>(
        &self,
        key: Self::K,
        func: Func,
    ) -> Result<&Self::V, Err>
    where
        Func: FnOnce() -> Result<Self::V, Err>,
    {
        if let Some(value) = self.get(&key) {
            Ok(value)
        } else {
            let value = Box::new(func()?);
            Ok(self.insert(key, value))
        }
    }
}

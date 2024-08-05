use std::marker::PhantomData;

/// Utility to use a simple enum as a lookup key.
#[derive(Clone)]
pub struct EnumMap<Key, Value> {
    // Once `generic_const_exprs` is available, the size can be
    // determined from `<Key as EnumKey>::N` For now, setting it to
    // the size required for `MetadataIndexKind`.
    values: [Value; 3 + 38 + 13],
    _phantom: PhantomData<Key>,
}

/// Trait to define how an enum should be used as a key.
pub trait EnumKey: Sized {
    const N: usize;

    fn as_index(self) -> usize;

    fn iter_keys() -> impl Iterator<Item = Self>;
}

impl<Key, Value> EnumMap<Key, Value> {
    pub fn init(func: impl Fn() -> Value) -> Self {
        Self {
            values: std::array::from_fn(|_| func()),
            _phantom: PhantomData,
        }
    }
}

impl<Key, Value: Default> Default for EnumMap<Key, Value> {
    fn default() -> Self {
        Self {
            values: std::array::from_fn(|_| Default::default()),
            _phantom: PhantomData,
        }
    }
}

impl<Key, Index, Value> std::ops::Index<Index> for EnumMap<Key, Value>
where
    Key: EnumKey,
    Index: Into<Key>,
{
    type Output = Value;

    fn index(&self, index: Index) -> &Self::Output {
        &self.values[index.into().as_index()]
    }
}

impl<Key, Index, Value> std::ops::IndexMut<Index> for EnumMap<Key, Value>
where
    Key: EnumKey,
    Index: Into<Key>,
{
    fn index_mut(&mut self, index: Index) -> &mut Self::Output {
        &mut self.values[index.into().as_index()]
    }
}

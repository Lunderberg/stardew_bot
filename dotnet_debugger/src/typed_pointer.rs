use std::marker::PhantomData;

use memory_reader::{MemoryReader, Pointer, UnpackBytes};

use crate::Error;

pub struct TypedPointer<T> {
    ptr: Pointer,
    _phantom: PhantomData<T>,
}

pub trait ReadTypedPointer: Sized {
    fn read_typed_ptr(
        ptr: Pointer,
        reader: &MemoryReader,
    ) -> Result<Self, Error>;
}

impl<T> TypedPointer<T> {
    pub fn new(ptr: Pointer) -> Self {
        Self {
            ptr,
            _phantom: PhantomData,
        }
    }

    pub fn read(&self, reader: &MemoryReader) -> Result<T, Error>
    where
        T: ReadTypedPointer,
    {
        T::read_typed_ptr(self.ptr, reader)
    }
}

impl<T> std::fmt::Debug for TypedPointer<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TypedPointer")
            .field("ptr", &self.ptr)
            .finish()
    }
}

impl<T> std::fmt::Display for TypedPointer<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ptr)
    }
}

impl<T> std::ops::Deref for TypedPointer<T> {
    type Target = Pointer;

    fn deref(&self) -> &Self::Target {
        &self.ptr
    }
}

impl<T> Clone for TypedPointer<T> {
    fn clone(&self) -> Self {
        Self {
            ptr: self.ptr,
            _phantom: PhantomData,
        }
    }
}
impl<T> Copy for TypedPointer<T> {}

impl<T> PartialEq for TypedPointer<T> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }
}
impl<T> Eq for TypedPointer<T> {}
impl<T> PartialEq<Pointer> for TypedPointer<T> {
    fn eq(&self, other: &Pointer) -> bool {
        self.ptr == *other
    }
}
impl<T> PartialEq<TypedPointer<T>> for Pointer {
    fn eq(&self, other: &TypedPointer<T>) -> bool {
        *self == other.ptr
    }
}

impl<T> std::hash::Hash for TypedPointer<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ptr.hash(state);
    }
}

impl<T> From<Pointer> for TypedPointer<T> {
    fn from(ptr: Pointer) -> Self {
        Self::new(ptr)
    }
}

impl<T> Into<Pointer> for TypedPointer<T> {
    fn into(self) -> Pointer {
        self.ptr
    }
}

impl<'a, T> UnpackBytes<'a> for TypedPointer<T> {
    type Error = <Pointer as UnpackBytes<'a>>::Error;

    fn unpack(
        bytes: memory_reader::ByteRange<'a>,
    ) -> Result<Self, Self::Error> {
        let ptr: Pointer = bytes.unpack()?;
        Ok(ptr.into())
    }
}

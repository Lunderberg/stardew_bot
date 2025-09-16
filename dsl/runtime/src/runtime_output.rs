use std::any::Any;

use dsl_ir::{Pointer, RustNativeObject, RustType, StackValue};

use crate::{Error, ValueIndex};

pub struct RuntimeOutput {
    values: Vec<Option<StackValue>>,
    num_instructions_evaluated: usize,
}

mod detail {
    pub trait NormalizeStackIndex {
        fn normalize_stack_index(self) -> usize;
    }
    impl NormalizeStackIndex for super::ValueIndex {
        fn normalize_stack_index(self) -> usize {
            self.0
        }
    }
    impl NormalizeStackIndex for usize {
        fn normalize_stack_index(self) -> usize {
            self
        }
    }
}

impl RuntimeOutput {
    pub fn new(size: usize) -> Self {
        let values = (0..size).map(|_| None).collect();
        Self {
            values,
            num_instructions_evaluated: 0,
        }
    }

    pub fn replace(self, values: Vec<Option<StackValue>>) -> Self {
        Self { values, ..self }
    }

    pub fn get(
        &self,
        index: impl detail::NormalizeStackIndex,
    ) -> Option<&StackValue> {
        let index = index.normalize_stack_index();
        self[index].as_ref()
    }

    pub fn get_as<'a, T>(
        &'a self,
        index: impl detail::NormalizeStackIndex,
    ) -> Result<Option<T>, Error>
    where
        &'a StackValue: TryInto<T>,
        Error: From<<&'a StackValue as TryInto<T>>::Error>,
    {
        let index = index.normalize_stack_index();
        let opt_value = self[index]
            .as_ref()
            .map(|value| value.try_into())
            .transpose()?;

        Ok(opt_value)
    }

    pub fn get_any(
        &self,
        index: impl detail::NormalizeStackIndex,
    ) -> Result<Option<&dyn Any>, Error> {
        self.get_as(index)
    }

    pub fn take_obj<T: RustNativeObject>(
        &mut self,
        index: impl detail::NormalizeStackIndex,
    ) -> Result<Option<T>, Error> {
        let index = index.normalize_stack_index();

        Ok(self[index]
            .take()
            .map(|value| match value {
                StackValue::Native(native) => {
                    native.downcast::<T>().map_err(|native| {
                        Error::IncorrectOutputType {
                            attempted: RustType::new::<T>().into(),
                            actual: native.runtime_type(),
                        }
                    })
                }
                other => Err(Error::IncorrectOutputType {
                    attempted: RustType::new::<T>().into(),
                    actual: other.runtime_type(),
                }),
            })
            .transpose()?
            .map(|boxed| *boxed))
    }

    pub fn get_obj<T: RustNativeObject>(
        &self,
        index: impl detail::NormalizeStackIndex,
    ) -> Result<Option<&T>, Error> {
        let index = index.normalize_stack_index();
        let opt_value = self[index].as_ref();

        let opt_obj = opt_value
            .map(|value| match value {
                StackValue::Native(native) => native
                    .downcast_ref::<T>()
                    .ok_or_else(|| Error::IncorrectOutputType {
                        attempted: RustType::new::<T>().into(),
                        actual: native.runtime_type(),
                    }),
                other => Err(Error::IncorrectOutputType {
                    attempted: RustType::new::<T>().into(),
                    actual: other.runtime_type(),
                }),
            })
            .transpose()?;

        Ok(opt_obj)
    }

    pub fn num_instructions_evaluated(&self) -> usize {
        self.num_instructions_evaluated
    }

    pub fn increment_eval_counter(&mut self) {
        self.num_instructions_evaluated += 1;
    }

    pub fn push(&mut self, value: Option<StackValue>) {
        self.values.push(value);
    }

    pub fn pop(&mut self) -> Result<Option<StackValue>, Error> {
        self.values.pop().ok_or(Error::CannotPopFromEmptyValueStack)
    }
}

impl IntoIterator for RuntimeOutput {
    type Item = <Vec<Option<StackValue>> as IntoIterator>::Item;

    type IntoIter = <Vec<Option<StackValue>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.values.into_iter()
    }
}

impl std::ops::Deref for RuntimeOutput {
    type Target = Vec<Option<StackValue>>;

    fn deref(&self) -> &Self::Target {
        &self.values
    }
}
impl std::ops::DerefMut for RuntimeOutput {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.values
    }
}

impl std::ops::Index<usize> for RuntimeOutput {
    type Output = Option<StackValue>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.values[index]
    }
}
impl std::ops::IndexMut<usize> for RuntimeOutput {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.values[index]
    }
}

impl std::ops::Index<ValueIndex> for RuntimeOutput {
    type Output = Option<StackValue>;

    fn index(&self, index: ValueIndex) -> &Self::Output {
        &self.values[index.0]
    }
}
impl std::ops::IndexMut<ValueIndex> for RuntimeOutput {
    fn index_mut(&mut self, index: ValueIndex) -> &mut Self::Output {
        &mut self.values[index.0]
    }
}
impl std::ops::Index<std::ops::Range<ValueIndex>> for RuntimeOutput {
    type Output = [Option<StackValue>];

    fn index(&self, index: std::ops::Range<ValueIndex>) -> &Self::Output {
        &self.values[index.start.0..index.end.0]
    }
}
impl std::ops::IndexMut<std::ops::Range<ValueIndex>> for RuntimeOutput {
    fn index_mut(
        &mut self,
        index: std::ops::Range<ValueIndex>,
    ) -> &mut Self::Output {
        &mut self.values[index.start.0..index.end.0]
    }
}
impl std::ops::Index<std::ops::RangeFrom<ValueIndex>> for RuntimeOutput {
    type Output = [Option<StackValue>];

    fn index(&self, index: std::ops::RangeFrom<ValueIndex>) -> &Self::Output {
        &self.values[index.start.0..]
    }
}
impl std::ops::IndexMut<std::ops::RangeFrom<ValueIndex>> for RuntimeOutput {
    fn index_mut(
        &mut self,
        index: std::ops::RangeFrom<ValueIndex>,
    ) -> &mut Self::Output {
        &mut self.values[index.start.0..]
    }
}
impl std::ops::Index<std::ops::RangeFull> for RuntimeOutput {
    type Output = [Option<StackValue>];

    fn index(&self, _: std::ops::RangeFull) -> &Self::Output {
        &self.values[..]
    }
}
impl std::ops::IndexMut<std::ops::RangeFull> for RuntimeOutput {
    fn index_mut(&mut self, _: std::ops::RangeFull) -> &mut Self::Output {
        &mut self.values[..]
    }
}

impl std::ops::Add<usize> for ValueIndex {
    type Output = ValueIndex;

    fn add(self, rhs: usize) -> Self::Output {
        ValueIndex(self.0 + rhs)
    }
}
impl std::ops::Sub for ValueIndex {
    type Output = usize;

    fn sub(self, rhs: Self) -> Self::Output {
        self.0 - rhs.0
    }
}

macro_rules! values_to_single_prim {
    ($prim:ty) => {
        impl TryInto<$prim> for RuntimeOutput {
            type Error = Error;

            fn try_into(mut self) -> Result<$prim, Self::Error> {
                let num_elements = self.values.len();
                if num_elements == 1 {
                    self.values[0]
                        .take()
                        .ok_or(Error::AttemptedConversionOfMissingValue)?
                        .try_into()
                        .map_err(Into::into)
                } else {
                    Err(Error::IncorrectNumberOfValues {
                        expected: 1,
                        actual: num_elements,
                    })
                }
            }
        }
    };
}

values_to_single_prim!(bool);
values_to_single_prim!(u8);
values_to_single_prim!(u16);
values_to_single_prim!(u32);
values_to_single_prim!(u64);
values_to_single_prim!(usize);
values_to_single_prim!(i8);
values_to_single_prim!(i16);
values_to_single_prim!(i32);
values_to_single_prim!(i64);
values_to_single_prim!(isize);
values_to_single_prim!(f32);
values_to_single_prim!(f64);
values_to_single_prim!(Pointer);

impl TryInto<Box<dyn Any>> for RuntimeOutput {
    type Error = Error;

    fn try_into(mut self) -> Result<Box<dyn Any>, Self::Error> {
        let num_elements = self.values.len();
        if num_elements == 1 {
            self.values[0]
                .take()
                .ok_or(Error::AttemptedConversionOfMissingValue)?
                .try_into()
                .map_err(Into::into)
        } else {
            Err(Error::IncorrectNumberOfValues {
                expected: 1,
                actual: num_elements,
            })
        }
    }
}

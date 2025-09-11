use std::{any::Any, borrow::Cow, marker::PhantomData};

use itertools::Itertools as _;

use crate::{DSLType, Error, RustType};

use super::{RustNativeObject, StackValue};

#[derive(Debug)]
pub struct ExposedNativeObject {
    pub(crate) obj: Box<dyn Any>,
    pub(crate) ty: DSLType,
}

impl ExposedNativeObject {
    pub(crate) fn new<T: RustNativeObject>(obj: T) -> Self {
        let obj = Box::new(obj);
        let ty: DSLType = RustType::new::<T>().into();
        Self { obj, ty }
    }

    pub fn downcast_ref<T: RustNativeObject>(&self) -> Option<&T> {
        self.obj.downcast_ref()
    }

    pub fn downcast_mut<T: RustNativeObject>(&mut self) -> Option<&mut T> {
        self.obj.downcast_mut()
    }

    pub fn downcast<T: RustNativeObject>(self) -> Result<Box<T>, Self> {
        self.obj.downcast().map_err(|obj| Self { obj, ty: self.ty })
    }

    pub fn type_id(&self) -> std::any::TypeId {
        self.obj.as_ref().type_id()
    }

    pub(crate) fn runtime_type(&self) -> DSLType {
        self.ty.clone()
    }
}

impl From<ExposedNativeObject> for Box<dyn Any> {
    fn from(val: ExposedNativeObject) -> Self {
        val.obj
    }
}

impl AsRef<dyn Any> for ExposedNativeObject {
    fn as_ref(&self) -> &dyn Any {
        self.obj.as_ref()
    }
}

impl AsMut<dyn Any> for ExposedNativeObject {
    fn as_mut(&mut self) -> &mut dyn Any {
        self.obj.as_mut()
    }
}

pub(crate) trait RustNativeTypeUtils {
    fn clone(&self) -> Box<dyn RustNativeTypeUtils>;

    fn new_vector(&self) -> Result<ExposedNativeObject, Error>;

    fn collect_into_vector(
        &self,
        vec: &mut StackValue,
        item: &mut Option<StackValue>,
        output_name: &str,
    ) -> Result<(), Error>;

    fn vector_type(&self) -> Result<DSLType, Error>;

    fn type_name(&self) -> Cow<'static, str>;
}
pub(crate) struct RustNativeUtilContainer<T>(PhantomData<T>);
impl<T: RustNativeObject> RustNativeUtilContainer<T> {
    pub(crate) fn new() -> Self {
        Self(PhantomData)
    }
}
impl<T: RustNativeObject> RustNativeTypeUtils for RustNativeUtilContainer<T> {
    fn clone(&self) -> Box<dyn RustNativeTypeUtils> {
        Box::new(Self::new())
    }

    fn new_vector(&self) -> Result<ExposedNativeObject, Error> {
        T::new_vector()
    }

    fn collect_into_vector(
        &self,
        vec: &mut StackValue,
        item: &mut Option<StackValue>,
        output_name: &str,
    ) -> Result<(), Error> {
        T::collect_into_vector(vec, item, output_name)
    }

    fn vector_type(&self) -> Result<DSLType, Error> {
        T::vector_type()
    }

    fn type_name(&self) -> Cow<'static, str> {
        let name = std::any::type_name::<T>();

        fn last_path_segment(segment: &str) -> &str {
            if let Some((_, last)) = segment.rsplit_once("::") {
                last
            } else {
                segment
            }
        }

        if name.contains('<') {
            let name: String = name
                .split_inclusive(['<', '>'])
                .map(last_path_segment)
                .join("");
            name.into()
        } else {
            last_path_segment(name).into()
        }
    }
}

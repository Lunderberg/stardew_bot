use dotnet_debugger::{CachedReader, FieldDescription, MethodTable};
use dsl_ir::{DSLType, StaticField, TypedPointer};

use crate::{SymbolicTypeExt as _, TypeInferenceError};

pub trait StaticFieldExt {
    fn method_table_and_field<'a>(
        &self,
        reader: CachedReader<'a>,
    ) -> Result<
        (TypedPointer<MethodTable>, FieldDescription<'a>),
        TypeInferenceError,
    >;
    fn runtime_type(
        &self,
        reader: CachedReader<'_>,
    ) -> Result<DSLType, TypeInferenceError>;
}
impl StaticFieldExt for StaticField {
    fn method_table_and_field<'a>(
        &self,
        reader: CachedReader<'a>,
    ) -> Result<
        (TypedPointer<MethodTable>, FieldDescription<'a>),
        TypeInferenceError,
    > {
        let base_method_table_ptr = self.class.method_table(reader)?;

        let field = reader
            .iter_static_fields(base_method_table_ptr)?
            .find(|field| {
                reader
                    .field_to_name(field)
                    .map(|name| name == self.field_name)
                    .unwrap_or(false)
            })
            .ok_or_else(|| TypeInferenceError::NoSuchStaticField {
                class: format!("{}", self.class),
                field: self.field_name.clone(),
            })?;

        Ok((base_method_table_ptr, field))
    }

    fn runtime_type(
        &self,
        reader: CachedReader<'_>,
    ) -> Result<DSLType, TypeInferenceError> {
        let (base_method_table_ptr, field) =
            self.method_table_and_field(reader)?;

        let base_type =
            reader.field_to_runtime_type(base_method_table_ptr, &field)?;

        Ok(base_type.into())
    }
}

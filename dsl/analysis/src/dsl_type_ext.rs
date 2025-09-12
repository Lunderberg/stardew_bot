use dotnet_debugger::{DotNetType, MethodTable};
use dsl_ir::{DSLType, TypedPointer};

use crate::TypeInferenceError;

pub trait DSLTypeExt {
    fn method_table_for_field_access(
        &self,
        gen_name: impl FnOnce() -> String,
    ) -> Result<TypedPointer<MethodTable>, TypeInferenceError>;

    fn method_table_for_downcast(
        &self,
    ) -> Result<TypedPointer<MethodTable>, TypeInferenceError>;
}

impl DSLTypeExt for DSLType {
    fn method_table_for_field_access(
        &self,
        gen_name: impl FnOnce() -> String,
    ) -> Result<TypedPointer<MethodTable>, TypeInferenceError> {
        match self {
            DSLType::DotNet(DotNetType::ValueType { method_table, .. })
            | DSLType::DotNet(DotNetType::Class { method_table }) => {
                method_table.ok_or_else(|| {
                    TypeInferenceError::UnexpectedNullMethodTable(gen_name())
                })
            }
            _ => Err(TypeInferenceError::FieldAccessRequiresClassOrStruct(
                gen_name(),
                self.clone(),
            )),
        }
    }

    fn method_table_for_downcast(
        &self,
    ) -> Result<TypedPointer<MethodTable>, TypeInferenceError> {
        match self {
            DSLType::DotNet(DotNetType::Class { method_table }) => method_table
                .ok_or(TypeInferenceError::DowncastRequiresKnownBaseClass),
            _ => Err(TypeInferenceError::DowncastRequiresClassInstance(
                self.clone(),
            )),
        }
    }
}

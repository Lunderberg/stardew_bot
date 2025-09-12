use dotnet_debugger::{DotNetType, MethodTable};
use dsl_ir::{DSLType, TypedPointer};

use crate::Error;

pub trait DSLTypeExt {
    fn method_table_for_field_access(
        &self,
        gen_name: impl FnOnce() -> String,
    ) -> Result<TypedPointer<MethodTable>, Error>;

    fn method_table_for_downcast(
        &self,
    ) -> Result<TypedPointer<MethodTable>, Error>;
}

impl DSLTypeExt for DSLType {
    fn method_table_for_field_access(
        &self,
        gen_name: impl FnOnce() -> String,
    ) -> Result<TypedPointer<MethodTable>, Error> {
        match self {
            DSLType::DotNet(DotNetType::ValueType { method_table, .. })
            | DSLType::DotNet(DotNetType::Class { method_table }) => {
                method_table
                    .ok_or_else(|| Error::UnexpectedNullMethodTable(gen_name()))
            }
            _ => Err(Error::FieldAccessRequiresClassOrStruct(
                gen_name(),
                self.clone(),
            )),
        }
    }

    fn method_table_for_downcast(
        &self,
    ) -> Result<TypedPointer<MethodTable>, Error> {
        match self {
            DSLType::DotNet(DotNetType::Class { method_table }) => {
                method_table.ok_or(Error::DowncastRequiresKnownBaseClass)
            }
            _ => Err(Error::DowncastRequiresClassInstance(self.clone())),
        }
    }
}

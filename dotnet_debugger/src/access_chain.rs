use memory_reader::Pointer;

use crate::runtime_type::RuntimePrimType;

pub struct SymbolicAccessChain {
    pub static_field_class: String,
    pub static_field_name: String,
    pub ops: Vec<SymbolicOperation>,
}

pub enum SymbolicOperation {
    Field(String),
    IndexAccess(usize),
}

pub struct PhysicalAccessChain {
    _base: Pointer,
    _ops: Vec<PhysicalAccessOperation>,
    _prim_type: RuntimePrimType,
}

pub enum PhysicalAccessOperation {
    Dereference,
    Offset(usize),
}

impl std::fmt::Display for SymbolicAccessChain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.static_field_class, self.static_field_name)?;

        self.ops.iter().try_for_each(|op| match op {
            SymbolicOperation::Field(field) => write!(f, ".{field}"),
            SymbolicOperation::IndexAccess(index) => write!(f, "[{index}]"),
        })?;
        Ok(())
    }
}

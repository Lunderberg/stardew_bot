use memory_reader::Pointer;

use crate::runtime_type::RuntimePrimType;

pub struct SymbolicAccessChain {
    pub class_namespace: Option<String>,
    pub class_name: String,
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
        if let Some(namespace) = &self.class_namespace {
            write!(f, "{namespace}.")?;
        }
        write!(f, "{}.{}", self.class_name, self.static_field_name)?;

        self.ops.iter().try_for_each(|op| match op {
            SymbolicOperation::Field(field) => write!(f, ".{field}"),
            SymbolicOperation::IndexAccess(index) => write!(f, "[{index}]"),
        })?;
        Ok(())
    }
}

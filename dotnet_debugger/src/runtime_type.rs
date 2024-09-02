use memory_reader::Pointer;

#[derive(Clone, Copy, Debug)]
pub enum RuntimeType {
    Prim(RuntimePrimType),
    String,
    ValueType,
    Class,
    // GenericVar,
    // Array,
    // GenericInst,
    // TypedByRef,
    // FunctionPtr,
    // Object,
    // SizeArray,
    // MethodType,
}

#[derive(Clone, Copy, Debug)]
pub enum RuntimePrimType {
    Bool,
    Char,

    U8,
    U16,
    U32,
    U64,
    NativeUInt,

    I8,
    I16,
    I32,
    I64,
    NativeInt,

    F32,
    F64,

    Ptr,
}

impl RuntimePrimType {
    pub fn size_bytes(&self) -> usize {
        match self {
            RuntimePrimType::Bool => 1,
            // CLR uses this type to represent an element of UTF-16.
            // This is neither a C/C++ char (1 byte), nor is it a
            // unicode code point (4 bytes), nor is it a unicode code
            // point encoded into UTF-16 (either 2 or 4 bytes).  It is
            // a UTF-16 code unit, and may not have any semantic
            // meaning outside of the string in which it occurs.
            //
            // (From ECMA-335, section III.1.1.3, " CLI char type
            // occupies 2 bytes in memory and represents a Unicode
            // code unit using UTF-16 encoding.")
            RuntimePrimType::Char => 2,
            RuntimePrimType::U8 => 1,
            RuntimePrimType::U16 => 2,
            RuntimePrimType::U32 => 4,
            RuntimePrimType::U64 => 8,
            RuntimePrimType::NativeUInt => Pointer::SIZE,
            RuntimePrimType::I8 => 1,
            RuntimePrimType::I16 => 2,
            RuntimePrimType::I32 => 4,
            RuntimePrimType::I64 => 8,
            RuntimePrimType::NativeInt => Pointer::SIZE,
            RuntimePrimType::F32 => 4,
            RuntimePrimType::F64 => 8,
            RuntimePrimType::Ptr => Pointer::SIZE,
        }
    }
}

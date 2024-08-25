use itertools::Itertools as _;

use crate::{
    ByteRange, Error, MetadataCodedIndex, MetadataTableKind, TypeDefOrRef,
};

/// A CLR signature, as defined in ECMA-335, section II.23.2.
///
/// Parsing of all types is not yet implemented.
#[derive(Clone, Copy)]
pub struct Signature<'a> {
    bytes: ByteRange<'a>,
}

/// Decompress the values of a signature, as defined in ECMA-335,
/// section II.23.2.
///
/// This provides `Clone`, but does not provide `Copy`, because all
/// methods require `&mut self`.
#[derive(Clone)]
struct SignatureDecompressor<'a> {
    bytes: ByteRange<'a>,
    verbose: bool,
    offset: usize,
}

#[derive(Clone, Copy)]
pub struct SignatureFlags(u8);

#[derive(Clone, Copy, Debug)]
pub enum PrimType {
    Bool,
    Char,
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    F32,
    F64,
    NativeInt,
    NativeUInt,
}

#[derive(Clone, Debug)]
pub enum SignatureType {
    Prim(PrimType),
    ValueType(MetadataCodedIndex<TypeDefOrRef>),
    Class(MetadataCodedIndex<TypeDefOrRef>),

    Array {
        element_type: Box<SignatureType>,
        #[allow(dead_code)]
        rank: u32,
        #[allow(dead_code)]
        fixed_sizes: Vec<u32>,
        #[allow(dead_code)]
        lower_bounds: Vec<u32>,
    },
    SizeArray(Box<SignatureType>),

    GenericType(u32),
    GenericInst {
        _is_value_type: bool,
        index: MetadataCodedIndex<TypeDefOrRef>,
        type_args: Vec<Box<SignatureType>>,
    },
    Object,
    String,
}

#[derive(Clone, Copy)]
pub enum CallingConvention {
    /// Default calling convention.  Allowed in MethodDef, MethodRef,
    /// StandAloneMethod.
    Default,

    /// C-style calling convention.  Only allowed in StandaloneMethod.
    C,

    /// C-style calling convention.  Only allowed in StandaloneMethod.
    StdCall,

    /// C-style calling convention.  Only allowed in StandaloneMethod.
    ThisCall,

    /// C-style calling convention.  Only allowed in StandaloneMethod.
    FastCall,

    /// Variadic argument calling convention.  Allowed in MethodDef,
    /// MethodRef, StandAloneMethod.
    VarArg,
}

/// Equivalent representation to CorElementType
#[derive(Clone, Copy, Debug)]
pub enum ElementType {
    End,
    Void,
    Prim(PrimType),
    // Bool,
    // Char,
    // I8,
    // U8,
    // I16,
    // U16,
    // I32,
    // U32,
    // I64,
    // U64,
    // F32,
    // F64,
    String,
    Ptr,
    ByRef,
    ValueType,
    Class,
    Var,
    Array,
    GenericInst,
    TypedByRef,
    // NativeInt,
    // NativeUInt,
    FunctionPtr,
    Object,
    SizeArray,
    MethodType,
    RequiredCustomModifier,
    OptionalCustomModifier,
    Internal,

    // Not sure whether this will need special handling
    Modifier,
    Sentinel,
    Pinned,
}

impl<'a> Signature<'a> {
    pub fn new(bytes: ByteRange<'a>) -> Self {
        Self { bytes }
    }

    pub fn flags(&self) -> SignatureFlags {
        SignatureFlags(self.bytes[0])
    }
}

impl SignatureFlags {
    /// Determine the calling convention from the first 4 bits.
    pub fn calling_convention(&self) -> Result<CallingConvention, Error> {
        match self.0 & 0x0f {
            0 => Ok(CallingConvention::Default),
            1 => Ok(CallingConvention::C),
            2 => Ok(CallingConvention::StdCall),
            3 => Ok(CallingConvention::ThisCall),
            4 => Ok(CallingConvention::FastCall),
            5 => Ok(CallingConvention::VarArg),
            other => Err(Error::InvalidCallingConvention(other)),
        }
    }

    /// Verify the first 4 bits contain 0x6, required for a Field
    /// signature.
    pub fn check_field(&self) -> Result<(), Error> {
        match self.0 & 0x0f {
            6 => Ok(()),
            other => Err(Error::InvalidFieldSignature(other)),
        }
    }

    /// Verify the first 4 bits contain 0x7, required for a LocalVar
    /// signature.
    pub fn check_local_var(&self) -> Result<(), Error> {
        match self.0 & 0x0f {
            7 => Ok(()),
            other => Err(Error::InvalidLocalVarSignature(other)),
        }
    }

    /// Verify the first 4 bits contain 0x8, required for a Property
    /// signature.
    pub fn check_property(&self) -> Result<(), Error> {
        match self.0 & 0x0f {
            8 => Ok(()),
            other => Err(Error::InvalidPropertySignature(other)),
        }
    }

    /// Used for MethodDef and MethodRef signatures to indicates that
    /// this method has generic parameters.
    pub fn is_generic(&self) -> bool {
        self.0 & 0x10 > 0
    }

    /// Used for MethodDef, MethodRef, Field, and Property signatures
    /// to indicate that this is associated with an instance (i.e. is
    /// not static).
    pub fn has_this(&self) -> bool {
        self.0 & 0x20 > 0
    }

    /// Indicates whether the type of the `this` pointer is explicitly
    /// listed as the first argument in the parameter list.
    pub fn explicit_this(&self) -> bool {
        self.0 & 0x40 > 0
    }
}

#[derive(Debug)]
enum CompressedValue {
    U8(u8),
    U16(u16),
    U32(u32),
}

impl<'a> SignatureDecompressor<'a> {
    fn next_byte(&mut self) -> Result<u8, Error> {
        if self.bytes.is_empty() {
            Err(Error::UnexpectedEndOfMetadataSignature)
        } else {
            let res = self.bytes[0];
            self.bytes = self.bytes.subrange(1..);
            self.offset += 1;
            Ok(res)
        }
    }

    fn next_compressed_value(&mut self) -> Result<CompressedValue, Error> {
        if self.bytes.is_empty() {
            return Err(Error::UnexpectedEndOfMetadataSignature);
        }

        let leading_ones = self.bytes[0].leading_ones();
        let encoded_length = match leading_ones {
            0 => Ok(1),
            1 => Ok(2),
            2 => Ok(4),
            _ => Err(Error::InvalidCompressedValueInMetadataSignature {
                leading_ones,
            }),
        }?;

        if self.bytes.len() < encoded_length {
            return Err(Error::UnexpectedEndOfMetadataSignature);
        }

        let value = match encoded_length {
            1 => CompressedValue::U8(self.bytes[0] & 0x7f),
            2 => CompressedValue::U16(u16::from_be_bytes([
                self.bytes[0] & 0x3f,
                self.bytes[1],
            ])),
            4 => CompressedValue::U32(u32::from_be_bytes([
                self.bytes[0] & 0x1f,
                self.bytes[1],
                self.bytes[2],
                self.bytes[3],
            ])),
            _ => panic!(
                "Unreachable, due to earlier definition of encoded_length"
            ),
        };

        if self.verbose {
            println!(
                "At offset {}, bytes [{:?}] produce value {:?}",
                self.offset,
                self.bytes
                    .subrange(..encoded_length)
                    .bytes
                    .iter()
                    .map(|byte| format!("0x{byte:02x}"))
                    .join(", "),
                value
            );
        }

        self.offset += encoded_length;
        self.bytes = self.bytes.subrange(encoded_length..);

        Ok(value)
    }

    fn next_unsigned(&mut self) -> Result<u32, Error> {
        let value = match self.next_compressed_value()? {
            CompressedValue::U8(value) => value as u32,
            CompressedValue::U16(value) => value as u32,
            CompressedValue::U32(value) => value,
        };

        Ok(value)
    }

    #[allow(dead_code)]
    fn next_signed(&mut self) -> Result<i32, Error> {
        let value = match self.next_compressed_value()? {
            CompressedValue::U8(value) => {
                let value = value.rotate_right(1);
                i8::from_be_bytes(value.to_be_bytes()) as i32
            }
            CompressedValue::U16(value) => {
                let value = value.rotate_right(1);
                i16::from_be_bytes(value.to_be_bytes()) as i32
            }
            CompressedValue::U32(value) => {
                let value = value.rotate_right(1);
                i32::from_be_bytes(value.to_be_bytes())
            }
        };

        Ok(value)
    }

    fn next_element_type(&mut self) -> Result<ElementType, Error> {
        let offset = self.offset;
        let element: ElementType = self.next_unsigned()?.try_into()?;
        if self.verbose {
            println!("At offset {offset}, read element type {element}");
        }

        Ok(element)
        //self.next_unsigned()?.try_into()
    }

    fn next_type_def_or_ref(
        &mut self,
    ) -> Result<MetadataCodedIndex<TypeDefOrRef>, Error> {
        let offset = self.offset;
        let coded_index = self.next_unsigned()?;

        if self.verbose {
            println!("At offset {offset}, reading TypeDefOrRef with coded index {coded_index}");
        }

        let kind = match coded_index & 0x3 {
            0 => Ok(MetadataTableKind::TypeDef),
            1 => Ok(MetadataTableKind::TypeRef),
            2 => Ok(MetadataTableKind::TypeSpec),
            other => Err(Error::InvalidMetadataTypeDefOrRef(other)),
        }?;
        let index = (coded_index >> 2) as usize;
        let typed_index = MetadataCodedIndex::new(kind, index);

        if self.verbose {
            println!("Produced decoded index {typed_index}");
        }

        Ok(typed_index)
    }

    fn next_element_type_ignoring_custom_modifiers(
        &mut self,
    ) -> Result<ElementType, Error> {
        let mut offset = self.offset;
        // TODO: Actually handle the custom modifiers instead of just
        // throwing them out.
        let mut element = self.next_element_type()?;
        while element.is_custom_modifier() {
            self.next_type_def_or_ref()?;
            if self.verbose {
                println!("Skipping past {element} at offset {offset}");
            }
            offset = self.offset;
            element = self.next_element_type()?;
        }

        Ok(element)
    }

    fn next_type(&mut self) -> Result<SignatureType, Error> {
        let offset = self.offset;
        if self.verbose {
            println!("Starting parsing of a type at offset {offset}");
        }

        let element = self.next_element_type_ignoring_custom_modifiers()?;

        let ty = match element {
            ElementType::Prim(prim) => SignatureType::Prim(prim),
            ElementType::Var => {
                SignatureType::GenericType(self.next_unsigned()?)
            }
            ElementType::SizeArray => {
                SignatureType::SizeArray(Box::new(self.next_type()?))
            }
            ElementType::Class => {
                SignatureType::Class(self.next_type_def_or_ref()?)
            }
            ElementType::ValueType => {
                SignatureType::ValueType(self.next_type_def_or_ref()?)
            }
            ElementType::GenericInst => {
                let is_value_type = match self.next_element_type()? {
                    ElementType::Class => Ok(false),
                    ElementType::ValueType => Ok(true),
                    other => {
                        Err(Error::InvalidElementFollowingGenericInst(other))
                    }
                }?;

                let index = self.next_type_def_or_ref()?;

                let num_type_args = self.next_unsigned()?;
                let type_args = (0..num_type_args)
                    .map(|_| self.next_type().map(|ty| Box::new(ty)))
                    .collect::<Result<_, _>>()?;

                SignatureType::GenericInst {
                    _is_value_type: is_value_type,
                    index,
                    type_args,
                }
            }

            ElementType::Object => SignatureType::Object,
            ElementType::String => SignatureType::String,

            ElementType::Array => {
                let element_type = Box::new(self.next_type()?);
                let rank = self.next_unsigned()?;

                let num_sizes = self.next_unsigned()?;
                let fixed_sizes = (0..num_sizes)
                    .map(|_| self.next_unsigned())
                    .collect::<Result<_, _>>()?;

                let num_lower_bounds = self.next_unsigned()?;
                let lower_bounds = (0..num_lower_bounds)
                    .map(|_| self.next_unsigned())
                    .collect::<Result<_, _>>()?;

                SignatureType::Array {
                    element_type,
                    rank,
                    fixed_sizes,
                    lower_bounds,
                }
            }

            other => todo!("Map element type {other:?} to signature type"),
        };

        if self.verbose {
            println!(
                "At offset {}, \
                 finished parsing type that started at {offset}, \
                 type = {ty}",
                self.offset
            );
        }

        Ok(ty)
    }
}

impl ElementType {
    fn is_custom_modifier(&self) -> bool {
        matches!(
            self,
            Self::RequiredCustomModifier | Self::OptionalCustomModifier
        )
    }
}

impl std::fmt::Display for ElementType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl TryFrom<u32> for ElementType {
    type Error = Error;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            0x00 => Ok(Self::End),
            0x01 => Ok(Self::Void),
            0x02 => Ok(Self::Prim(PrimType::Bool)),
            0x03 => Ok(Self::Prim(PrimType::Char)),
            0x04 => Ok(Self::Prim(PrimType::I8)),
            0x05 => Ok(Self::Prim(PrimType::U8)),
            0x06 => Ok(Self::Prim(PrimType::I16)),
            0x07 => Ok(Self::Prim(PrimType::U16)),
            0x08 => Ok(Self::Prim(PrimType::I32)),
            0x09 => Ok(Self::Prim(PrimType::U32)),
            0x0a => Ok(Self::Prim(PrimType::I64)),
            0x0b => Ok(Self::Prim(PrimType::U64)),
            0x0c => Ok(Self::Prim(PrimType::F32)),
            0x0d => Ok(Self::Prim(PrimType::F64)),
            0x0e => Ok(Self::String),
            0x0f => Ok(Self::Ptr),
            0x10 => Ok(Self::ByRef),
            0x11 => Ok(Self::ValueType),
            0x12 => Ok(Self::Class),
            0x13 => Ok(Self::Var),
            0x14 => Ok(Self::Array),
            0x15 => Ok(Self::GenericInst),
            0x16 => Ok(Self::TypedByRef),
            0x18 => Ok(Self::Prim(PrimType::NativeInt)),
            0x19 => Ok(Self::Prim(PrimType::NativeUInt)),
            0x1b => Ok(Self::FunctionPtr),
            0x1c => Ok(Self::Object),
            0x1d => Ok(Self::SizeArray),
            0x1e => Ok(Self::MethodType),
            0x1f => Ok(Self::RequiredCustomModifier),
            0x20 => Ok(Self::OptionalCustomModifier),
            0x21 => Ok(Self::Internal),
            0x40 => Ok(Self::Modifier),
            0x41 => Ok(Self::Sentinel),
            0x45 => Ok(Self::Pinned),
            value => Err(Error::InvalidElementType(value)),
        }
    }
}

impl<'a> std::fmt::Display for Signature<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = SignatureDecompressor {
            bytes: self.bytes.clone(),
            verbose: false,
            offset: 0,
        };

        let flags = SignatureFlags(iter.next_byte().unwrap());

        if !flags.has_this() {
            write!(f, "static ")?;
        }

        match flags.0 & 0x0f {
            0..6 => write!(f, "TODO: MethodSig unpacking")?,
            6 => write!(f, "{}", iter.next_type().unwrap())?,
            7 => write!(f, "TODO: LocalVarSig unpacking")?,
            8 => write!(f, "TODO: Property unpacking")?,
            _ => (),
        }

        Ok(())
    }
}

impl std::fmt::Display for SignatureType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SignatureType::Prim(prim) => write!(f, "{prim}"),
            SignatureType::GenericType(index) => write!(f, "_{index}"),
            SignatureType::SizeArray(ty) => write!(f, "{ty}[]"),
            SignatureType::Array { element_type, .. } => {
                write!(f, "{element_type}[]")
            }
            SignatureType::Class(index) => write!(f, "{index}"),
            SignatureType::ValueType(index) => write!(f, "{index}"),
            SignatureType::GenericInst {
                index, type_args, ..
            } => {
                write!(f, "{index}<")?;
                type_args.iter().with_position().try_for_each(
                    |(position, arg)| match position {
                        itertools::Position::First
                        | itertools::Position::Only => write!(f, "{arg}"),
                        itertools::Position::Middle
                        | itertools::Position::Last => write!(f, ", {arg}"),
                    },
                )?;
                write!(f, ">")
            }
            SignatureType::Object => write!(f, "Object"),
            SignatureType::String => write!(f, "String"),
        }
    }
}

impl std::fmt::Display for PrimType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            PrimType::Bool => "bool",
            PrimType::Char => "char",
            PrimType::I8 => "i8",
            PrimType::U8 => "u8",
            PrimType::I16 => "i16",
            PrimType::U16 => "u16",
            PrimType::I32 => "i32",
            PrimType::U32 => "u32",
            PrimType::I64 => "i64",
            PrimType::U64 => "u64",
            PrimType::F32 => "f32",
            PrimType::F64 => "f64",
            PrimType::NativeInt => "isize",
            PrimType::NativeUInt => "usize",
        };
        write!(f, "{name}")
    }
}

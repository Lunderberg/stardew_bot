use itertools::Itertools as _;

use memory_reader::ByteRange;

use crate::{
    Error, Metadata, MetadataCodedIndex, MetadataTableKind, TypeDefOrRef,
};

/// A CLR signature, as defined in ECMA-335, section II.23.2.
///
/// Parsing of all types is not yet implemented.
#[derive(Clone, Copy)]
pub struct Signature<'a> {
    /// The bytes that compose the signature
    bytes: ByteRange<'a>,

    /// The metadata that owns this signature.  Used to determine
    /// appropriate names for TypeDefOrRef fields within the
    /// signature.
    metadata: Metadata<'a>,

    /// Which table referenced the signature.  Used to handle
    /// per-table parsing (e.g. the TypeSpec table points directly to
    /// a Type, and not to any flags.
    referenced_from: MetadataTableKind,
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
    metadata: Metadata<'a>,
}

#[derive(Clone, Copy)]
pub struct SignatureFlags(u8);

#[derive(Clone, Copy, Debug)]
pub enum SignaturePrimType {
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

#[derive(Clone)]
pub enum SignatureType<'a> {
    Prim(SignaturePrimType),
    ValueType {
        index: MetadataCodedIndex<TypeDefOrRef>,
        metadata: Metadata<'a>,
    },
    Class {
        index: MetadataCodedIndex<TypeDefOrRef>,
        metadata: Metadata<'a>,
    },

    MultiDimArray {
        element_type: Box<SignatureType<'a>>,
        rank: usize,
        fixed_sizes: Vec<usize>,
        lower_bounds: Vec<usize>,
    },
    SizeArray(Box<SignatureType<'a>>),

    GenericVarFromType(u32),
    GenericVarFromMethod(u32),
    GenericInst {
        is_value_type: bool,
        index: MetadataCodedIndex<TypeDefOrRef>,
        type_args: Vec<Box<SignatureType<'a>>>,
        metadata: Metadata<'a>,
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
    Prim(SignaturePrimType),
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
    GenericVarFromType,
    Array,
    GenericInst,
    TypedByRef,
    // NativeInt,
    // NativeUInt,
    FunctionPtr,
    Object,
    SizeArray,
    GenericVarFromMethod,
    RequiredCustomModifier,
    OptionalCustomModifier,
    Internal,

    // Not sure whether this will need special handling
    Modifier,
    Sentinel,
    Pinned,
}

impl<'a> Signature<'a> {
    pub fn new(
        bytes: ByteRange<'a>,
        metadata: Metadata<'a>,
        referenced_from: MetadataTableKind,
    ) -> Self {
        Self {
            bytes,
            metadata,
            referenced_from,
        }
    }

    pub fn flags(&self) -> SignatureFlags {
        SignatureFlags(self.bytes[0])
    }

    pub fn first_type(&self) -> Result<SignatureType<'a>, Error> {
        let mut unpacker = self.unpacker();
        let flags = unpacker.next_flags()?;
        flags.check_field()?;

        let ty = unpacker.next_type()?;

        Ok(ty)
    }

    pub fn is_value_type(&self) -> Result<bool, Error> {
        let ty = self.first_type()?;
        Ok(matches!(
            ty,
            SignatureType::ValueType { .. }
                | SignatureType::GenericInst {
                    is_value_type: true,
                    ..
                }
        ))
    }

    pub fn as_coded_index(
        &self,
    ) -> Result<Option<MetadataCodedIndex<TypeDefOrRef>>, Error> {
        let ty = self.first_type()?;

        Ok(match ty {
            SignatureType::ValueType { index, .. }
            | SignatureType::GenericInst { index, .. } => Some(index),
            _ => None,
        })
    }

    pub fn as_value_type(
        &self,
    ) -> Result<Option<MetadataCodedIndex<TypeDefOrRef>>, Error> {
        let ty = self.first_type()?;

        match ty {
            SignatureType::ValueType { index, .. }
            | SignatureType::GenericInst {
                index,
                is_value_type: true,
                ..
            } => Ok(Some(index)),
            _ => Ok(None),
        }
    }

    fn unpacker(&self) -> SignatureDecompressor<'a> {
        SignatureDecompressor {
            bytes: self.bytes,
            verbose: false,
            offset: 0,
            metadata: self.metadata,
        }
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

    /// Used for MethodDef, MethodRef, and Property signatures
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

    fn next_flags(&mut self) -> Result<SignatureFlags, Error> {
        let byte = self.next_byte()?;
        Ok(SignatureFlags(byte))
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
                    .bytes()
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
        assert!(index != 0);
        let index = index - 1;
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

    fn next_type(&mut self) -> Result<SignatureType<'a>, Error> {
        let offset = self.offset;
        if self.verbose {
            println!("Starting parsing of a type at offset {offset}");
        }

        let element = self.next_element_type_ignoring_custom_modifiers()?;

        let ty = match element {
            ElementType::Prim(prim) => SignatureType::Prim(prim),
            ElementType::GenericVarFromType => {
                SignatureType::GenericVarFromType(self.next_unsigned()?)
            }
            ElementType::GenericVarFromMethod => {
                SignatureType::GenericVarFromMethod(self.next_unsigned()?)
            }
            ElementType::SizeArray => {
                SignatureType::SizeArray(Box::new(self.next_type()?))
            }
            ElementType::Class => SignatureType::Class {
                index: self.next_type_def_or_ref()?,
                metadata: self.metadata,
            },
            ElementType::ValueType => SignatureType::ValueType {
                index: self.next_type_def_or_ref()?,
                metadata: self.metadata,
            },
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
                    .map(|_| self.next_type().map(Box::new))
                    .collect::<Result<_, _>>()?;

                SignatureType::GenericInst {
                    is_value_type,
                    index,
                    type_args,
                    metadata: self.metadata,
                }
            }

            ElementType::Object => SignatureType::Object,
            ElementType::String => SignatureType::String,

            ElementType::Array => {
                let element_type = Box::new(self.next_type()?);
                let rank = self.next_unsigned()? as usize;

                let num_sizes = self.next_unsigned()?;
                let fixed_sizes = (0..num_sizes)
                    .map(|_| self.next_unsigned())
                    .map_ok(|dim| dim as usize)
                    .collect::<Result<_, _>>()?;

                let num_lower_bounds = self.next_unsigned()?;
                let lower_bounds = (0..num_lower_bounds)
                    .map(|_| self.next_unsigned())
                    .map_ok(|bound| bound as usize)
                    .collect::<Result<_, _>>()?;

                SignatureType::MultiDimArray {
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
            0x02 => Ok(Self::Prim(SignaturePrimType::Bool)),
            0x03 => Ok(Self::Prim(SignaturePrimType::Char)),
            0x04 => Ok(Self::Prim(SignaturePrimType::I8)),
            0x05 => Ok(Self::Prim(SignaturePrimType::U8)),
            0x06 => Ok(Self::Prim(SignaturePrimType::I16)),
            0x07 => Ok(Self::Prim(SignaturePrimType::U16)),
            0x08 => Ok(Self::Prim(SignaturePrimType::I32)),
            0x09 => Ok(Self::Prim(SignaturePrimType::U32)),
            0x0a => Ok(Self::Prim(SignaturePrimType::I64)),
            0x0b => Ok(Self::Prim(SignaturePrimType::U64)),
            0x0c => Ok(Self::Prim(SignaturePrimType::F32)),
            0x0d => Ok(Self::Prim(SignaturePrimType::F64)),
            0x0e => Ok(Self::String),
            0x0f => Ok(Self::Ptr),
            0x10 => Ok(Self::ByRef),
            0x11 => Ok(Self::ValueType),
            0x12 => Ok(Self::Class),
            0x13 => Ok(Self::GenericVarFromType),
            0x14 => Ok(Self::Array),
            0x15 => Ok(Self::GenericInst),
            0x16 => Ok(Self::TypedByRef),
            0x18 => Ok(Self::Prim(SignaturePrimType::NativeInt)),
            0x19 => Ok(Self::Prim(SignaturePrimType::NativeUInt)),
            0x1b => Ok(Self::FunctionPtr),
            0x1c => Ok(Self::Object),
            0x1d => Ok(Self::SizeArray),
            0x1e => Ok(Self::GenericVarFromMethod),
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
            bytes: self.bytes,
            verbose: false,
            offset: 0,
            metadata: self.metadata,
        };

        if matches!(self.referenced_from, MetadataTableKind::TypeSpec) {
            write!(f, "{}", iter.next_type().unwrap())?
        } else {
            let flags = iter.next_flags().unwrap();

            match flags.0 & 0x0f {
                0..6 => write!(f, "TODO: MethodSig unpacking")?,
                6 => write!(f, "{}", iter.next_type().unwrap())?,
                7 => write!(f, "TODO: LocalVarSig unpacking")?,
                8 => write!(f, "TODO: Property unpacking")?,
                _ => (),
            }
        }

        Ok(())
    }
}

impl std::fmt::Debug for SignatureType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Prim(arg0) => f.debug_tuple("Prim").field(arg0).finish(),
            Self::ValueType { index, .. } => {
                f.debug_struct("ValueType").field("index", index).finish()
            }
            Self::Class { index, .. } => {
                f.debug_struct("Class").field("index", index).finish()
            }
            Self::MultiDimArray {
                element_type,
                rank,
                fixed_sizes,
                lower_bounds,
            } => f
                .debug_struct("Array")
                .field("element_type", element_type)
                .field("rank", rank)
                .field("fixed_sizes", fixed_sizes)
                .field("lower_bounds", lower_bounds)
                .finish(),
            Self::SizeArray(arg0) => {
                f.debug_tuple("SizeArray").field(arg0).finish()
            }
            Self::GenericVarFromType(arg0) => {
                f.debug_tuple("GenericVarFromType").field(arg0).finish()
            }
            Self::GenericVarFromMethod(arg0) => {
                f.debug_tuple("GenericVarFromMethod").field(arg0).finish()
            }
            Self::GenericInst {
                is_value_type,
                index,
                type_args,
                ..
            } => f
                .debug_struct("GenericInst")
                .field("is_value_type", is_value_type)
                .field("index", index)
                .field("type_args", type_args)
                .finish(),
            Self::Object => write!(f, "Object"),
            Self::String => write!(f, "String"),
        }
    }
}

impl std::fmt::Display for SignatureType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SignatureType::Prim(prim) => write!(f, "{prim}"),
            SignatureType::GenericVarFromType(index) => write!(f, "_T{index}"),
            SignatureType::GenericVarFromMethod(index) => {
                write!(f, "_M{index}")
            }
            SignatureType::SizeArray(ty) => write!(f, "{ty}[]"),
            SignatureType::MultiDimArray {
                element_type, rank, ..
            } => {
                write!(f, "{element_type}[")?;
                for _ in 0..rank.saturating_sub(1) {
                    write!(f, ",")?;
                }
                write!(f, "]")?;
                Ok(())
            }
            SignatureType::Class { index, metadata }
            | SignatureType::ValueType { index, metadata } => {
                write!(
                    f,
                    "{}",
                    metadata.get(*index).unwrap().full_name().unwrap()
                )
            }
            SignatureType::GenericInst {
                index,
                type_args,
                metadata,
                ..
            } => {
                write!(
                    f,
                    "{}<",
                    metadata.get(*index).unwrap().name().unwrap()
                )?;
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

impl std::fmt::Display for SignaturePrimType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            SignaturePrimType::Bool => "bool",
            SignaturePrimType::Char => "char",
            SignaturePrimType::I8 => "i8",
            SignaturePrimType::U8 => "u8",
            SignaturePrimType::I16 => "i16",
            SignaturePrimType::U16 => "u16",
            SignaturePrimType::I32 => "i32",
            SignaturePrimType::U32 => "u32",
            SignaturePrimType::I64 => "i64",
            SignaturePrimType::U64 => "u64",
            SignaturePrimType::F32 => "f32",
            SignaturePrimType::F64 => "f64",
            SignaturePrimType::NativeInt => "isize",
            SignaturePrimType::NativeUInt => "usize",
        };
        write!(f, "{name}")
    }
}

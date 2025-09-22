use dll_unpacker::{
    MetadataCodedIndex, MetadataRow, MetadataTypeDefOrRef, TypeDef,
    TypeDefOrRef,
};
use elsa::FrozenMap;

use dotnet_debugger::{
    CachedReader, DotNetType, SignaturePrimType, SignatureType,
};

use dsl_ir::{
    DSLType, ExprKind, OpIndex, RuntimePrimType, SymbolicGraph, SymbolicType,
    SymbolicValue,
};
use thiserror::Error;

use crate::Error;

#[derive(Error)]
pub enum SignatureInferenceError {
    #[error(
        "Within a local-only context, \
         attempted to infer the type of an .NET expression \
         within a remote process."
    )]
    NoRemoteProcess,

    #[error("Type name '{0}' could not be found in the remote process")]
    UnknownTypeName(String),

    #[error(
        "Access of array by index requires an array, \
         but instead found type '{0}'."
    )]
    InvalidArray(String),

    #[error(
        "Only SignatureType::Class and SignatureType::ValueType \
         can contain fields.  \
         However, attempted access of field '{field}' \
         within type '{ty}'."
    )]
    InvalidFieldAccess { ty: String, field: String },

    #[error(
        "Could not find field '{field}' within \
         TypeDef metadata of '{class}'."
    )]
    NoSuchField { class: String, field: String },

    #[error(
        "Type {ty} depends on generic type argument {index}, \
         but occurs in a context that only provides \
         {num_generics} generic types."
    )]
    NoSuchGenericTypeArgument {
        ty: String,
        index: usize,
        num_generics: usize,
    },

    #[error(
        "Output of ExprKind::PointerCast should be \
             a primitive or a .NET type.  \
             However, instead found type '{0}'"
    )]
    UnexpectedPointerType(String),
}
type SigErr = SignatureInferenceError;

pub struct SignatureInference<'a> {
    opt_reader: Option<CachedReader<'a>>,
    cache: FrozenMap<OpIndex, Box<SignatureType<'a>>>,
}

#[derive(Clone, Copy, Debug)]
enum TypeDefKind {
    Class,
    ValueType,
}

fn prim_type_to_signature<'a>(
    prim_type: RuntimePrimType,
) -> &'static SignatureType<'a> {
    match prim_type {
        RuntimePrimType::Bool => &SignatureType::Prim(SignaturePrimType::Bool),
        RuntimePrimType::Char => &SignatureType::Prim(SignaturePrimType::Char),
        RuntimePrimType::U8 => &SignatureType::Prim(SignaturePrimType::U8),
        RuntimePrimType::U16 => &SignatureType::Prim(SignaturePrimType::U16),
        RuntimePrimType::U32 => &SignatureType::Prim(SignaturePrimType::U32),
        RuntimePrimType::U64 => &SignatureType::Prim(SignaturePrimType::U64),
        RuntimePrimType::NativeUInt => {
            &SignatureType::Prim(SignaturePrimType::NativeUInt)
        }
        RuntimePrimType::I8 => &SignatureType::Prim(SignaturePrimType::I8),
        RuntimePrimType::I16 => &SignatureType::Prim(SignaturePrimType::I16),
        RuntimePrimType::I32 => &SignatureType::Prim(SignaturePrimType::I32),
        RuntimePrimType::I64 => &SignatureType::Prim(SignaturePrimType::I64),
        RuntimePrimType::NativeInt => {
            &SignatureType::Prim(SignaturePrimType::NativeInt)
        }
        RuntimePrimType::F32 => &SignatureType::Prim(SignaturePrimType::F32),
        RuntimePrimType::F64 => &SignatureType::Prim(SignaturePrimType::F64),
        RuntimePrimType::Ptr => todo!(),
    }
}

trait TypeDefMetadataExt<'a> {
    fn type_def_kind(&self) -> Result<TypeDefKind, Error>;

    fn to_signature(&self) -> Result<SignatureType<'a>, Error>;
}
impl<'a> TypeDefMetadataExt<'a> for MetadataRow<'a, TypeDef> {
    fn type_def_kind(&self) -> Result<TypeDefKind, Error> {
        let is_value_type = self
            .extends()?
            .map(|base| -> Result<_, Error> {
                let name = base.full_name()?;
                Ok(name == "System.ValueType" || name == "System.Enum")
            })
            .transpose()?
            .unwrap_or(false);
        Ok(if is_value_type {
            TypeDefKind::ValueType
        } else {
            TypeDefKind::Class
        })
    }

    fn to_signature(&self) -> Result<SignatureType<'a>, Error> {
        let metadata = self.metadata();
        let index: MetadataCodedIndex<TypeDefOrRef> = self.index().into();
        let sig = match self.type_def_kind()? {
            TypeDefKind::Class => SignatureType::Class { index, metadata },
            TypeDefKind::ValueType => {
                SignatureType::ValueType { index, metadata }
            }
        };

        Ok(sig)
    }
}

impl<'a> SignatureInference<'a> {
    pub fn new(reader: Option<CachedReader<'a>>) -> Self {
        let opt_reader = reader;
        Self {
            opt_reader,
            cache: Default::default(),
        }
    }

    fn reader(&self) -> Result<CachedReader<'a>, SignatureInferenceError> {
        self.opt_reader
            .ok_or(SignatureInferenceError::NoRemoteProcess)
    }

    fn lookup_field(
        &self,
        container: &SignatureType<'a>,
        field_name: &str,
    ) -> Result<(SignatureType<'a>, SignatureType<'a>), Error> {
        let type_def_or_ref = match container {
            SignatureType::ValueType { index, metadata }
            | SignatureType::Class { index, metadata }
            | SignatureType::GenericInst {
                index, metadata, ..
            } => metadata.get(*index)?,
            other => {
                return Err(SigErr::InvalidFieldAccess {
                    ty: format!("{other}"),
                    field: field_name.to_string(),
                }
                .into());
            }
        };

        let type_def_or_ref =
            self.reader()?.unwrap_type_ref(type_def_or_ref)?;

        let mut type_def = match type_def_or_ref {
            MetadataTypeDefOrRef::TypeDef(td) => td,
            MetadataTypeDefOrRef::TypeRef(_) => {
                unreachable!("Should be unwrapped by unwrap_type_ref")
            }
            MetadataTypeDefOrRef::TypeSpec(type_spec) => panic!(
                "Signature of generic instance \
                 should be SignatureType::GenericInst, \
                 but instead found a TypeSpec '{}'",
                type_spec.signature()?
            ),
        };

        let mut generics: Option<Vec<SignatureType>> = match container {
            SignatureType::GenericInst { type_args, .. } => {
                Some(type_args.iter().cloned().collect())
            }
            _ => None,
        };

        loop {
            for field in type_def.iter_fields()? {
                if field.name()? != field_name {
                    continue;
                }

                let ty = field.signature()?.field_type()?;
                let field_type = ty.substitute(|index| {
                    generics
                        .as_ref()
                        .and_then(|vec| vec.get(index))
                        .cloned()
                        .ok_or_else(|| SigErr::NoSuchGenericTypeArgument {
                            ty: format!(
                                "{}",
                                field
                                    .signature()
                                    .unwrap()
                                    .field_type()
                                    .unwrap()
                            ),
                            index,
                            num_generics: generics
                                .as_ref()
                                .map(|vec| vec.len())
                                .unwrap_or(0),
                        })
                })?;

                let base_type = type_def.to_signature()?;
                let container_type = if let Some(generics) = generics {
                    base_type.with_type_args(generics)?
                } else {
                    base_type
                };

                return Ok((container_type, field_type));
            }

            let Some(extends) = type_def.extends()? else {
                return Err(SignatureInferenceError::NoSuchField {
                    class: format!("{container}"),
                    field: field_name.to_string(),
                }
                .into());
            };

            let extends = self.reader()?.unwrap_type_ref(extends)?;

            match extends {
                MetadataTypeDefOrRef::TypeDef(base) => {
                    type_def = base;
                    generics = None;
                }
                MetadataTypeDefOrRef::TypeRef(_) => {
                    unreachable!("Should be unwrapped by .unwrap_type_ref")
                }
                MetadataTypeDefOrRef::TypeSpec(type_spec) => {
                    let full_signature = type_spec.signature()?;
                    let sig_with_generics = full_signature.type_spec_type()?;
                    let sig = sig_with_generics.substitute(|index| {
                        generics
                            .as_ref()
                            .and_then(|vec| vec.get(index))
                            .cloned()
                            .ok_or_else(|| SigErr::NoSuchGenericTypeArgument {
                                ty: format!("{full_signature}"),
                                index,
                                num_generics: generics
                                    .as_ref()
                                    .map(|vec| vec.len())
                                    .unwrap_or(0),
                            })
                    })?;

                    match sig {
                        SignatureType::ValueType { .. }
                        | SignatureType::Class { .. } => unreachable!(
                            "Should be represented as TypeRef or TypeDef"
                        ),
                        SignatureType::GenericInst {
                            index,
                            type_args,
                            metadata,
                            ..
                        } => {
                            let type_def_or_ref = metadata.get(index)?;
                            let type_def_or_ref = self
                                .reader()?
                                .unwrap_type_ref(type_def_or_ref)?;
                            type_def = match type_def_or_ref {
                                MetadataTypeDefOrRef::TypeDef(td) => td,
                                MetadataTypeDefOrRef::TypeRef(_) => {
                                    unreachable!("Should be unwrapped by unwrap_type_ref")
                                }
                                MetadataTypeDefOrRef::TypeSpec(type_spec) => panic!(
                                    "Base class '{full_signature}' contained that TypeSpec \
                                     that itself referenced a TypeSpec '{}'",
                                    type_spec.signature()?
                                ),
                            };
                            generics = Some(type_args);
                        }
                        other => todo!(
                            "Handle {other} signature \
                             as parent class during field lookup \
                             of {field_name} in {container}"
                        ),
                    }
                }
            }
        }
    }

    pub fn symbolic_to_signature(
        &self,
        symbolic: &SymbolicType,
    ) -> Result<SignatureType<'a>, Error> {
        Ok(self.reader()?.symbolic_to_signature(symbolic)?)
    }

    pub fn infer_sig<'b>(
        &'b self,
        graph: &SymbolicGraph,
        value: SymbolicValue,
    ) -> Result<Option<&'b SignatureType<'a>>, Error> {
        let op_index = match value {
            SymbolicValue::Result(index) => index,
            SymbolicValue::Const(prim) => {
                return Ok(Some(prim_type_to_signature(prim.runtime_type())))
            }
        };

        if let Some(cached) = self.cache.get(&op_index) {
            return Ok(Some(cached));
        }

        let expr_kind = &graph[op_index].kind;
        let opt_inferred = self.infer_expr_sig(graph, expr_kind)?;
        if let Some(inferred) = opt_inferred {
            self.cache.insert(op_index, Box::new(inferred));
        }
        Ok(self.cache.get(&op_index))
    }

    pub fn infer_expr_sig<'b>(
        &'b self,
        graph: &SymbolicGraph,
        expr_kind: &ExprKind,
    ) -> Result<Option<SignatureType<'a>>, Error> {
        let inferred_type: Option<SignatureType> = match expr_kind {
            ExprKind::StaticField(static_field) => match &static_field.class {
                SymbolicType::Named { module, name } => {
                    let type_def = self.reader()?.find_type_def(
                        &name,
                        module.as_ref().map(|s| s.as_str()),
                    )?;
                    let (_container, field_sig) = self.lookup_field(
                        &SignatureType::Class {
                            index: type_def.index().into(),
                            metadata: type_def.metadata(),
                        },
                        &static_field.field_name,
                    )?;
                    Some(field_sig)
                }
                other => todo!("Handle static field within type {other}"),
            },
            ExprKind::IndexAccess { obj, .. } => {
                let Some(obj_type) = self.infer_sig(graph, *obj)? else {
                    return Ok(None);
                };
                let element_type = match obj_type {
                    SignatureType::MultiDimArray { element_type, .. }
                    | SignatureType::SizeArray(element_type) => {
                        Ok(element_type)
                    }
                    other => Err(SignatureInferenceError::InvalidArray(
                        format!("{other}"),
                    )),
                }?;
                Some(element_type.as_ref().clone())
            }
            ExprKind::FieldAccess { obj, field } => self
                .infer_sig(graph, *obj)?
                .map(|obj_type| -> Result<_, Error> {
                    println!(
                        "Inferring type of field '{field}' within {}",
                        graph.print(*obj)
                    );
                    let (_container_sig, field_sig) =
                        self.lookup_field(obj_type, field)?;
                    Ok(field_sig)
                })
                .transpose()?,

            ExprKind::SymbolicDowncast { ty: symbolic, .. }
            | ExprKind::PointerCast {
                ty:
                    DSLType::DotNet(
                        DotNetType::ValueType {
                            symbolic: Some(symbolic),
                            ..
                        }
                        | DotNetType::Class {
                            symbolic: Some(symbolic),
                            ..
                        },
                    ),
                ..
            }
            | ExprKind::FunctionArg(DSLType::DotNet(
                DotNetType::ValueType {
                    symbolic: Some(symbolic),
                    ..
                }
                | DotNetType::Class {
                    symbolic: Some(symbolic),
                    ..
                },
            )) => Some(self.symbolic_to_signature(symbolic)?),

            ExprKind::FunctionArg(DSLType::Unknown) => None,

            ExprKind::FunctionArg(DSLType::Prim(prim)) => {
                Some(prim_type_to_signature(*prim).clone())
            }

            ExprKind::SimpleReduce {
                initial, reduction, ..
            } => {
                let initial = self.infer_sig(graph, *initial)?;

                let reduction_index = reduction.as_op_index().unwrap();
                let (reduction_param, reduction_output) =
                    if let ExprKind::Function { params, output } =
                        &graph[reduction_index].kind
                    {
                        let param = self.infer_sig(graph, params[0])?;
                        let output = self.infer_sig(graph, *output)?;
                        (param, output)
                    } else {
                        (None, None)
                    };

                initial.or(reduction_param).or(reduction_output).cloned()
            }

            ExprKind::None => None,

            ExprKind::IfElse {
                if_branch,
                else_branch,
                ..
            } => {
                let if_branch = self.infer_sig(graph, *if_branch)?;
                let else_branch = self.infer_sig(graph, *else_branch)?;
                if_branch.or(else_branch).cloned()
            }

            ExprKind::PrimCast { prim_type, .. }
            | ExprKind::ReadPrim { prim_type, .. } => Some((*prim_type).into()),

            ExprKind::PointerCast { ty, .. } => match ty {
                DSLType::Unknown => None,
                DSLType::Prim(prim) => {
                    Some(SignatureType::Ptr(Box::new((*prim).into())))
                }
                DSLType::DotNet(dot_net_type) => match dot_net_type {
                    DotNetType::ValueType { symbolic, .. }
                    | DotNetType::Class { symbolic, .. } => symbolic
                        .as_ref()
                        .map(|sym| self.symbolic_to_signature(sym))
                        .transpose()?,

                    DotNetType::String => Some(SignatureType::String),

                    DotNetType::Array {
                        symbolic_element, ..
                    } => symbolic_element
                        .as_ref()
                        .map(|element| self.symbolic_to_signature(element))
                        .transpose()?
                        .map(|element| {
                            SignatureType::SizeArray(Box::new(element))
                        }),

                    DotNetType::MultiDimArray {
                        symbolic_element,
                        rank,
                        ..
                    } => symbolic_element
                        .as_ref()
                        .map(|element| self.symbolic_to_signature(element))
                        .transpose()?
                        .map(|element| SignatureType::MultiDimArray {
                            element_type: Box::new(element),
                            rank: *rank,
                            fixed_sizes: Vec::new(),
                            lower_bounds: Vec::new(),
                        }),
                },
                other => {
                    return Err(SigErr::UnexpectedPointerType(format!(
                        "{other}"
                    ))
                    .into())
                }
            },

            other => todo!("Infer type of {other:?}"),
        };

        Ok(inferred_type)
    }

    pub fn infer_object_field_sig(
        &self,
        graph: &SymbolicGraph,
        obj: SymbolicValue,
        field: &str,
    ) -> Result<Option<(SignatureType<'a>, SignatureType<'a>)>, Error> {
        let Some(obj_type) = self.infer_sig(graph, obj)? else {
            return Ok(None);
        };

        let (container_sig, field_sig) = self.lookup_field(obj_type, field)?;

        Ok(Some((container_sig, field_sig)))
    }
}

impl std::fmt::Debug for SignatureInferenceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

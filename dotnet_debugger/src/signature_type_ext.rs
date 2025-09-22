use dll_unpacker::{MetadataTypeDefOrRef, SignatureType};

use crate::SymbolicType;

pub trait SignatureTypeExt {
    fn to_symbolic<Func>(&self, type_formatter: &Func) -> SymbolicType
    where
        Func: Fn(usize) -> SymbolicType;

    fn is_value_type(&self) -> bool;
}

impl<'a> SignatureTypeExt for SignatureType<'a> {
    fn to_symbolic<Func>(&self, type_formatter: &Func) -> SymbolicType
    where
        Func: Fn(usize) -> SymbolicType,
    {
        match self {
            SignatureType::GenericInst {
                index,
                metadata,
                type_args,
                ..
            } => {
                let base = match metadata.get(*index).unwrap() {
                    MetadataTypeDefOrRef::TypeDef(row) => {
                        SymbolicType::Metadata {
                            module: metadata.name().unwrap().to_string(),
                            index: row.index().into(),
                        }
                    }
                    MetadataTypeDefOrRef::TypeRef(row) => SymbolicType::named(
                        row.full_name().unwrap(),
                        Some(row.target_dll_name().unwrap().to_string()),
                    ),
                    MetadataTypeDefOrRef::TypeSpec(_) => todo!(),
                };
                base.with_type_args(
                    type_args.iter().map(|ty| ty.to_symbolic(type_formatter)),
                )
            }
            SignatureType::Prim(prim) => {
                SymbolicType::named(format!("{prim}"), None)
            }
            SignatureType::ValueType { index, metadata }
            | SignatureType::Class { index, metadata } => {
                SymbolicType::Metadata {
                    module: metadata.name().unwrap().to_string(),
                    index: (*index).try_into().unwrap(),
                }
            }
            SignatureType::MultiDimArray {
                element_type, rank, ..
            } => {
                let element_type = element_type.to_symbolic(type_formatter);
                SymbolicType::MultiDimArray {
                    element_type: Box::new(element_type),
                    rank: *rank,
                }
            }
            SignatureType::SizeArray(element) => SymbolicType::Array(Box::new(
                element.to_symbolic(type_formatter),
            )),
            SignatureType::GenericVarFromType(i) => type_formatter(*i as usize),
            SignatureType::GenericVarFromMethod(_) => todo!(),
            SignatureType::Object => {
                SymbolicType::named("Object".to_string(), None)
            }
            SignatureType::String => {
                SymbolicType::named("String".to_string(), None)
            }
            SignatureType::Void => {
                SymbolicType::named("Void".to_string(), None)
            }
            SignatureType::Ptr(_) => todo!(),
        }
    }

    fn is_value_type(&self) -> bool {
        match self {
            SignatureType::GenericInst { is_value_type, .. } => *is_value_type,
            SignatureType::ValueType { .. } => true,
            _ => false,
        }
    }
}

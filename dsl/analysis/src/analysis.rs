use dotnet_debugger::{CachedReader, SignatureType};
use dsl_ir::{DSLType, ExprKind, SymbolicGraph, SymbolicType, SymbolicValue};

use crate::{Error, SignatureInference, TypeInference, TypeInferenceError};

/// Structure to hold analyzers used in common by several different
/// rewriters.
///
/// While the rewriters could each have their own copy of the analysis
/// routines, the same results may be cached and used by each
/// rewriter.  Having a common structure allows the analysis routines
/// to be extended with additional checks/validation in the future.
///
/// Which is probably a bit of overengineering given that type
/// inference is currently the only analysis performed.
pub struct Analysis<'a> {
    opt_reader: Option<CachedReader<'a>>,
    type_inference: TypeInference<'a>,
    sig_inference: SignatureInference<'a>,
}

impl<'a> Analysis<'a> {
    pub fn new(reader: impl Into<Option<CachedReader<'a>>>) -> Self {
        let opt_reader = reader.into();
        let type_inference = TypeInference::new(opt_reader);
        let sig_inference = SignatureInference::new(opt_reader);
        Self {
            opt_reader,
            type_inference,
            sig_inference,
        }
    }

    pub fn infer_type<'b>(
        &'b self,
        graph: &SymbolicGraph,
        value: SymbolicValue,
    ) -> Result<&'b DSLType, TypeInferenceError> {
        self.type_inference.infer_type(graph, value)
    }

    /// Given an object, and the name of the field within it, return a
    /// tuple containing the (container_type, field_type).
    ///
    /// Note: The `container_type` is the type of the class that
    /// contains the field, and is not necessarily the same as the
    /// type of `obj`.  If the field is defined within a base class,
    /// and `obj` refers to a derived class, then `container_type`
    /// will be the base class.
    pub fn infer_object_field_sig(
        &self,
        graph: &SymbolicGraph,
        obj: SymbolicValue,
        field: &str,
    ) -> Result<Option<(SignatureType<'a>, SignatureType<'a>)>, Error> {
        self.sig_inference.infer_object_field_sig(graph, obj, field)
    }

    pub fn symbolic_to_signature(
        &self,
        symbolic: &SymbolicType,
    ) -> Result<SignatureType<'a>, Error> {
        self.sig_inference.symbolic_to_signature(symbolic)
    }

    pub fn infer_sig<'b>(
        &'b self,
        graph: &SymbolicGraph,
        value: SymbolicValue,
    ) -> Result<Option<&'b SignatureType<'a>>, Error> {
        self.sig_inference.infer_sig(graph, value)
    }

    pub fn infer_expr_sig<'b>(
        &'b self,
        graph: &SymbolicGraph,
        expr: &ExprKind,
    ) -> Result<Option<SignatureType<'a>>, Error> {
        self.sig_inference.infer_expr_sig(graph, expr)
    }

    pub fn reader(&self) -> Result<CachedReader<'a>, Error> {
        Ok(self.opt_reader.unwrap())
    }
}

use dsl_ir::{DSLType, ExprKind, FunctionType, SymbolicGraph, SymbolicValue};

pub trait CopyFirstParamExt {
    fn copy_first_param(&mut self, func: SymbolicValue) -> SymbolicValue;
}
impl CopyFirstParamExt for SymbolicGraph {
    fn copy_first_param(&mut self, func: SymbolicValue) -> SymbolicValue {
        let SymbolicValue::Result(func) = func else {
            panic!(
                "Internal error, \
                 SymbolicValue should point to function \
                 for copy_first_param"
            )
        };
        let params = match &self[func].kind {
            ExprKind::Function { params, .. } => params,
            ExprKind::NativeFunction(func) => {
                let sig = func.signature().unwrap();
                match sig {
                    DSLType::Function(FunctionType { params, .. }) => {
                        let param_ty = params
                            .map(|params| params[0].clone())
                            .unwrap_or(DSLType::Unknown);
                        return self.function_arg(param_ty);
                    }
                    _ => panic!(
                        "Internal error, \
                         NativeFunction should return TunctionType"
                    ),
                }
            }
            _ => panic!(
                "Internal error, \
                 SymbolicValue should point to function \
                 for copy_first_param"
            ),
        };

        let SymbolicValue::Result(first_param_index) = params[0] else {
            panic!(
                "Internal error, \
                 All function parameters \
                 should point to FunctionArg"
            )
        };

        let first_param = &self[first_param_index];

        let ExprKind::FunctionArg(param_ty) = &first_param.kind else {
            panic!(
                "Internal error, \
                 All function parameters \
                 should point to FunctionArg"
            )
        };

        let opt_name = first_param.name.clone();
        let new_param = self.function_arg(param_ty.clone());
        if let Some(name) = opt_name {
            self.name(new_param, name).expect(
                "Internal error, \
                 Existing name must already be valid",
            );
        }

        new_param
    }
}

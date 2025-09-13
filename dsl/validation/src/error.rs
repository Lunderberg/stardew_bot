use thiserror::Error;

use dsl_ir::OpIndex;

#[derive(Error)]
pub enum Error {
    #[error("dsl::analysis::TypeInferenceError( {0} )")]
    TypeInferenceError(#[from] dsl_analysis::TypeInferenceError),

    #[error("Invalid reference from expression {from} to {to}")]
    InvalidReference { from: OpIndex, to: OpIndex },

    #[error(
        "Parameters must have a unique owner, \
             but the parameter at {param} is used by \
             the functions at {first_owner} and {second_owner}."
    )]
    MultipleFunctionsOwnSameParam {
        param: OpIndex,
        first_owner: OpIndex,
        second_owner: OpIndex,
    },

    #[error(
        "The function parameter {param} was used, \
         but did not have a definition."
    )]
    UseOfUndefinedParam { param: OpIndex },
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

use thiserror::Error;

#[derive(Error)]
pub enum Error {
    #[error("dsl::ir::Error( {0} )")]
    DslIr(#[from] dsl_ir::Error),
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

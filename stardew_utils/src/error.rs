use thiserror::Error;

#[derive(Error)]
pub enum Error {
    #[error("Stardew Valley process not running")]
    StardewNotRunning,
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

use thiserror::Error;

#[derive(Error)]
pub enum BotError {
    #[error("No route to target tile")]
    NoRouteToTarget,
}

impl std::fmt::Debug for BotError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, LazyLock, Once};

static INIT: Once = Once::new();
static CLOSE_ON_SIGTERM: LazyLock<Arc<AtomicBool>> =
    LazyLock::new(|| Arc::new(AtomicBool::new(false)));

pub struct SigintHandler;

impl Default for SigintHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl SigintHandler {
    pub fn new() -> Self {
        INIT.call_once(|| {
            let flag = &*CLOSE_ON_SIGTERM;
            signal_hook::flag::register_conditional_default(
                signal_hook::consts::SIGINT,
                flag.clone(),
            )
            .unwrap();
            signal_hook::flag::register(
                signal_hook::consts::SIGINT,
                flag.clone(),
            )
            .unwrap();
        });

        let flag = &*CLOSE_ON_SIGTERM;
        flag.store(false, Ordering::SeqCst);

        Self
    }

    pub fn received(&self) -> bool {
        let flag = &*CLOSE_ON_SIGTERM;
        flag.load(Ordering::SeqCst)
    }
}

// Set the value to true on drop, so that any further SIGINT events
// act as if the default handler is still in effect.
impl std::ops::Drop for SigintHandler {
    fn drop(&mut self) {
        let flag = &*CLOSE_ON_SIGTERM;
        flag.store(true, Ordering::SeqCst);
    }
}

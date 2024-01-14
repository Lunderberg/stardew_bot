use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Once};

static INIT: Once = Once::new();
static mut CLOSE_ON_SIGTERM: Option<Arc<AtomicBool>> = None;

pub struct SigintHandler;

impl Default for SigintHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl SigintHandler {
    pub fn new() -> Self {
        INIT.call_once(|| {
            let flag = Arc::new(AtomicBool::new(false));
            unsafe {
                CLOSE_ON_SIGTERM = Some(flag.clone());
            }
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

        let flag = unsafe { CLOSE_ON_SIGTERM.as_ref().unwrap() };
        flag.store(false, Ordering::SeqCst);

        Self
    }

    pub fn received(&self) -> bool {
        let flag = unsafe { CLOSE_ON_SIGTERM.as_ref().unwrap() };
        flag.load(Ordering::SeqCst)
    }
}

// Set the value to true on drop, so that any further SIGINT events
// act as if the default handler is still in effect.
impl std::ops::Drop for SigintHandler {
    fn drop(&mut self) {
        let flag = unsafe { CLOSE_ON_SIGTERM.as_ref().unwrap() };
        flag.store(true, Ordering::SeqCst);
    }
}

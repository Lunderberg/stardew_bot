use std::cell::OnceCell;

pub trait OrTryInit {
    type T;

    /// Initialize the value if it isn't already filled, using the
    /// fallible function provided.  If an error occurs during
    /// initialization, the cell is left unfilled and the error is
    /// returned.
    fn or_try_init<Func, Err>(&self, func: Func) -> Result<&Self::T, Err>
    where
        Func: FnOnce() -> Result<Self::T, Err>;
}

impl<T> OrTryInit for OnceCell<T> {
    type T = T;

    fn or_try_init<Func, Err>(&self, func: Func) -> Result<&Self::T, Err>
    where
        Func: FnOnce() -> Result<Self::T, Err>,
    {
        Ok(if let Some(value) = self.get() {
            value
        } else {
            let value = func()?;
            let _ = self.set(value);
            self.get().unwrap()
        })
    }
}

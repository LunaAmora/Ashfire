use anyhow::Result;

pub struct OptionErr<T> (pub Result<Option<T>>);

impl<T> From<Result<Option<T>>> for OptionErr<T> {
    fn from(opt: Result<Option<T>>) -> Self {
        Self(opt)
    }
}

impl<T> OptionErr<T> {
    pub fn or_try<F>(mut self, f: F) -> OptionErr<T>
    where
        F: FnOnce() -> Result<Option<T>>,
    {
        if let Ok(None) = self.0 {
            self.0 = f();
        }
        self
    }

    pub fn or_else<F>(mut self, f: F) -> OptionErr<T>
    where
        F: FnOnce() -> Option<T>,
    {
        if let Ok(None) = self.0 {
            self.0 = Ok(f());
        }
        self
    }
}

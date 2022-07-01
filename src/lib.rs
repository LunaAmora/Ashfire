use anyhow::Result;

pub struct OptionErr<T> (Result<Option<T>>);

impl<T> From<OptionErr<T>> for Result<Option<T>> {
    fn from(val: OptionErr<T>) -> Self {
        val.0
    }
}

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

    pub fn or<F>(self, f: F) -> Result<Option<T>>
    where
        F: FnOnce() -> T,
    {
        match self.0 {
            Ok(None) => Ok(Some(f())),
            _ => self.0,
        }
    }
}

use anyhow::Result;

pub struct OptionErr<T> (pub Result<Option<T>>);

impl<T> From<Result<Option<T>>> for OptionErr<T> {
    fn from(opt: Result<Option<T>>) -> Self {
        Self(opt)
    }
}

impl<T> OptionErr<T> {
    pub fn or_try<F>(self, f: F) -> OptionErr<T>
    where
        F: FnOnce() -> Result<Option<T>>,
    {
        match self.0 {
            Ok(Some(x)) => Self(Ok(Some(x))),
            Ok(None) => Self(f()),
            Err(err) => Self(Err(err)),
        }
    }

    pub fn or_else<F>(self, f: F) -> OptionErr<T>
    where
        F: FnOnce() -> Option<T>,
    {
        match self.0 {
            Ok(Some(x)) => Self(Ok(Some(x))),
            Ok(None) => Self(Ok(f())),
            Err(err) => Self(Err(err)),
        }
    }
}

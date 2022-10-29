use std::{
    convert::Infallible,
    ops::{ControlFlow, FromResidual, Try},
};

pub type Formatter<T> = dyn Fn(T) -> String;
pub type LazyResult<'a, R, T> = Result<R, LazyError<'a, T>>;

pub struct LazyError<'a, T>(Box<dyn Fn(&'a Formatter<T>) -> Result<Infallible, anyhow::Error>>);

impl<'a, T> LazyError<'a, T> {
    pub fn new<E>(lazy_error: E) -> Self
    where
        E: 'static + Fn(&'a Formatter<T>) -> Result<Infallible, anyhow::Error>,
    {
        Self(Box::new(lazy_error))
    }

    pub fn apply(&self, formatter: &'a Formatter<T>) -> anyhow::Error {
        self.0(formatter).unwrap_err()
    }
}

pub trait LazyCtx<'a, T: 'a>: private::Sealed {
    fn with_ctx<X>(
        self, lazy_error: Result<Infallible, LazyError<T>>,
    ) -> Result<Self::Internal, LazyError<T>>
    where
        X: FromResidual<Result<Infallible, LazyError<'a, T>>>;

    fn try_or_apply(self, formatter: &'a Formatter<T>) -> anyhow::Result<Self::Internal>
    where
        Self: Try<Residual = Result<Infallible, LazyError<'a, T>>, Output = Self::Internal>;
}

impl<'a, A: private::Sealed, T: 'a> LazyCtx<'a, T> for A {
    fn with_ctx<X>(
        self, lazy_error: Result<Infallible, LazyError<T>>,
    ) -> Result<Self::Internal, LazyError<T>>
    where
        X: FromResidual<Result<Infallible, LazyError<'a, T>>>,
    {
        match self.value() {
            Some(val) => Ok(val),
            None => Err(lazy_error.unwrap_err()),
        }
    }

    fn try_or_apply(self, formatter: &'a Formatter<T>) -> anyhow::Result<Self::Internal>
    where
        Self: Try<Residual = Result<Infallible, LazyError<'a, T>>, Output = Self::Internal>,
    {
        match self.branch() {
            ControlFlow::Continue(output) => Ok(output),
            ControlFlow::Break(lazy) => Err(lazy.unwrap_err().apply(formatter)),
        }
    }
}

#[macro_export]
macro_rules! lazybail {
    ( |$i:pat_param| $($fmt:expr ),* ) => {
        Err($crate::lazy::LazyError::new(move |$i| {
            Err($crate::anyhow::anyhow!( $( $fmt ),* ))
        }))
    };
}

pub(crate) mod private {
    use anyhow::Result;

    use super::LazyResult;

    pub trait Sealed {
        type Internal;
        fn value(self) -> Option<Self::Internal>;
    }

    impl<T> Sealed for Result<T> {
        type Internal = T;

        fn value(self) -> Option<T> {
            self.ok()
        }
    }

    impl<R, T> Sealed for LazyResult<'_, R, T> {
        type Internal = R;

        fn value(self) -> Option<R> {
            self.ok()
        }
    }

    impl<T> Sealed for Option<T> {
        type Internal = T;

        fn value(self) -> Option<T> {
            self
        }
    }

    impl Sealed for bool {
        type Internal = bool;

        fn value(self) -> Option<bool> {
            self.then_some(true)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{LazyCtx, LazyResult};

    #[test]
    fn lazybail_test() {
        let err = lazybail_error(69).try_or_apply(&int_error_fmt).unwrap_err();

        assert_eq!("Error: `69`", err.to_string())
    }

    #[test]
    fn ctx_test() {
        let err = with_ctx_error(None, 420)
            .try_or_apply(&int_error_fmt)
            .unwrap_err();

        assert_eq!("Error: `420`", err.to_string())
    }

    #[test]
    fn ctx_ok_test() {
        with_ctx_error(Some(true), 0)
            .try_or_apply(&int_error_fmt)
            .unwrap();
    }

    fn with_ctx_error<'a>(option: Option<bool>, diagnostic: i32) -> LazyResult<'a, bool, i32> {
        let unwraped_option: bool =
            option.with_ctx::<LazyResult<bool, i32>>(lazybail!(|fmt| "{}", fmt(diagnostic)))?;

        Ok(unwraped_option)
    }

    fn lazybail_error<'a>(i: i32) -> LazyResult<'a, bool, i32> {
        lazybail!(|fmt| "{}", fmt(i))?;
        Ok(true)
    }

    fn int_error_fmt(i: i32) -> String {
        format!("Error: `{}`", i)
    }
}

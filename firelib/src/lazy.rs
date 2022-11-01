use std::{
    convert::Infallible,
    fmt::{Debug, Display},
    ops::{ControlFlow, Try},
};

use anyhow::{Error, Result};

pub type LazyResult<R, T> = Result<R, LazyError<T>>;

pub trait Formatter<T> {
    fn apply(&self, t: T) -> String;
}

impl<T, A: Fn(T) -> String> Formatter<T> for A {
    fn apply(&self, t: T) -> String {
        self(t)
    }
}

pub struct LazyError<T>(Box<dyn for<'a> Formatter<&'a dyn Formatter<T>>>);

impl<T> LazyError<T> {
    pub fn new(lazy_error: impl Fn(&dyn Formatter<T>) -> String + 'static) -> Self {
        LazyError(Box::new(lazy_error))
    }

    pub fn apply(&self, formatter: &dyn Formatter<T>) -> Error {
        anyhow::anyhow!(self.0.apply(formatter))
    }

    pub fn skip(&self) -> Error {
        anyhow::anyhow!(self.0.apply(&|_| String::new()))
    }
}

impl<T> Display for LazyError<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.skip())
    }
}

impl<T> Debug for LazyError<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("LazyError").field(&self.skip()).finish()
    }
}

impl<T> std::error::Error for LazyError<T> {}
unsafe impl<T> Send for LazyError<T> {}
unsafe impl<T> Sync for LazyError<T> {}

pub trait LazyCtx<T>: private::Sealed {
    fn with_ctx(
        self, lazy_error: impl Fn(&dyn Formatter<T>) -> String + 'static,
    ) -> Result<Self::Internal, LazyError<T>>;

    fn try_or_apply(self, format: &dyn Formatter<T>) -> Result<Self::Internal>
    where
        Self: Try<Residual = Result<Infallible, LazyError<T>>, Output = Self::Internal>;
}

impl<A: private::Sealed, T> LazyCtx<T> for A {
    fn with_ctx(
        self, lazy_error: impl Fn(&dyn Formatter<T>) -> String + 'static,
    ) -> Result<Self::Internal, LazyError<T>> {
        match self.value() {
            Some(output) => Ok(output),
            None => Err(LazyError::new(lazy_error)),
        }
    }

    fn try_or_apply(self, format: &dyn Formatter<T>) -> Result<Self::Internal>
    where
        Self: Try<Residual = Result<Infallible, LazyError<T>>, Output = Self::Internal>,
    {
        match self.branch() {
            ControlFlow::Continue(output) => Ok(output),
            ControlFlow::Break(lazy) => Err(lazy.unwrap_err().apply(format)),
        }
    }
}

#[macro_export]
macro_rules! lazybail {
    ( |$i:pat_param| $($fmt:expr ),* ) => {
        Err($crate::lazy::LazyError::new(move |$i| {
            format!( $( $fmt ),* )
        }))?
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

    impl<R, T> Sealed for LazyResult<R, T> {
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
    use super::{LazyCtx, LazyError, LazyResult};

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

    fn with_ctx_error(option: Option<bool>, diagnostic: i32) -> LazyResult<bool, i32> {
        let unwraped_option: bool = option.with_ctx(move |f| format!("{}", f.apply(diagnostic)))?;

        Ok(unwraped_option)
    }

    fn lazybail_error(i: i32) -> LazyResult<bool, i32> {
        lazybail!(|f| "{}", f.apply(i))
    }

    fn int_error_fmt(i: i32) -> String {
        format!("Error: `{}`", i)
    }

    #[test]
    fn lifetimes_test() {
        let a = format_mutate_format(&create_cls(34), &mut Program(25));
        let b = format_mutate_format(&create_cls(40), &mut Program(19));
        assert_eq!(a, b);
    }

    fn format_mutate_format(cls: &LazyError<i32>, program: &mut Program) -> String {
        let first = cls.apply(&|i| program.format(i));
        program.plus(10);
        let second = cls.apply(&|i| program.format(i));

        format!("{first}. {second}")
    }

    fn create_cls<T: Copy + 'static>(value: T) -> LazyError<T> {
        LazyError::new(move |f| format!("Value: {}", f.apply(value)))
    }

    struct Program(i32);

    impl Program {
        fn format(&self, i: i32) -> String {
            format!("`{}`", self.0 + i)
        }

        fn plus(&mut self, i: i32) {
            self.0 += i;
        }
    }
}

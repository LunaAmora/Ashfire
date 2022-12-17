use std::ops::{FromResidual, Try};

/// A trait for giving a type [`choice`][crate::choice!] macro support.
pub trait Alternative: Try + Sized
where
    Self: From<Self::ChoiceOutput> + FromResidual<Self::ChoiceResidual>,
    Self::ChoiceOutput: Default,
{
    type ChoiceOutput;
    type ChoiceResidual;
}

/// Chain [`Try::branch`][std::ops::Try::branch] calls,
/// returning early when a valid value for the given
/// type is found.
///
/// # See also
///
/// The choice function for Alternatives in haskell.
///
/// # Examples
///
///```
/// # #![feature(try_trait_v2)]
/// # #![feature(try_blocks)]
/// # use firelib::{choice, alternative, choice::Alternative};
/// // Here we create an struct that implements the Alternative
/// // trait with a neutral matching pattern of `value: None`.
/// #[alternative(value, None)]
/// struct Alter<T> {
///     pub value: Option<T>,
/// }
///
/// impl<T> Alternative for Alter<T> {
///     type ChoiceOutput = Option<T>;
///     type ChoiceResidual = Option<T>;
/// }
///
/// # impl<T> From<T> for Alter<T> {
/// #     fn from(value: T) -> Self {
/// #         Self { value: Some(value) }
/// #     }
/// # }
/// # impl<T> From<Option<T>> for Alter<T> {
/// #     fn from(value: Option<T>) -> Self {
/// #         Self { value }
/// #     }
/// # }
/// # fn usize_with_side_effect() -> usize {
/// #     panic!()
/// # }
/// // Next we can use the `choice` macro with the name of the
/// // struct preceding by any value or fn call that can be
/// // converted to it via `From`.
/// // Note: A basic impl of `From<T>` and `From<Option<T>>` are
/// // given, but hidden in this example for the sake of brevity.
/// fn choose() -> Alter<usize> {
///     choice!(Alter, None, 1, usize_with_side_effect())
/// }
///
/// // The `choose` fn will then lazily try each of its alternatives,
/// // returning early on the first non neutral-matching value.
/// // Note: As `1` is the value found as an alternative,
/// // `usize_with_side_effect` is never called, because `choice` is lazy.
/// fn main() {
///     assert_eq!(choose().value, Some(1));
/// }
/// ```
#[macro_export]
macro_rules! choice {
    ($typ:ident, $x:expr, $( $xs:expr ),* $(,)?) => {{
        let mut alternative = $typ::from($x);
        alternative = try {
            alternative? $(;
            $typ::from($xs)?)*
        };
        alternative
    }};
}

#[cfg(test)]
mod tests {
    use std::{convert::Infallible, fmt::Display, ops::FromResidual};

    use anyhow::{Context, Error, Result};

    use crate::{
        self as firelib, alternative, choice,
        choice::Alternative,
        lazy::{self, LazyErrCtx},
        FlowControl,
    };

    type LazyResult<R, T> = lazy::LazyResult<'static, R, T>;

    #[test]
    fn test1() {
        assert_eq!("Some: 420", choice1().to_string());
    }

    #[test]
    fn test2() {
        assert_eq!("[Error] choice2", choice2().to_string());
    }

    #[test]
    fn test3() {
        assert_eq!("None", choice3().to_string());
    }

    #[test]
    fn test4() {
        assert_eq!("[Error] choice4 (lazy_ctx)\n[Error] choice2", choice4().to_string());
    }

    fn choice1() -> OptionErr<i32, ()> {
        choice!(OptionErr, None, Some(420), choice2())
    }

    fn choice2() -> OptionErr<i32, ()> {
        choice!(OptionErr, None, anyhow::anyhow!("[Error] choice2"))
    }

    fn choice3() -> OptionErr<i32, ()> {
        choice!(OptionErr, None, None, None, None)
    }

    fn choice4() -> OptionErr<i32, ()> {
        let val = choice!(OptionErr, choice3(), choice2())
            .value
            .with_ctx(|_| "[Error] choice4 (lazy_ctx)".to_owned())?
            .with_context(|| "[Error] choice4 (anyhow)".to_owned())?;

        OptionErr::new(val)
    }

    #[derive(FlowControl)]
    #[alternative(value, Ok(None))]
    struct OptionErr<T, E> {
        value: LazyResult<Option<T>, E>,
    }

    impl<T, E> Alternative for OptionErr<T, E> {
        type ChoiceOutput = Option<T>;
        type ChoiceResidual = LazyResult<Option<T>, E>;
    }

    impl<T, E> OptionErr<T, E> {
        fn new(value: T) -> Self {
            Self { value: Ok(Some(value)) }
        }
    }

    impl<T, E> FromResidual<LazyResult<Infallible, E>> for OptionErr<T, E> {
        fn from_residual(residual: LazyResult<Infallible, E>) -> Self {
            match residual {
                Err(lazy) => Self { value: Err(lazy) },
                Ok(_) => unreachable!(),
            }
        }
    }

    impl<T, E> From<Error> for OptionErr<T, E> {
        fn from(err: Error) -> Self {
            Self { value: Err(err.into()) }
        }
    }

    impl<T, E> From<LazyResult<Option<T>, E>> for OptionErr<T, E> {
        fn from(value: LazyResult<Option<T>, E>) -> Self {
            Self { value }
        }
    }

    impl<T, E> From<Result<Option<T>>> for OptionErr<T, E> {
        fn from(value: Result<Option<T>>) -> Self {
            match value {
                Ok(ok) => Self::from(ok),
                Err(err) => Self::from(err),
            }
        }
    }

    impl<T, E> From<Option<T>> for OptionErr<T, E> {
        fn from(value: Option<T>) -> Self {
            Self { value: Ok(value) }
        }
    }

    impl<T: Display, E> Display for OptionErr<T, E> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match &self.value {
                Ok(Some(value)) => write!(f, "Some: {value}"),
                Ok(None) => write!(f, "None"),
                Err(err) => write!(f, "{err}"),
            }
        }
    }
}

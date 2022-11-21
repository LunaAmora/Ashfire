use std::ops::Deref;

use ashlib::UncheckedStack;
use firelib::{lazy::LazyFormatter, lexer::Loc};
use itertools::Itertools;

use crate::compiler::{
    program::{Fmt, LazyError, LazyResult},
    types::{
        core::{Location, TokenType, Typed},
        data::{Data, Value},
    },
};

pub enum ArityType<T: Copy> {
    Any,
    Same,
    Type(T),
}

/// Safe wrapper for [`UncheckedStack`] with proper errors messages.
pub trait Expect<T: Clone + Typed + Location + 'static>: UncheckedStack<T> {
    fn expect_exact_pop(&mut self, contract: &[TokenType], loc: Loc) -> LazyResult<()> {
        self.expect_stack_size(contract.len(), loc)?;
        self.expect_exact(contract, loc)?;
        self.truncate(contract.len());
        Ok(())
    }

    fn expect_contract_pop(&mut self, contract: &[TokenType], loc: Loc) -> LazyResult<()> {
        self.expect_stack_size(contract.len(), loc)?;
        self.expect_arity(contract, loc)?;
        self.truncate(contract.len());
        Ok(())
    }

    fn expect_array_pop<const N: usize>(
        &mut self, contract: [TokenType; N], loc: Loc,
    ) -> LazyResult<[T; N]> {
        self.expect_stack_size(contract.len(), loc)?;
        self.expect_arity(&contract, loc)?;
        Ok(unsafe { self.pop_array() })
    }

    fn expect_arity_pop<const N: usize>(
        &mut self, arity: ArityType<TokenType>, loc: Loc,
    ) -> LazyResult<[T; N]> {
        self.expect_arity_type::<N>(arity, loc)?;
        Ok(unsafe { self.pop_array() })
    }

    fn expect_peek(&mut self, arity_t: ArityType<TokenType>, loc: Loc) -> LazyResult<&T> {
        self.expect_arity_type::<1>(arity_t, loc)?;
        Ok(unsafe { self.peek() })
    }

    fn expect_pop(&mut self, loc: Loc) -> LazyResult<T> {
        self.expect_stack_size(1, loc)?;
        Ok(unsafe { self.pop() })
    }

    fn expect_pop_n<const N: usize>(&mut self, loc: Loc) -> LazyResult<[T; N]> {
        self.expect_stack_size(N, loc)?;
        Ok(unsafe { self.pop_array() })
    }

    fn expect_pop_type(&mut self, arity_t: TokenType, loc: Loc) -> LazyResult<T> {
        self.expect_arity_type::<1>(ArityType::Type(arity_t), loc)?;
        Ok(unsafe { self.pop() })
    }

    fn expect_arity_type<const N: usize>(
        &self, arity: ArityType<TokenType>, loc: Loc,
    ) -> LazyResult<()> {
        self.expect_stack_size(N, loc)?;

        let typ = match arity {
            ArityType::Any => return Ok(()),
            ArityType::Same => unsafe { self.get_from_top(0) }.get_type(),
            ArityType::Type(typ) => typ,
        };

        self.expect_arity(&[typ; N], loc)?;
        Ok(())
    }

    fn expect_stack_size(&self, n: usize, loc: Loc) -> LazyResult<()> {
        let len = self.len();
        if len < n {
            lazybail!(
                |f| concat!(
                    "Stack has less elements than expected\n",
                    "[INFO] {}Expected `{}` elements, but found `{}`"
                ),
                f.format(Fmt::Loc(loc)),
                n,
                len
            );
        };
        Ok(())
    }

    fn pop_push<const N: usize>(
        &mut self, contr: [TokenType; N], to_push: T, loc: Loc,
    ) -> LazyResult<()> {
        self.expect_array_pop(contr, loc)?;
        self.push(to_push);
        Ok(())
    }

    fn pop_push_arity<const N: usize, F>(
        &mut self, f: F, arity_t: ArityType<TokenType>, loc: Loc,
    ) -> LazyResult<()>
    where
        F: Fn([T; N]) -> T,
    {
        let popped = self.expect_arity_pop(arity_t, loc)?;
        self.push(f(popped));
        Ok(())
    }

    fn pop_extend<const N: usize, const M: usize, F>(&mut self, f: F, loc: Loc) -> LazyResult<()>
    where
        F: Fn([T; N]) -> [T; M],
    {
        let popped = self.expect_pop_n(loc)?;
        self.extend(f(popped));
        Ok(())
    }
}

impl<T: Clone + Typed + Location + 'static, X: UncheckedStack<T> + ?Sized> Compare<T> for X {}

pub trait Compare<T: Clone + Typed + Location + 'static>: Deref<Target = [T]> {
    fn expect_exact<V: Typed>(&self, contract: &[V], loc: Loc) -> LazyResult<()> {
        if self.len() == contract.len() && self.expect_arity(contract, loc).is_ok() {
            return Ok(());
        }
        Err(self.format_stack_diff(contract, loc))
    }

    fn expect_arity<V: Typed>(&self, contract: &[V], loc: Loc) -> LazyResult<()> {
        for (stk, contract) in self.iter().rev().zip(contract.iter().rev()) {
            expect_type(stk, contract, loc)?;
        }
        Ok(())
    }

    fn format_stack_diff<V: Typed>(&self, contract: &[V], loc: Loc) -> LazyError {
        let contract = format_stack(contract);
        let stack = format_stack(self);
        let frame = format_frames(self);

        LazyError::new(move |f| {
            format!(
                concat!(
                    "Found stack at the end of the context does not match the expected types:\n",
                    "[INFO] {}Expected types: {}\n",
                    "[INFO] {}Actual types:   {}\n{}"
                ),
                f.format(Fmt::Loc(loc)),
                contract.apply(f),
                f.format(Fmt::Loc(loc)),
                stack.apply(f),
                frame.apply(f)
            )
        })
    }
}

fn expect_type<T: Clone + Typed + Location + 'static, V: Typed>(
    frame: &T, expected: V, loc: Loc,
) -> LazyResult<()> {
    if equals_any!(expected.get_type(), Value::Any, Data::Ptr(Value::Any), frame.get_type()) {
        return Ok(());
    }
    Err(format_type_diff(frame.clone(), expected.get_type(), loc))
}

fn format_type_diff<T: Clone + Typed + Location + 'static>(
    frame: T, expected: TokenType, loc: Loc,
) -> LazyError {
    LazyError::new(move |f| {
        format!(
            "{}Expected type `{}`, but found `{}`\n{}",
            f.format(Fmt::Loc(loc)),
            f.format(Fmt::Typ(expected)),
            f.format(Fmt::Typ(frame.get_type())),
            format_frame(&frame).apply(f)
        )
    })
}

pub fn format_frames<T: Clone + Typed + Location>(stack: &[T]) -> impl LazyFormatter<Fmt> {
    let copied = stack.to_vec();
    lazyformat! { |f| copied.iter().map(|t| format_frame(t).apply(f)).join("\n") }
}

pub fn format_frame<T: Typed + Location>(t: T) -> impl LazyFormatter<Fmt> {
    lazyformat! { |f|
        format!(
            "[INFO] {}Type `{}` was declared here",
            f.format(Fmt::Loc(t.loc())),
            f.format(Fmt::Typ(t.get_type()))
        )
    }
}

pub fn format_stack<T: Typed>(stack: &[T]) -> impl LazyFormatter<Fmt> + 'static {
    let types: Vec<TokenType> = stack.iter().map(Typed::get_type).collect();
    lazyformat! { |f|
        format!(
            "[{}] ->",
            types
                .iter()
                .map(|&t| format!("<{}>", f.format(Fmt::Typ(t))))
                .join(", ")
        )
    }
}

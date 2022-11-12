use std::ops::Deref;

use ashlib::Stack;
use firelib::{lazy::LazyFormatter, lexer::Loc};
use itertools::Itertools;

use super::{
    program::{Fmt, LazyError, LazyResult},
    types::*,
};

pub enum ArityType<T: Copy> {
    Any,
    Same,
    Type(T),
}

pub trait Expect<T: Clone + Typed + Location + 'static>: Stack<T> {
    fn expect_exact_pop(&mut self, contract: &[TokenType], loc: Loc) -> LazyResult<Vec<T>> {
        self.expect_stack_size(contract.len(), loc)?;
        self.expect_exact(contract, loc)?;
        self.pop_n(contract.len())
    }

    fn expect_contract_pop(&mut self, contr: &[TokenType], loc: Loc) -> LazyResult<Vec<T>> {
        self.expect_stack_size(contr.len(), loc)?;
        self.expect_arity(contr, loc)?;
        self.pop_n(contr.len())
    }

    fn expect_array_pop<const N: usize>(
        &mut self, contr: [TokenType; N], loc: Loc,
    ) -> LazyResult<[T; N]> {
        self.expect_stack_size(contr.len(), loc)?;
        self.expect_arity(&contr, loc)?;
        self.pop_array()
    }

    fn expect_arity_pop<const N: usize>(
        &mut self, arity: ArityType<TokenType>, loc: Loc,
    ) -> LazyResult<[T; N]> {
        self.expect_arity_type(N, arity, loc)?;
        self.pop_array()
    }

    fn expect_peek(&mut self, arity_t: ArityType<TokenType>, loc: Loc) -> LazyResult<&T> {
        self.expect_arity_type(1, arity_t, loc)?;
        Ok(self.peek().unwrap())
    }

    fn expect_pop(&mut self, loc: Loc) -> LazyResult<T> {
        self.expect_stack_size(1, loc)?;
        Ok(self.pop().unwrap())
    }

    fn expect_pop_n<const N: usize>(&mut self, loc: Loc) -> LazyResult<[T; N]> {
        self.expect_stack_size(N, loc)?;
        self.pop_array()
    }

    fn expect_pop_type(&mut self, arity_t: TokenType, loc: Loc) -> LazyResult<T> {
        self.expect_arity_type(1, ArityType::Type(arity_t), loc)?;
        Ok(self.pop().unwrap())
    }

    fn expect_arity_type(&self, n: usize, arity: ArityType<TokenType>, loc: Loc) -> LazyResult<()> {
        self.expect_stack_size(n, loc)?;

        let (typ, start) = match arity {
            ArityType::Any => return Ok(()),
            ArityType::Same => (self.get_from(0).cloned().unwrap().get_type(), 1),
            ArityType::Type(typ) => (typ, 0),
        };

        for i in start..n {
            expect_type(self.get_from(i).unwrap(), typ, loc)?;
        }

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
            )
        };
        Ok(())
    }
}

impl<T: Clone + Typed + Location + 'static, X: Stack<T> + ?Sized> Compare<T> for X {}

pub trait Compare<T: Clone + Typed + Location + 'static>: Deref<Target = [T]> {
    fn expect_exact<V: Typed>(&self, contract: &[V], loc: Loc) -> LazyResult<()> {
        if !(self.len() == contract.len() && self.expect_arity(&contract, loc).is_ok()) {
            Err(self.format_stack_diff(contract, loc))
        } else {
            Ok(())
        }
    }

    fn expect_arity<V: Typed>(&self, contract: &[V], loc: Loc) -> LazyResult<()> {
        for (stk, contr) in self.iter().rev().zip(contract.iter().rev()) {
            expect_type(stk, contr, loc)?;
        }
        Ok(())
    }

    fn format_stack_diff<V: Typed>(&self, contract: &[V], loc: Loc) -> LazyError {
        let contr = format_stack(contract);
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
                contr.apply(f),
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
    if !equals_any!(expected.get_type(), Value::Any, Data::Ptr(Value::Any), frame.get_type()) {
        Err(format_type_diff(frame.clone(), expected.get_type(), loc))
    } else {
        Ok(())
    }
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

use ashlib::{from_i32, DoubleResult, EvalStack, Stack};
use either::Either;
use firelib::{
    lazy::{LazyCtx, LazyFormatter},
    lexer::Loc,
};
use itertools::Itertools;

use super::{
    program::{Fmt, LazyError, LazyResult, Program},
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
        (len >= n).with_ctx(move |fmt| {
            format!(
                concat!(
                    "Stack has less elements than expected\n",
                    "[INFO] {}Expected `{}` elements, but found `{}`"
                ),
                fmt.apply(Fmt::Loc(loc)),
                n,
                len
            )
        })?;
        Ok(())
    }

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
                f.apply(Fmt::Loc(loc)),
                contr.apply(f),
                f.apply(Fmt::Loc(loc)),
                stack.apply(f),
                frame.apply(f)
            )
        })
    }
}

fn expect_type<T: Clone + Typed + Location + 'static, V: Typed>(
    frame: &T, expected: V, loc: Loc,
) -> LazyResult<()> {
    if !equals_any!(
        expected.get_type(),
        Value::Any,
        TokenType::DataPtr(Value::Any),
        frame.get_type()
    ) {
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
            f.apply(Fmt::Loc(loc)),
            f.apply(Fmt::Typ(expected)),
            f.apply(Fmt::Typ(frame.get_type())),
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
            f.apply(Fmt::Loc(t.loc())),
            f.apply(Fmt::Typ(t.get_type()))
        )
    }
}

pub fn format_stack<T: Typed>(stack: &[T]) -> impl LazyFormatter<Fmt> + 'static {
    let types: Vec<TokenType> = stack.iter().map(|t| t.get_type()).collect();
    lazyformat! { |f|
        format!(
            "[{}] ->",
            types
                .iter()
                .map(|&t| format!("<{}>", f.apply(Fmt::Typ(t))))
                .join(", ")
        )
    }
}

pub type CompEvalStack = EvalStack<IRToken>;
impl Expect<IRToken> for CompEvalStack {}

pub trait Evaluator<T> {
    fn evaluate(&mut self, item: T, prog: &mut Program) -> DoubleResult<(), T>;
}

impl Evaluator<IRToken> for CompEvalStack {
    fn evaluate(&mut self, tok: IRToken, prog: &mut Program) -> DoubleResult<(), IRToken> {
        match tok.token_type {
            TokenType::Keyword => match from_i32(tok.operand) {
                KeywordType::Drop => {
                    self.expect_pop(tok.loc)?;
                }

                KeywordType::Dup => {
                    let top = (*self.expect_peek(ArityType::Any, tok.loc)?).clone();
                    self.push(top);
                }

                KeywordType::Swap => {
                    let [a, b] = self.expect_pop_n(tok.loc)?;
                    self.push_n([b, a]);
                }

                KeywordType::Over => {
                    let [a, b] = self.expect_pop_n(tok.loc)?;
                    self.push_n([a.clone(), b, a]);
                }

                KeywordType::Rot => {
                    let [a, b, c] = self.expect_pop_n(tok.loc)?;
                    self.push_n([b, c, a]);
                }

                KeywordType::Equal => {
                    let [a, b] = self.expect_arity_pop(ArityType::Same, tok.loc)?;
                    let value = fold_bool!(a.operand == b.operand, 1, 0);
                    self.push(IRToken::new(BOOL, value, tok.loc))
                }

                _ => Err(Either::Left(IRToken::new(TokenType::Keyword, tok.operand, tok.loc)))?,
            },

            TokenType::Word => {
                let word = prog.get_word(tok.operand);
                match prog.get_intrinsic_type(word) {
                    Some(intrinsic) => match intrinsic {
                        IntrinsicType::Plus => {
                            let [a, b] = self.expect_arity_pop(ArityType::Same, tok.loc)?;
                            let value = a.operand + b.operand;
                            self.push(IRToken::new(a.token_type, value, tok.loc))
                        }

                        IntrinsicType::Minus => {
                            let [a, b] = self.expect_arity_pop(ArityType::Same, tok.loc)?;
                            let value = a.operand - b.operand;
                            self.push(IRToken::new(a.token_type, value, tok.loc))
                        }

                        IntrinsicType::Cast(n) => {
                            let a = self.expect_pop(tok.loc)?;

                            let cast = match n {
                                1.. => Value::from((n - 1) as usize).get_type(),
                                0 => unreachable!(),
                                _ => todo!("casting to ptr type not implemented yet"),
                            };

                            self.push(IRToken::new(cast, a.operand, tok.loc));
                        }

                        _ => {
                            Err(Either::Left(IRToken::new(TokenType::Word, tok.operand, tok.loc)))?
                        }
                    },

                    None => match prog.get_const_name(word) {
                        Some(constant) => self.push((constant, tok.loc).into()),
                        None => Err(Either::Left(tok))?,
                    },
                }
            }

            TokenType::Str => {
                prog.register_string(tok.operand);
                let data = prog.get_string(tok.operand);

                self.push(IRToken::new(INT, data.size(), tok.loc));
                self.push(IRToken::new(PTR, data.offset(), tok.loc));
            }

            TokenType::DataType(value) => match value {
                Value::Int | Value::Bool | Value::Ptr => self.push(tok),
                _ => Err(Either::Left(tok))?,
            },

            _ => Err(Either::Left(tok))?,
        }

        DoubleResult::new(())
    }
}

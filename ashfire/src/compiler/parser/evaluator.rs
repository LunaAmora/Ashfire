use ashlib::{from_i32, EvalStack, UncheckedStack};
use either::Either;
use firelib::lazy::LazyFormatter;

use super::parser::Parser;
use crate::compiler::{
    expect::{format_frames, ArityType, Expect},
    program::{Fmt, Program},
    types::*,
};

type DoubleResult<T> = ashlib::DoubleResult<T, IRToken, Fmt>;

impl Parser {
    pub fn compile_eval(&self, prog: &Program) -> DoubleResult<(IRToken, usize)> {
        let (mut result, skip) = self.compile_eval_n(1, prog)?;
        DoubleResult::new((result.pop().unwrap(), skip))
    }

    pub fn compile_eval_n(&self, n: usize, prog: &Program) -> DoubleResult<(Vec<IRToken>, usize)> {
        let mut stack = EvalStack::default();
        let mut i = 0;

        while let Some(tok) = self.tokens().get(i).cloned() {
            if &tok == KeywordType::End {
                if stack.is_empty() && i == 0 {
                    stack.push(IRToken::new(Value::Any.get_type(), 0, tok.loc));
                } else if stack.len() != n {
                    let frames = format_frames(&stack);
                    let len = stack.len();

                    lazybail!(
                        |f| concat!(
                            "{}Expected {} value{} on the stack ",
                            "in the end of the compile-time evaluation, ",
                            "but found {}:\n{}"
                        ),
                        f.format(Fmt::Loc(tok.loc)),
                        n,
                        fold_bool!(n > 1, "s", ""),
                        len,
                        frames.apply(f)
                    );
                }
                break;
            }

            stack.evaluate(tok, prog)?;

            i += 1;
        }

        DoubleResult::new((stack.to_vec(), i + 1))
    }
}

impl Expect<IRToken> for EvalStack<IRToken> {}

pub trait Evaluator {
    fn evaluate(&mut self, item: IRToken, prog: &Program) -> DoubleResult<()>;
}

impl Evaluator for EvalStack<IRToken> {
    fn evaluate(&mut self, tok: IRToken, prog: &Program) -> DoubleResult<()> {
        match tok.token_type {
            TokenType::Keyword => match from_i32(tok.operand) {
                KeywordType::Drop => {
                    self.expect_pop(tok.loc)?;
                }

                KeywordType::Dup => {
                    let top = self.expect_peek(ArityType::Any, tok.loc)?.clone();
                    self.push(top);
                }

                KeywordType::Swap => {
                    let [a, b] = self.expect_pop_n(tok.loc)?;
                    self.extend([b, a]);
                }

                KeywordType::Over => {
                    let [a, b] = self.expect_pop_n(tok.loc)?;
                    self.extend([a, b, a]);
                }

                KeywordType::Rot => {
                    let [a, b, c] = self.expect_pop_n(tok.loc)?;
                    self.extend([b, c, a]);
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

                            let cast = Data::from(n).get_type();
                            self.push(IRToken::new(cast, a.operand, tok.loc));
                        }

                        _ => {
                            Err(Either::Left(IRToken::new(TokenType::Word, tok.operand, tok.loc)))?
                        }
                    },

                    None => match prog.get_const_by_name(word) {
                        Some(constant) => self.push((constant, tok.loc).into()),
                        None => Err(Either::Left(tok))?,
                    },
                }
            }

            TokenType::Str => {
                let (size, offset) = prog.get_string(tok.operand).data();

                self.extend([
                    IRToken::new(INT, size, tok.loc),
                    IRToken::new(PTR, offset, tok.loc),
                ]);
            }

            TokenType::Data(Data::Typ(value)) => match value {
                Value::Int | Value::Bool | Value::Ptr => self.push(tok),
                _ => Err(Either::Left(tok))?,
            },

            _ => Err(Either::Left(tok))?,
        }

        DoubleResult::new(())
    }
}

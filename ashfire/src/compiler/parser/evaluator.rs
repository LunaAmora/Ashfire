use ashlib::{from_i32, DoubleResult, EvalStack, Stack};
use either::Either;
use firelib::lazy::LazyFormatter;

use super::parser::Parser;
use crate::compiler::{
    expect::{format_frames, ArityType, Expect},
    program::{Fmt, Program},
    types::*,
};

impl Parser {
    pub fn compile_eval(&self, prog: &mut Program) -> DoubleResult<(IRToken, usize), IRToken> {
        let (mut result, skip) = self.compile_eval_n(1, prog)?;
        DoubleResult::new((result.pop().unwrap(), skip))
    }

    pub fn compile_eval_n(
        &self, n: usize, prog: &mut Program,
    ) -> DoubleResult<(Vec<IRToken>, usize), IRToken> {
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

pub trait Evaluator<T> {
    fn evaluate(&mut self, item: T, prog: &mut Program) -> DoubleResult<(), T>;
}

impl Evaluator<IRToken> for EvalStack<IRToken> {
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

                    None => match prog.get_const_by_name(word) {
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

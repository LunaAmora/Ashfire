use ashfire_types::{
    core::*,
    data::{Value, ValueType},
    enums::{IntrinsicType, KeywordType},
};
use ashlib::{Either, EvalStack, UncheckedStack};
use firelib::{lazy::LazyFormatter, lexer::Loc};

use super::{parser::Parser, utils::unexpected_end};
use crate::compiler::{
    program::{Fmt, Program},
    typechecking::expect::{format_frames, ArityType, Expect},
};

type DoubleResult<T> = ashlib::DoubleResult<T, IRToken, Fmt>;

impl Parser {
    pub fn compile_eval(&self, prog: &Program, loc: Loc) -> DoubleResult<(IRToken, usize)> {
        let (mut result, skip) = self.compile_eval_n(1, prog, loc)?;
        DoubleResult::new((result.pop().unwrap(), skip))
    }

    pub fn compile_eval_n(
        &self, n: usize, prog: &Program, loc: Loc,
    ) -> DoubleResult<(Vec<IRToken>, usize)> {
        let mut stack = EvalStack::default();
        let mut i = 0;

        while let Some(tok) = self.get_cloned(i) {
            if &tok == KeywordType::End {
                if stack.is_empty() && i == 0 {
                    todo!()
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

                return DoubleResult::new((stack.to_vec(), i + 1));
            }

            stack.evaluate(tok, prog)?;

            i += 1;
        }

        Err(unexpected_end("`end` to close compile time evaluation block", loc))?
    }
}

impl Expect<IRToken> for EvalStack<IRToken> {}

pub trait Evaluator {
    fn evaluate(&mut self, item: IRToken, prog: &Program) -> DoubleResult<()>;
}

impl Evaluator for EvalStack<IRToken> {
    fn evaluate(&mut self, tok: IRToken, prog: &Program) -> DoubleResult<()> {
        match tok.token_type {
            TokenType::Keyword => match tok.as_keyword() {
                KeywordType::Drop => {
                    self.expect_pop(tok.loc)?;
                }

                KeywordType::Dup => self.pop_extend(|[a]| [a.clone(), a], tok.loc)?,
                KeywordType::Swap => self.pop_extend(|[a, b]| [b, a], tok.loc)?,
                KeywordType::Over => self.pop_extend(|[a, b]| [a.clone(), b, a], tok.loc)?,
                KeywordType::Rot => self.pop_extend(|[a, b, c]| [b, c, a], tok.loc)?,

                KeywordType::Equal => self.pop_push_arity(
                    |[a, b]| IRToken::new(BOOL, fold_bool!(*a == *b, 1, 0), tok.loc),
                    ArityType::Same,
                    tok.loc,
                )?,

                _ => Err(Either::Left(IRToken::new(TokenType::Keyword, &tok, tok.loc)))?,
            },

            TokenType::Word => match prog.get_intrinsic_type(prog.get_word(&tok)) {
                Some(intrinsic) => match intrinsic {
                    IntrinsicType::Plus => self.pop_push_arity(
                        |[a, b]| IRToken::new(a.token_type, *a + *b, tok.loc),
                        ArityType::Same,
                        tok.loc,
                    )?,

                    IntrinsicType::Minus => self.pop_push_arity(
                        |[a, b]| IRToken::new(a.token_type, *a - *b, tok.loc),
                        ArityType::Same,
                        tok.loc,
                    )?,

                    IntrinsicType::Cast(n) => self.pop_push_arity(
                        |[a]| IRToken::new(ValueType::from(n).get_type(), a, tok.loc),
                        ArityType::Any,
                        tok.loc,
                    )?,

                    _ => Err(Either::Left(IRToken::new(TokenType::Word, &tok, tok.loc)))?,
                },

                None => match prog.get_const_by_name(&tok.str_key()) {
                    Some(constant) => self.push((constant, tok.loc).into()),
                    None => Err(Either::Left(tok))?,
                },
            },

            TokenType::Str => {
                let (size, _) = prog.get_data(&tok).data();

                self.extend([
                    IRToken::new(INT, size, tok.loc),
                    IRToken::new(STR, tok.operand, tok.loc),
                ]);
            }

            TokenType::Data(ValueType::Typ(value)) => match value {
                Value::INT | Value::BOOL | Value::PTR => self.push(tok),
                _ => Err(Either::Left(tok))?,
            },

            TokenType::Data(_) => Err(Either::Left(tok))?,
        }

        DoubleResult::new(())
    }
}

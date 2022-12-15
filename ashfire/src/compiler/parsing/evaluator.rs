use ashfire_types::{
    core::*,
    data::{TypeDescr, TypeId, ValueType},
    enums::{IntrinsicType, KeywordType},
};
use ashlib::{Either, EvalStack, UncheckedStack};
use firelib::{lazy::LazyFormatter, lexer::Loc};

use super::{parser::Parser, utils::unexpected_end};
use crate::compiler::{
    program::{Fmt, Program},
    typechecking::expect::{format_frames, ArityType, Expect},
};

type DoubleResult<T> = ashlib::DoubleResult<'static, T, IRToken, Fmt>;

impl Parser {
    pub fn compile_eval(&self, prog: &mut Program, loc: Loc) -> DoubleResult<(IRToken, usize)> {
        let (mut result, skip) = self.compile_eval_n(1, prog, loc)?;
        DoubleResult::new((result.pop().unwrap(), skip))
    }

    pub fn compile_eval_n(
        &self, n: usize, prog: &mut Program, loc: Loc,
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
                        f.format(Fmt::Loc(tok.loc())),
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
    fn evaluate(&mut self, item: IRToken, prog: &mut Program) -> DoubleResult<()>;
}

impl Evaluator for EvalStack<IRToken> {
    fn evaluate(&mut self, tok: IRToken, prog: &mut Program) -> DoubleResult<()> {
        let IRToken(token_type, operand, loc) = tok;

        match token_type {
            TokenType::Keyword => match tok.as_keyword() {
                KeywordType::Drop => {
                    self.expect_pop(loc)?;
                }

                KeywordType::Dup => self.pop_extend(|[a]| [a.clone(), a], loc)?,
                KeywordType::Swap => self.pop_extend(|[a, b]| [b, a], loc)?,
                KeywordType::Over => self.pop_extend(|[a, b]| [a.clone(), b, a], loc)?,
                KeywordType::Rot => self.pop_extend(|[a, b, c]| [b, c, a], loc)?,

                KeywordType::Equal => self.pop_push_arity(
                    |[a, b]| IRToken(BOOL, fold_bool!(*a == *b, 1, 0), loc),
                    ArityType::Same,
                    loc,
                )?,

                _ => Err(Either::Left(IRToken(TokenType::Keyword, *tok, loc)))?,
            },

            TokenType::Word => match prog.get_intrinsic_type(&prog.get_word(tok.index())) {
                Some(intrinsic) => match intrinsic {
                    IntrinsicType::Plus => self.pop_push_arity(
                        |[a, b]| IRToken(a.get_type(), *a + *b, loc),
                        ArityType::Same,
                        loc,
                    )?,

                    IntrinsicType::Minus => self.pop_push_arity(
                        |[a, b]| IRToken(a.get_type(), *a - *b, loc),
                        ArityType::Same,
                        loc,
                    )?,

                    IntrinsicType::Cast(type_id) => self.pop_push_arity(
                        |[a]| IRToken(type_id.get_type(), *a, loc),
                        ArityType::Any,
                        loc,
                    )?,

                    _ => Err(Either::Left(IRToken(TokenType::Word, *tok, loc)))?,
                },

                None => match prog.get_const_by_name(tok.name()) {
                    Some(TypeDescr::Primitive(unit)) => {
                        self.push(IRToken(unit.type_id().get_type(), unit.value(), loc));
                    }
                    Some(_) => todo!("Support const use on other consts"),
                    None => Err(Either::Left(tok))?,
                },
            },

            TokenType::Str => {
                let (size, _) = prog.get_data(tok.index()).data();

                self.extend([IRToken(INT, size as i32, loc), IRToken(STR, operand, loc)]);
            }

            TokenType::Data(ValueType(value)) => match value {
                TypeId::INT | TypeId::BOOL | TypeId::PTR => self.push(tok),
                _ => Err(Either::Left(tok))?,
            },
        }

        DoubleResult::new(())
    }
}

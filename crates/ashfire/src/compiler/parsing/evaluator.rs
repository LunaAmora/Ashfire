use ashfire_types::{
    core::*,
    data::{DataType, TypeDescr, TypeId},
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
        let (Either::Left(result), skip) = self.compile_eval_n(1, prog, loc)? else {
            unreachable!();
        };
        DoubleResult::new((result, skip))
    }

    pub fn compile_eval_n(
        &self, n: usize, prog: &mut Program, loc: Loc,
    ) -> DoubleResult<(Either<IRToken, Vec<IRToken>>, usize)> {
        let mut stack = EvalStack::<DataToken>::default();
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

                return DoubleResult::new((
                    if stack.len() == 1 {
                        Either::Left(stack.pop().into())
                    } else {
                        Either::Right(stack.iter().map(|&data| data.into()).collect())
                    },
                    i + 1,
                ));
            }

            stack.evaluate(tok, prog)?;

            i += 1;
        }

        Err(unexpected_end("`end` to close compile time evaluation block", loc))?
    }
}

impl Expect<'_, DataToken> for EvalStack<DataToken> {}

pub trait Evaluator {
    fn evaluate(&mut self, item: IRToken, prog: &mut Program) -> DoubleResult<()>;
}

impl Evaluator for EvalStack<DataToken> {
    fn evaluate(&mut self, tok: IRToken, prog: &mut Program) -> DoubleResult<()> {
        let IRToken(token_type, loc) = tok;

        match token_type {
            TokenType::Keyword(key) => match key {
                KeywordType::Drop => {
                    self.expect_pop(loc)?;
                }

                KeywordType::Dup => self.pop_extend(|[a]| [a, a], loc)?,
                KeywordType::Swap => self.pop_extend(|[a, b]| [b, a], loc)?,
                KeywordType::Over => self.pop_extend(|[a, b]| [a, b, a], loc)?,
                KeywordType::Rot => self.pop_extend(|[a, b, c]| [b, c, a], loc)?,

                KeywordType::Equal => self.pop_push_arity(
                    |[a, b]| DataToken::new(TypeId::BOOL, (a == b).into(), loc),
                    ArityType::Same,
                    loc,
                )?,

                _ => Err(Either::Left(tok))?,
            },

            TokenType::Word(name) => match prog.get_intrinsic_type(&prog.get_word(name)) {
                Some(intrinsic) => match intrinsic {
                    IntrinsicType::Plus => {
                        self.pop_push_arity(|[a, b]| a.add(b, loc), ArityType::Same, loc)?;
                    }

                    IntrinsicType::Minus => {
                        self.pop_push_arity(|[a, b]| a.sub(b, loc), ArityType::Same, loc)?;
                    }

                    IntrinsicType::Cast(id) => self.pop_push_arity(
                        |[a]| DataToken::new(id, a.value(), loc),
                        ArityType::Any,
                        loc,
                    )?,

                    _ => Err(Either::Left(tok))?,
                },

                None => match prog.get_const_by_name(name) {
                    Some(TypeDescr::Primitive(unit)) => {
                        self.push(DataToken::new(unit.type_id(), unit.value(), loc));
                    }
                    Some(_) => todo!("Support const use on other consts"),
                    None => Err(Either::Left(tok))?,
                },
            },

            TokenType::Str(index) => {
                let (size, _) = prog.get_data(index).data();

                self.extend([
                    DataToken::new(TypeId::INT, size as i32, loc),
                    DataToken::new(TypeId::STR, index as i32, loc),
                ]);
            }

            TokenType::Data(value @ Value(DataType(id), _)) => match id {
                TypeId::INT | TypeId::BOOL | TypeId::PTR => {
                    self.push(DataToken(value, loc));
                }
                _ => Err(Either::Left(tok))?,
            },

            TokenType::Type(_) => todo!(),
        }

        DoubleResult::new(())
    }
}

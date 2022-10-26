use ashlib::{from_i32, DoubleResult, EvalStack, Stack};
use either::Either;
use firelib::{anyhow::Result, lexer::Loc};

use super::{program::Program, types::*};

pub enum ArityType<T: Copy> {
    Any,
    Same,
    Type(T),
}

pub trait Expect<T: Clone, B: From<T> + Copy>: Stack<T> {
    fn program_arity(&self, program: &Program, contract: &[B], loc: &Loc) -> Result<()>;
    fn program_exact(&self, program: &Program, contract: &[B], loc: &Loc) -> Result<()>;
    fn program_type(&self, program: &Program, frame: &T, expected: B, loc: &Loc) -> Result<()>;

    fn expect_exact_pop(&mut self, contract: &[B], program: &Program, loc: &Loc) -> Result<Vec<T>> {
        self.expect_stack_size(contract.len(), loc)?;
        self.program_exact(program, contract, loc)?;
        self.pop_n(contract.len())
    }

    fn expect_contract_pop(&mut self, contr: &[B], program: &Program, loc: &Loc) -> Result<Vec<T>> {
        self.expect_stack_size(contr.len(), loc)?;
        self.program_arity(program, contr, loc)?;
        self.pop_n(contr.len())
    }

    fn expect_array_pop<const N: usize>(
        &mut self, contr: [B; N], program: &Program, loc: &Loc,
    ) -> Result<[T; N]> {
        self.expect_stack_size(contr.len(), loc)?;
        self.program_arity(program, &contr, loc)?;
        self.pop_array()
    }

    fn expect_arity_pop<const N: usize>(
        &mut self, arity: ArityType<B>, program: &Program, loc: &Loc,
    ) -> Result<[T; N]> {
        self.expect_arity(N, arity, program, loc)?;
        self.pop_array()
    }

    fn expect_peek(&mut self, arity_t: ArityType<B>, program: &Program, loc: &Loc) -> Result<&T> {
        self.expect_arity(1, arity_t, program, loc)?;
        Ok(self.peek().unwrap())
    }

    fn expect_pop(&mut self, loc: &Loc) -> Result<T> {
        self.expect_stack_size(1, loc)?;
        Ok(self.pop().unwrap())
    }

    fn expect_pop_n<const N: usize>(&mut self, loc: &Loc) -> Result<[T; N]> {
        self.expect_stack_size(N, loc)?;
        self.pop_array()
    }

    fn expect_pop_type(&mut self, arity_t: B, program: &Program, loc: &Loc) -> Result<T> {
        self.expect_arity(1, ArityType::Type(arity_t), program, loc)?;
        Ok(self.pop().unwrap())
    }

    fn expect_arity(
        &self, n: usize, arity: ArityType<B>, program: &Program, loc: &Loc,
    ) -> Result<()> {
        self.expect_stack_size(n, loc)?;

        let (typ, start) = match arity {
            ArityType::Any => return Ok(()),
            ArityType::Same => (B::from(self.get(0).cloned().unwrap()), 1),
            ArityType::Type(typ) => (typ, 0),
        };

        for i in start..n {
            self.program_type(program, self.get(i).unwrap(), typ, loc)?;
        }

        Ok(())
    }

    fn expect_stack_size(&self, n: usize, loc: &Loc) -> Result<()> {
        let len = self.len();
        any_ensure!(
            len >= n,
            concat!(
                "Stack has less elements than expected\n",
                "[INFO] {}Expected `{}` elements, but found `{}`"
            ),
            loc,
            n,
            len
        );
        Ok(())
    }
}

pub type CompEvalStack = EvalStack<IRToken>;

impl Expect<IRToken, TokenType> for CompEvalStack {
    fn program_arity(&self, program: &Program, contract: &[TokenType], loc: &Loc) -> Result<()> {
        let frames: Vec<TypeFrame> = self.iter().map(TypeFrame::from).collect();
        program.expect_arity(&frames, contract, loc)
    }

    fn program_exact(&self, program: &Program, contract: &[TokenType], loc: &Loc) -> Result<()> {
        let frames: Vec<TypeFrame> = self.iter().map(TypeFrame::from).collect();
        program.expect_exact(&frames, contract, loc)
    }

    fn program_type(
        &self, program: &Program, frame: &IRToken, expected: TokenType, loc: &Loc,
    ) -> Result<()> {
        program.expect_type(expected, &frame.into(), loc)
    }
}

pub trait Evaluator<T> {
    fn evaluate(&mut self, item: T, prog: &mut Program) -> DoubleResult<(), T>;
}

impl Evaluator<IRToken> for CompEvalStack {
    fn evaluate(&mut self, tok: IRToken, prog: &mut Program) -> DoubleResult<(), IRToken> {
        match tok.token_type {
            TokenType::Keyword => match from_i32(tok.operand) {
                KeywordType::Drop => {
                    self.expect_pop(&tok.loc)?;
                }

                KeywordType::Dup => {
                    let top = (*self.expect_peek(ArityType::Any, prog, &tok.loc)?).clone();
                    self.push(top);
                }

                KeywordType::Swap => {
                    let [a, b] = self.expect_pop_n(&tok.loc)?;
                    self.push_n([b, a]);
                }

                KeywordType::Over => {
                    let [a, b] = self.expect_pop_n(&tok.loc)?;
                    self.push_n([a.clone(), b, a]);
                }

                KeywordType::Rot => {
                    let [a, b, c] = self.expect_pop_n(&tok.loc)?;
                    self.push_n([b, c, a]);
                }

                KeywordType::Equal => {
                    let [a, b] = self.expect_arity_pop(ArityType::Same, prog, &tok.loc)?;
                    let value = fold_bool!(a.operand == b.operand, 1, 0);
                    self.push(IRToken::new(BOOL, value, &tok.loc))
                }

                _ => Err(Either::Left(IRToken::new(TokenType::Keyword, tok.operand, &tok.loc)))?,
            },

            TokenType::Word => {
                let word = prog.get_word(tok.operand);
                match prog.get_intrinsic_type(word) {
                    Some(intrinsic) => match intrinsic {
                        IntrinsicType::Plus => {
                            let [a, b] = self.expect_arity_pop(ArityType::Same, prog, &tok.loc)?;
                            let value = a.operand + b.operand;
                            self.push(IRToken::new(a.token_type, value, &tok.loc))
                        }

                        IntrinsicType::Minus => {
                            let [a, b] = self.expect_arity_pop(ArityType::Same, prog, &tok.loc)?;
                            let value = a.operand - b.operand;
                            self.push(IRToken::new(a.token_type, value, &tok.loc))
                        }

                        IntrinsicType::Cast(n) => {
                            let a = self.expect_pop(&tok.loc)?;

                            let cast = match n {
                                1.. => Value::from((n - 1) as usize).get_type(),
                                0 => unreachable!(),
                                _ => todo!("casting to ptr type not implemented yet"),
                            };

                            self.push(IRToken::new(cast, a.operand, &tok.loc));
                        }

                        _ => {
                            Err(Either::Left(IRToken::new(TokenType::Word, tok.operand, &tok.loc)))?
                        }
                    },

                    None => match prog.get_const_name(word) {
                        Some(constant) => self.push((constant, &tok.loc).into()),
                        None => Err(Either::Left(tok))?,
                    },
                }
            }

            TokenType::Str => {
                prog.register_string(tok.operand);
                let data = prog.get_string(tok.operand);

                self.push(IRToken::new(INT, data.size(), &tok.loc));
                self.push(IRToken::new(PTR, data.offset(), &tok.loc));
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

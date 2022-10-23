use std::{collections::VecDeque, fs::File, io::BufReader, ops::Deref, path::PathBuf};

use anyhow::{Context, Result};
use ashlib::*;
use either::Either;
use firelib::*;

use super::{
    evaluator::{CompEvalStack, Evaluator},
    lexer::Lexer,
    program::*,
    types::*,
};

pub struct LocWord {
    pub name: String,
    pub loc: Loc,
}

impl PartialEq<str> for LocWord {
    fn eq(&self, other: &str) -> bool {
        self.name == other
    }
}

impl Deref for LocWord {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.name
    }
}

impl LocWord {
    pub fn new(name: &String, loc: Loc) -> Self {
        Self { name: name.to_owned(), loc }
    }

    /// Returns an `Vec<Op>` if the word started with a `.`
    fn get_offset(&self, operand: i32) -> Option<Vec<Op>> {
        self.strip_prefix('.').map(|strip| {
            Op::from(strip.strip_prefix('*').map_or_else(
                || (OpType::OffsetLoad, operand, self.loc.clone()),
                |_| (OpType::Offset, operand, self.loc.clone()),
            ))
            .into()
        })
    }

    /// Searches for a `global mem` that matches the given [`LocWord`] name.
    fn get_global_mem(&self, prog: &Program) -> Option<Vec<Op>> {
        prog.get_mem(self)
            .map(|global| Op::new(OpType::PushGlobalMem, global.value(), &self.loc).into())
    }
}

#[derive(Clone, Copy)]
pub enum VarWordType {
    Store,
    Pointer,
}

#[derive(Default)]
pub struct Parser {
    ir_tokens: VecDeque<IRToken>,
    op_blocks: Vec<Op>,
    structs: Vec<Word>,
    current_proc: Option<usize>,
}

impl ProgramVisitor for Parser {
    fn set_index(&mut self, i: Option<usize>) {
        self.current_proc = i;
    }

    fn get_index(&self) -> Option<usize> {
        self.current_proc
    }
}

impl Parser {
    pub fn new() -> Self {
        Self { ..Default::default() }
    }

    /// Pops and returns the next [`IRToken`] of this [`Parser`].
    fn next(&mut self) -> Option<IRToken> {
        self.ir_tokens.pop_front()
    }

    /// Pops `n` elements and returns the last [`IRToken`] of this [`Parser`].
    fn skip(&mut self, n: usize) -> Option<IRToken> {
        let mut result = None;
        for _ in 0..n {
            result = self.ir_tokens.pop_front()
        }
        result
    }

    /// Parse each [`IRToken`] from this [`Parser`] to an [`Op`],
    /// appending to the given [`Program`].
    ///
    /// # Errors
    ///
    /// This function will return an error if any problem is encountered during parsing.
    pub fn parse_tokens(&mut self, prog: &mut Program) -> Result<()> {
        while let Some(token) = self.next() {
            if let Some(mut op) = self.define_op(token, prog).value? {
                prog.ops.append(&mut op)
            }
        }
        Ok(())
    }

    fn define_op(&mut self, tok: IRToken, prog: &mut Program) -> OptionErr<Vec<Op>> {
        ensure!(
            matches!(tok.token_type, TokenType::Keyword | TokenType::Word) || self.inside_proc(),
            "{}Token type cannot be used outside of a procedure: `{:?}",
            tok.loc,
            tok.token_type
        );

        let op = Op::from(match tok.token_type {
            TokenType::Keyword => return self.define_keyword_op(tok.operand, tok.loc),

            TokenType::Str => (OpType::PushStr, prog.register_string(tok.operand), tok.loc),

            TokenType::DataType(typ) => match typ {
                ValueType::Type(_) => bail!("{}Value type not valid here: `{:?}`", tok.loc, typ),
                _ => (OpType::PushData(typ), tok.operand, tok.loc),
            },

            TokenType::DataPtr(typ) => {
                bail!("{}Data pointer type not valid here: `{:?}`", tok.loc, typ)
            }

            TokenType::Word => {
                let word = &LocWord::new(prog.get_word(tok.operand), tok.loc);

                choice!(
                    OptionErr,
                    self.get_const_struct(word, prog),
                    word.get_offset(tok.operand),
                    self.get_binding(word, prog),
                    prog.get_intrinsic(word),
                    self.get_local_mem(word, prog),
                    word.get_global_mem(prog),
                    prog.get_proc_name(word),
                    self.get_const_name(word, prog),
                    self.get_variable(word, prog),
                    self.define_context(word, prog);
                    bail!("{}Word was not declared on the program: `{}`", word.loc, word.name)
                )
            }
        });

        OptionErr::new(vec![op])
    }

    fn define_keyword_op(&mut self, operand: i32, loc: Loc) -> OptionErr<Vec<Op>> {
        let key = from_i32(operand);

        let op = match key {
            KeywordType::Drop => (OpType::Drop, loc).into(),
            KeywordType::Dup => (OpType::Dup, loc).into(),
            KeywordType::Swap => (OpType::Swap, loc).into(),
            KeywordType::Over => (OpType::Over, loc).into(),
            KeywordType::Rot => (OpType::Rot, loc).into(),
            KeywordType::Equal => (OpType::Equal, loc).into(),
            KeywordType::At => (OpType::Unpack, loc).into(),

            KeywordType::While => self.push_block((OpType::While, loc).into()),

            KeywordType::Do => match self.pop_block(&loc, key)? {
                Op { op_type: OpType::While, .. } => (OpType::Do, loc).into(),
                Op { op_type: OpType::CaseMatch, operand, .. } => {
                    (OpType::CaseOption, operand, loc).into()
                }
                block => {
                    bail!(block.format_err("`do` can only come in a `while` or `case` block", &loc))
                }
            },

            KeywordType::Let => todo!(),
            KeywordType::Case => todo!(),

            KeywordType::Colon => match self.pop_block(&loc, key)? {
                Op { op_type: OpType::CaseStart, .. } => todo!(),
                block => bail!(block
                    .format_err("`:` can only be used on word or `case` block definition", &loc)),
            },

            KeywordType::If => self.push_block((OpType::IfStart, loc).into()),

            KeywordType::Else => match self.pop_block(&loc, key)? {
                Op { op_type: OpType::IfStart, .. } => self.push_block((OpType::Else, loc).into()),
                Op { op_type: OpType::CaseOption, .. } => todo!(),
                block => {
                    bail!(block.format_err("`else` can only come in a `if` or `case` block", &loc,))
                }
            },

            KeywordType::End => match self.pop_block(&loc, key)? {
                Op { op_type: OpType::IfStart, .. } => (OpType::EndIf, loc).into(),
                Op { op_type: OpType::Else, .. } => (OpType::EndElse, loc).into(),
                Op { op_type: OpType::Do, .. } => (OpType::EndWhile, loc).into(),

                Op { op_type: OpType::BindStack, .. } => todo!(),
                Op { op_type: OpType::CaseOption, .. } => todo!(),

                Op { op_type: OpType::PrepProc, operand, .. } => {
                    self.exit_proc();
                    (OpType::EndProc, operand, loc).into()
                }

                block => {
                    bail!(block.format_err("Expected `end` to close a valid block", &loc))
                }
            },

            KeywordType::Include |
            KeywordType::Arrow |
            KeywordType::Proc |
            KeywordType::Mem |
            KeywordType::Struct => bail!("{}Keyword type is not valid here: `{:?}`", loc, key),
        };

        OptionErr::new(vec![op])
    }

    /// Creates a logic block starting from the given [`Op`].
    fn push_block(&mut self, op: Op) -> Op {
        self.op_blocks.push(op.clone());
        op
    }

    /// Pops the last opened logic block, returning a clone of the [`Op`] that started it.
    ///
    /// # Errors
    ///
    /// This function will return an error if no block is open.
    fn pop_block(&mut self, loc: &Loc, closing_type: KeywordType) -> Result<Op> {
        self.op_blocks.pop().with_context(|| {
            format!("{}There are no open blocks to close with `{:?}`", loc, closing_type)
        })
    }

    /// Searches for struct fields that starts with the given [`word`][LocWord]
    /// on the `consts` [Vec]. Parsing each one as an [`IRToken`] to an [`Op`].
    fn get_const_struct(&mut self, word: &LocWord, prog: &mut Program) -> OptionErr<Vec<Op>> {
        let ops = flatten(
            prog.consts
                .iter()
                .filter(|cnst| cnst.starts_with(&format!("{}.", word.name)))
                .map(|tword| (tword, &word.loc).into())
                .collect::<Vec<IRToken>>()
                .into_iter()
                .filter_map(|tok| self.define_op(tok, prog).value.transpose())
                .collect::<Result<Vec<Vec<Op>>>>()?,
        );
        empty_or_some(ops).into()
    }

    /// Searches for a `binding` that matches the given [`word`][LocWord]
    /// on the current [`Proc`].
    fn get_binding(&self, word: &LocWord, prog: &Program) -> Option<Vec<Op>> {
        self.current_proc(prog)
            .and_then(|proc| {
                proc.bindings
                    .iter()
                    .position(|bind| word == bind.as_str())
                    .map(|index| (proc.bindings.len() - 1 - index) as i32)
            })
            .map(|index| Op::new(OpType::PushBind, index, &word.loc).into())
    }

    /// Searches for a `mem` that matches the given [`word`][LocWord]
    /// on the current [`Proc`].
    fn get_local_mem(&self, word: &LocWord, prog: &Program) -> Option<Vec<Op>> {
        self.current_proc(prog)
            .and_then(|proc| proc.local_mem_names.iter().find(|mem| word == mem.as_str()))
            .map(|local| Op::new(OpType::PushLocalMem, local.value(), &word.loc).into())
    }

    /// Searches for a `const` that matches the given[`word`][LocWord]
    /// and parses it as an [`IRToken`] to an [`Op`].
    fn get_const_name(&mut self, word: &LocWord, prog: &mut Program) -> OptionErr<Vec<Op>> {
        prog.get_const_name(word)
            .map(|tword| (tword, &word.loc).into())
            .map_or_else(OptionErr::default, |tok| self.define_op(tok, prog))
    }

    /// Searches for a `variable` that matches the given [`word`][LocWord]
    /// and parses `store` and `pointer` information.
    fn get_variable(&mut self, loc_word: &LocWord, prog: &Program) -> OptionErr<Vec<Op>> {
        let (word, var_typ) = match loc_word.split_at(1) {
            ("!", rest) => (rest, Some(VarWordType::Store)),
            ("*", rest) => (rest, Some(VarWordType::Pointer)),
            _ => (loc_word.as_str(), None),
        };

        let word = LocWord { name: word.to_string(), loc: loc_word.loc.clone() };
        choice!(
            OptionErr,
            self.current_proc(prog)
                .map(|proc| proc.local_vars.clone())
                .map_or_else(OptionErr::default, |vars| self
                    .try_get_var(&word, &vars, true, var_typ, prog)),
            self.try_get_var(&word, &prog.global_vars, false, var_typ, prog),
        )
    }

    fn try_get_var(
        &self, word: &LocWord, vars: &[TypedWord], local: bool, var_typ: Option<VarWordType>,
        prog: &Program,
    ) -> OptionErr<Vec<Op>> {
        let mut result = Vec::new();
        let loc = &word.loc;

        let push_type = fold_bool!(local, OpType::PushLocal, OpType::PushGlobal);

        let (store, pointer) = match var_typ {
            Some(VarWordType::Store) => (true, false),
            Some(VarWordType::Pointer) => (false, true),
            _ => Default::default(),
        };

        if let Some(index) = vars.iter().position(|name| word == name.as_str()) {
            let typ = vars.get(index).unwrap().get_type().into();

            if store {
                result.push(Op::new(OpType::ExpectType, typ, loc))
            }

            result.push(Op::new(push_type, index as i32, loc));

            if store {
                result.push(Op::new(OpType::Intrinsic, IntrinsicType::Store32.into(), loc))
            } else if pointer {
                let ptr_typ = IntrinsicType::Cast(-typ);
                result.push(Op::new(OpType::Intrinsic, ptr_typ.into(), loc));
            } else {
                let data_typ = IntrinsicType::Cast(typ);
                result.push(Op::new(OpType::Intrinsic, IntrinsicType::Load32.into(), loc));
                result.push(Op::new(OpType::Intrinsic, data_typ.into(), loc));
            }

            return OptionErr::new(result);
        }

        let struct_type = vars
            .iter()
            .any(|val| val.starts_with(&format!("{}.", word.name)))
            .then(|| self.try_get_struct_type(word, prog))
            .flatten()
            .or_return(OptionErr::default)?;

        let mut member = struct_type.members().first().unwrap();

        if pointer {
            let pattern = &format!("{}.{}", word.name, member.name());
            let index = expect_index(vars, |name| name.eq(pattern));

            let stk_id = prog.get_data_type(struct_type.name()).unwrap();
            let ptr_typ = IntrinsicType::Cast(-(stk_id as i32));

            result.push(Op::new(push_type, index as i32, loc));
            result.push(Op::new(OpType::Intrinsic, ptr_typ.into(), loc));
        } else {
            let mut members = struct_type.members().to_vec();

            if store {
                members.reverse();
                member = members.first().unwrap();
            }

            let pattern = &format!("{}.{}", word.name, member.name());
            let index = expect_index(vars, |name| name.eq(pattern)) as i32;

            for (i, member) in (0_i32..).zip(members.into_iter()) {
                let operand = index + fold_bool!(local == store, i, -i);

                if store {
                    result.push(Op::new(OpType::ExpectType, member.get_type().into(), loc))
                }

                result.push(Op::new(push_type, operand, loc));

                if store {
                    result.push(Op::new(OpType::Intrinsic, IntrinsicType::Store32.into(), loc))
                } else {
                    let data_typ = IntrinsicType::Cast(member.get_type().into());
                    result.push(Op::new(OpType::Intrinsic, IntrinsicType::Load32.into(), loc));
                    result.push(Op::new(OpType::Intrinsic, data_typ.into(), loc));
                }
            }
        }

        OptionErr::new(result)
    }

    /// Searches for a `struct` that matches the given `&str`,
    /// returning its type.
    fn try_get_struct_type<'a>(&self, word: &str, prog: &'a Program) -> Option<&'a StructType> {
        self.structs
            .iter()
            .find(|stk| word == stk.as_str())
            .and_then(|stk| {
                prog.words
                    .get(stk.value() as usize)
                    .and_then(|stk| prog.get_type_name(stk))
            })
    }

    /// Tries to define the parsing context to use
    /// based on the preceding [`IRTokens`][IRToken].
    fn define_context(&mut self, word: &LocWord, prog: &mut Program) -> OptionErr<Vec<Op>> {
        let mut i = 0;
        let mut colons = 0;
        while let Some(tok) = self.ir_tokens.get(i).cloned() {
            match (colons, &tok.token_type) {
                (1, TokenType::DataType(ValueType::Int)) => {
                    self.parse_memory(word, prog).try_success()?
                }

                (1, TokenType::Word) => choice!(
                    OptionErr,
                    self.parse_proc_ctx(prog.get_word(tok.operand).to_owned(), word, prog),
                    self.parse_struct_ctx(i, word, prog);
                    bail!(prog.invalid_token(tok, "context declaration"))
                ),

                (_, TokenType::Keyword) => {
                    choice!(
                        OptionErr,
                        self.parse_keyword_ctx(&mut colons, word, tok, prog),
                        self.parse_end_ctx(colons, i, word, prog),
                    );
                }

                (0, _) => return self.parse_word_ctx(&tok, word, prog),

                _ => bail!(prog.invalid_token(tok, "context declaration")),
            }
            i += 1;
        }
        OptionErr::default()
    }

    fn parse_proc_ctx(
        &mut self, found_word: String, word: &LocWord, prog: &mut Program,
    ) -> OptionErr<Vec<Op>> {
        match (prog.get_type_name(&found_word), prog.get_data_pointer(&found_word)) {
            (None, None) => OptionErr::default(),
            _ => self.parse_procedure(word, prog),
        }
    }

    fn parse_struct_ctx(
        &mut self, top_index: usize, word: &LocWord, prog: &mut Program,
    ) -> OptionErr<Vec<Op>> {
        if let Ok([n1, n2]) = get_range_ref(&self.ir_tokens, top_index + 1) {
            if prog.get_struct_type(n1).is_some() &&
                equals_any!(n2, KeywordType::End, TokenType::Word)
            {
                return self.parse_struct(word, prog);
            }
        }
        OptionErr::default()
    }

    fn parse_keyword_ctx(
        &mut self, colons: &mut i32, word: &LocWord, tok: IRToken, prog: &mut Program,
    ) -> OptionErr<Vec<Op>> {
        match (*colons, from_i32(tok.operand)) {
            (0, KeywordType::Mem) => {
                self.next();
                self.parse_memory(word, prog).try_success()?;
            }

            (0, KeywordType::Struct) => {
                self.next();
                self.parse_struct(word, prog)
            }

            (0, KeywordType::Proc) => {
                self.next();
                self.parse_procedure(word, prog)
            }

            (1, KeywordType::Equal) => {
                self.skip(2);
                match self.compile_eval(prog).value {
                    Ok((result, eval)) => {
                        ensure!(
                            result.token_type != ValueType::Any,
                            "{}Undefined variable value is not allowed",
                            &word.loc
                        );

                        self.skip(eval);
                        let struct_word = TypedWord::new(word.to_string(), result);
                        self.register_var(struct_word, prog);

                        success!();
                    }
                    Err(either) => match either {
                        Either::Left(tok) => bail!(prog.invalid_token(tok, "context declaration")),
                        Either::Right(err) => bail!(err),
                    },
                }
            }

            (_, KeywordType::Colon) => {
                *colons += 1;
                OptionErr::default()
            }

            (_, KeywordType::End) => bail!(
                "{}Missing body or contract necessary to infer the type of the word: `{}`",
                word.loc,
                word.name
            ),

            _ => OptionErr::default(),
        }
    }

    fn parse_end_ctx(
        &mut self, colons: i32, ctx_size: usize, word: &LocWord, prog: &mut Program,
    ) -> OptionErr<Vec<Op>> {
        (colons == 2).or_return(OptionErr::default)?;

        self.parse_static_ctx(ctx_size, word, prog)?;
        self.define_proc(word, Contract::default(), prog)
            .into_success()?
    }

    fn parse_static_ctx(
        &mut self, ctx_size: usize, word: &LocWord, prog: &mut Program,
    ) -> OptionErr<Vec<Op>> {
        (ctx_size == 1).or_return(|| self.parse_procedure(word, prog))?;

        self.skip(2);
        if let Ok((eval, skip)) = self.compile_eval(prog).value {
            if eval.token_type != ValueType::Any {
                self.skip(skip);
                prog.consts.push(TypedWord::new(word.to_string(), eval));
                success!();
            }
        }
        OptionErr::default()
    }

    fn parse_word_ctx(
        &mut self, tok: &IRToken, word: &LocWord, prog: &mut Program,
    ) -> OptionErr<Vec<Op>> {
        if tok == TokenType::Word {
            if let Some(stk) = prog.get_struct_type(tok).cloned() {
                self.next();
                self.parse_const_or_var(word, tok.operand, stk, prog)
                    .try_success()?;
            }
        }
        OptionErr::default()
    }

    fn define_proc(
        &mut self, name: &LocWord, contract: Contract, prog: &mut Program,
    ) -> Result<Op> {
        anyhow::ensure!(
            !self.inside_proc(),
            "{}Cannot define a procedure inside of another procedure",
            &name.loc
        );

        let proc = Proc::new(name, contract);
        let operand = prog.procs.len();
        self.enter_proc(operand);
        prog.procs.push(proc);

        Ok(self.push_block(Op::new(OpType::PrepProc, operand as i32, &name.loc)))
    }

    fn compile_eval(&self, prog: &mut Program) -> DoubleResult<(IRToken, usize), IRToken> {
        let (mut result, skip) = self.compile_eval_n(1, prog)?;
        DoubleResult::new((result.pop().unwrap(), skip))
    }

    fn compile_eval_n(
        &self, n: usize, prog: &mut Program,
    ) -> DoubleResult<(Vec<IRToken>, usize), IRToken> {
        let mut result = CompEvalStack::default();
        let mut i = 0;

        while let Some(tok) = self.ir_tokens.get(i).cloned() {
            if &tok == KeywordType::End {
                if result.is_empty() && i == 0 {
                    result.push(IRToken::new(ValueType::Any.get_type(), 0, &tok.loc));
                } else if result.len() != n {
                    let frames: Vec<TypeFrame> = result.iter().map(TypeFrame::from).collect();
                    let fmt_frames = prog.format_frames(&frames);

                    bail!(
                        concat!(
                            "Expected {} value{} on the stack ",
                            "in the end of the compile-time evaluation, ",
                            "but found {}:\n{}"
                        ),
                        n,
                        fold_bool!(n > 1, "s", ""),
                        result.len(),
                        fmt_frames
                    );
                }
                break;
            }

            result.evaluate(tok, prog)?;

            i += 1;
        }

        DoubleResult::new((result.to_vec(), i + 1))
    }

    fn parse_procedure(&mut self, word: &LocWord, prog: &mut Program) -> OptionErr<Vec<Op>> {
        let mut ins = Vec::new();
        let mut outs = Vec::new();
        let mut arrow = false;
        let loc = &word.loc;

        self.expect_keyword(prog, KeywordType::Colon, "`:` after keyword `proc`", loc)?;
        let error_text =
            format!("{loc}Expected proc contract or `:` after procedure definition, but found");

        while let Some(tok) = self.next() {
            if let Some(key) = tok.get_keyword() {
                match key {
                    KeywordType::Arrow => {
                        ensure!(!arrow, "{loc}Duplicated `->` found on procedure definition");
                        arrow = true;
                        continue;
                    }
                    KeywordType::Colon => {
                        (self.define_proc(word, Contract::new(ins, outs), prog)).into_success()?
                    }
                    _ => {}
                }
            } else if let Some(found_word) = prog.get_word_from_token(&tok) {
                if let Some(stk) = prog.get_type_name(found_word) {
                    for member in stk.members() {
                        push_by_condition(arrow, member.get_type(), &mut outs, &mut ins);
                    }
                    continue;
                } else if let Some(type_ptr) = prog.get_data_pointer(found_word) {
                    push_by_condition(arrow, type_ptr, &mut outs, &mut ins);
                    continue;
                }
            }

            bail!("{error_text}: `{:?}`", prog.format_token(tok));
        }
        bail!("{error_text} nothing")
    }

    fn expect_keyword(
        &mut self, prog: &Program, key: KeywordType, error_text: &str, loc: &Loc,
    ) -> Result<IRToken> {
        self.expect_by(prog, |tok| tok == key, error_text, loc)
    }

    fn expect_by(
        &mut self, prog: &Program, pred: impl FnOnce(&IRToken) -> bool, error_text: &str, loc: &Loc,
    ) -> Result<IRToken> {
        let tok = self.next();
        expect_token_by(tok, pred, error_text, Some(loc.to_owned()), |tok| prog.format_token(tok))
    }

    fn parse_memory(&mut self, word: &LocWord, prog: &mut Program) -> Result<()> {
        let loc = &word.loc;

        self.expect_keyword(prog, KeywordType::Colon, "`:` after `mem`", loc)?;
        let value_token = self.expect_by(
            prog,
            |tok| tok.token_type == ValueType::Int,
            "memory size after `:`",
            loc,
        )?;
        self.expect_keyword(prog, KeywordType::End, "`end` after memory size", loc)?;

        let size = ((value_token.operand + 3) / 4) * 4;
        prog.push_mem_by_context(self.get_index(), word, size);

        Ok(())
    }

    fn parse_struct(&mut self, word: &LocWord, prog: &mut Program) -> OptionErr<Vec<Op>> {
        let mut members = Vec::new();
        let loc = &word.loc;
        self.expect_keyword(prog, KeywordType::Colon, "`:` after keyword `struct`", loc)?;

        while let Some(tok) = self.ir_tokens.get(0) {
            if tok.token_type == TokenType::Keyword {
                self.expect_keyword(prog, KeywordType::End, "`end` after struct declaration", loc)?;
                prog.structs_types
                    .push(StructType::new(word.to_string(), members));
                success!();
            }

            let next = self.expect_by(
                prog,
                |n| n.token_type == TokenType::Word,
                "struct member name",
                loc,
            )?;

            let found_word = prog.get_word(next.operand).to_owned();

            if let Some(name_type) = self.next() {
                if name_type.token_type == TokenType::Word {
                    let found_type = &prog.get_word(name_type.operand);

                    if let Some(stk_typ) = prog.get_type_name(found_type) {
                        let ref_members = stk_typ.members();

                        if ref_members.len() == 1 {
                            let typ = ref_members.first().unwrap();
                            members.push((found_word, typ).into());
                        } else {
                            for member in ref_members {
                                let member_name = format!("{found_word}.{}", member.name());
                                members.push((member_name, member).into());
                            }
                        }
                        continue;
                    } else if let Some(typ_ptr) = prog.get_data_pointer(found_type) {
                        members.push((found_word, &typ_ptr).into());
                        continue;
                    }
                }
                bail!(
                    "{loc}Expected struct member type but found: {}",
                    prog.format_token(name_type)
                );
            }
        }
        bail!("{loc}Expected struct members or `end` after struct declaration")
    }

    fn parse_const_or_var(
        &mut self, word: &LocWord, operand: i32, stk: StructType, prog: &mut Program,
    ) -> Result<()> {
        let loc = &word.loc;
        self.expect_keyword(prog, KeywordType::Colon, "`:` after variable type definition", loc)?;

        let assign = self
            .expect_by(
                prog,
                |tok| equals_any!(tok, KeywordType::Colon, KeywordType::Equal),
                "`:` or `=` after keyword `:`",
                loc,
            )?
            .get_keyword()
            .unwrap();

        let (mut result, eval) = match self.compile_eval_n(stk.members().len(), prog).value {
            Ok(value) => value,
            Err(either) => match either {
                Either::Right(err) => return Err(err),
                _ => bail!("Failed to parse an valid struct value at compile-time evaluation"),
            },
        };

        let end_token = self.skip(eval).unwrap();
        let mut members = stk.members().to_vec();

        if result.len() == 1 {
            let eval = result.pop().unwrap();

            if eval.token_type == ValueType::Any {
                if self.inside_proc() {
                    members.reverse();
                }
                for member in members {
                    let name = format!("{}.{}", word.name, member.name());
                    let struct_word = TypedWord::new(name, member);
                    self.register_typed_word(&assign, struct_word, prog);
                }
            } else {
                let member_type = members.last().unwrap().get_type();
                anyhow::ensure!(
                    equals_any!(member_type, ValueType::Any, eval.token_type),
                    concat!(
                        "{}Expected type `{:?}` on the stack at the end of ",
                        "the compile-time evaluation, but found: `{:?}`"
                    ),
                    end_token.loc,
                    member_type,
                    eval.token_type
                );

                let struct_word = TypedWord::new(word.to_string(), eval);
                self.register_typed_word(&assign, struct_word, prog);
            }
        } else {
            let stack: Vec<TypeFrame> = result.iter().map(TypeFrame::from).collect();
            let contract: Vec<TokenType> = members.iter().map(StructMember::get_type).collect();
            prog.expect_exact(&stack, &contract, &end_token.loc)?;

            if self.inside_proc() {
                members.reverse();
                if let KeywordType::Equal = assign {
                    result.reverse()
                }
            }

            for (member, item) in members.into_iter().zip(result.into_iter()) {
                let name = format!("{}.{}", word.name, member.name());
                let struct_word = TypedWord::new(name, item);
                self.register_typed_word(&assign, struct_word, prog);
            }
        }

        self.structs.push(Word::new(word, operand));
        Ok(())
    }

    fn register_typed_word(
        &mut self, assign: &KeywordType, struct_word: TypedWord, prog: &mut Program,
    ) {
        match assign {
            KeywordType::Colon => prog.consts.push(struct_word),
            KeywordType::Equal => self.register_var(struct_word, prog),
            _ => unreachable!(),
        }
    }

    fn register_var(&mut self, struct_word: TypedWord, prog: &mut Program) {
        match self.current_proc_mut(prog) {
            Some(proc) => proc.local_vars.push(struct_word),
            _ => prog.global_vars.push(struct_word),
        }
    }

    pub fn lex_file(&mut self, path: &PathBuf, prog: &mut Program) -> Result<&mut Self> {
        let reader = BufReader::new(
            File::open(path).with_context(|| format!("could not read file `{:?}`", &path))?,
        );
        let mut lex = Lexer::new(reader, path);

        while let Some(token) = lex.lex_next_token(prog).value? {
            if &token != KeywordType::Include {
                self.ir_tokens.push_back(token);
                continue;
            }

            let tok = expect_token_by(
                lex.lex_next_token(prog).value?,
                |token| token == TokenType::Str,
                "include file name",
                None,
                |tok| prog.format_token(tok),
            )?;

            let include_path = prog.get_string(tok.operand).as_str();

            let include = get_dir(path)
                .with_context(|| "failed to get file directory path")?
                .join(include_path);

            info!("Including file: {:?}", include);
            self.lex_file(&include, prog)?;
        }
        Ok(self)
    }
}

impl Program {
    fn get_intrinsic(&self, word: &LocWord) -> Option<Vec<Op>> {
        self.get_intrinsic_type(word)
            .map(|i| vec![Op::new(OpType::Intrinsic, i.into(), &word.loc)])
    }

    fn get_proc_name(&self, word: &LocWord) -> Option<Vec<Op>> {
        self.procs
            .iter()
            .position(|proc| word == proc.name.as_str())
            .map(|index| vec![Op::new(OpType::Call, index as i32, &word.loc)])
    }

    fn get_struct_type(&self, tok: &IRToken) -> Option<&StructType> {
        fold_bool!(tok == TokenType::Word, self.get_type_name(self.get_word(tok.operand)))
    }

    fn get_word_from_token(&self, tok: &IRToken) -> Option<&String> {
        fold_bool!(tok == TokenType::Word, Some(self.get_word(tok.operand)))
    }

    fn format_token(&self, tok: IRToken) -> String {
        let desc = self.type_name(tok.token_type);
        let name = self.type_display(tok.token_type, tok.operand);
        format!("{} `{}`", desc, name)
    }

    fn invalid_token(&self, tok: IRToken, error_context: &str) -> String {
        let desc = self.type_name(tok.token_type);
        let name = self.type_display(tok.token_type, tok.operand);
        format!("{}Invalid `{desc}` found on {error_context}: `{name}`", tok.loc)
    }

    pub fn compile_file(&mut self, path: &PathBuf) -> Result<&mut Self> {
        info!("Compiling file: {:?}", path);

        Parser::new().lex_file(path, self)?.parse_tokens(self)?;

        info!("Compilation done");
        Ok(self)
    }
}

impl Op {
    fn format_err(self, error: &str, loc: &Loc) -> String {
        format!(
            concat!(
                "{}{}, but found a `{:?}` block instead\n",
                "[INFO] {}The found block started here."
            ),
            loc, error, self.op_type, self.loc
        )
    }
}

fn expect_token_by(
    value: Option<IRToken>, pred: impl FnOnce(&IRToken) -> bool, desc: &str, loc: Option<Loc>,
    fmt: impl FnOnce(IRToken) -> String,
) -> Result<IRToken> {
    match value {
        Some(tok) if pred(&tok) => Ok(tok),
        Some(tok) => bail!("{}Expected to find {}, but found: {}", tok.loc.clone(), desc, fmt(tok)),
        None => bail!("{}Expected to find {}, but found nothing", loc.unwrap_or_default(), desc),
    }
}

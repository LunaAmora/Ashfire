use std::{collections::VecDeque, path::PathBuf};

use ashlib::from_i32;
use either::Either;
use firelib::{lazy::LazyCtx, lexer::Loc, utils::*, ShortCircuit, TrySuccess};
use num::iter::range_step_from;

use super::{types::*, utils::*};
use crate::compiler::{expect::*, program::*, types::*};

#[derive(Default)]
pub struct Parser {
    ir_tokens: VecDeque<IRToken>,
    name_scopes: NameScopes,
    structs: Vec<IndexWord>,
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

    pub fn tokens(&self) -> &VecDeque<IRToken> {
        &self.ir_tokens
    }

    /// Pops and returns the next [`IRToken`] of this [`Parser`].
    pub fn next(&mut self) -> Option<IRToken> {
        self.ir_tokens.pop_front()
    }

    /// Pops `n` elements and returns the last [`IRToken`] of this [`Parser`].
    pub fn skip(&mut self, n: usize) -> Option<IRToken> {
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
    pub fn parse_tokens(&mut self, prog: &mut Program) -> LazyResult<()> {
        while let Some(token) = self.next() {
            if let Some(mut op) = self.define_op(token, prog).value? {
                prog.ops.append(&mut op)
            }
        }
        Ok(())
    }

    fn define_op(&mut self, tok: IRToken, prog: &mut Program) -> OptionErr<Vec<Op>> {
        if !(matches!(tok.token_type, TokenType::Keyword | TokenType::Word) || self.inside_proc()) {
            lazybail!(
                |f| "{}Token type cannot be used outside of a procedure: `{}`",
                f.format(Fmt::Loc(tok.loc)),
                f.format(Fmt::Typ(tok.token_type))
            )
        };

        let op = Op::from(match tok.token_type {
            TokenType::Keyword => return self.define_keyword_op(tok.operand, tok.loc),

            TokenType::Str => (OpType::PushStr, prog.register_string(tok.operand), tok.loc),

            TokenType::DataType(val) => match val {
                Value::Type(_) => lazybail!(
                    |f| "{}Value type not valid here: `{}`",
                    f.format(Fmt::Loc(tok.loc)),
                    f.format(Fmt::Typ(val.get_type()))
                ),
                _ => (OpType::PushData(val), tok.operand, tok.loc),
            },

            TokenType::DataPtr(ptr) => lazybail!(
                |f| "{}Data pointer type not valid here: `{}`",
                f.format(Fmt::Loc(tok.loc)),
                f.format(Fmt::Typ(ptr.get_type()))
            ),

            TokenType::Word => {
                let word = &LocWord::new(prog.get_word(tok.operand), tok.loc);

                choice!(
                    OptionErr,
                    prog.get_intrinsic(word),
                    word.get_offset(tok.operand),
                    self.lookup_context(word, prog),
                    self.define_context(word, prog)
                )?;

                let error = format!("Word was not declared on the program: `{}`", word.name);
                return error_loc(&error, tok.loc).into();
            }
        });

        OptionErr::new(vec![op])
    }

    fn lookup_context(&self, word: &LocWord, prog: &Program) -> OptionErr<Vec<Op>> {
        let Some(ctx) = self.name_scopes.lookup(word.as_str()) else {
            return self.lookup_modfied_var(word, prog);
        };

        match ctx {
            ParseContext::ProcName => prog.get_proc_name(word),
            ParseContext::GlobalMem => prog.get_global_mem(word),
            ParseContext::Binding => self.get_binding(word, prog),
            ParseContext::ConstStruct => prog.get_const_struct(word),
            ParseContext::LocalMem => self.get_local_mem(word, prog),
            ParseContext::Variable => return self.get_variable(word, None, prog),
        }
        .into()
    }

    fn lookup_modfied_var(&self, word: &LocWord, prog: &Program) -> OptionErr<Vec<Op>> {
        let (rest, var_typ) = match word.split_at(1) {
            ("!", rest) => (rest, VarWordType::Store),
            ("*", rest) => (rest, VarWordType::Pointer),
            _ => return OptionErr::default(),
        };

        let Some(ParseContext::Variable) = self.name_scopes.lookup(rest) else {
            return OptionErr::default();
        };

        let word = LocWord::new(rest, word.loc);
        self.get_variable(&word, Some(var_typ), prog)
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

            KeywordType::Do => match self.pop_block(key, loc)? {
                Op { op_type: OpType::While, .. } => (OpType::Do, loc).into(),
                Op { op_type: OpType::CaseMatch, operand, .. } => {
                    (OpType::CaseOption, operand, loc).into()
                }
                block => Err(format_block(
                    "`do` can only come in a `while` or `case` block",
                    block,
                    loc,
                ))?,
            },

            KeywordType::Let => todo!(),
            KeywordType::Case => todo!(),

            KeywordType::Colon => match self.pop_block(key, loc)? {
                Op { op_type: OpType::CaseStart, .. } => todo!(),
                block => Err(format_block(
                    "`:` can only be used on word or `case` block definition",
                    block,
                    loc,
                ))?,
            },

            KeywordType::If => self.push_block((OpType::IfStart, loc).into()),

            KeywordType::Else => match self.pop_block(key, loc)? {
                Op { op_type: OpType::IfStart, .. } => self.push_block((OpType::Else, loc).into()),
                Op { op_type: OpType::CaseOption, .. } => todo!(),
                block => {
                    Err(format_block("`else` can only come in a `if` or `case` block", block, loc))?
                }
            },

            KeywordType::End => match self.pop_block(key, loc)? {
                Op { op_type: OpType::IfStart, .. } => (OpType::EndIf, loc).into(),
                Op { op_type: OpType::Else, .. } => (OpType::EndElse, loc).into(),
                Op { op_type: OpType::Do, .. } => (OpType::EndWhile, loc).into(),

                Op { op_type: OpType::BindStack, .. } => todo!(),
                Op { op_type: OpType::CaseOption, .. } => todo!(),

                Op { op_type: OpType::PrepProc, operand, .. } => {
                    self.exit_proc();
                    (OpType::EndProc, operand, loc).into()
                }

                block => Err(format_block("Expected `end` to close a valid block", block, loc))?,
            },

            KeywordType::Include |
            KeywordType::Arrow |
            KeywordType::Proc |
            KeywordType::Mem |
            KeywordType::Struct => {
                let error = format!("Keyword type is not valid here: `{:?}`", key);
                return error_loc(&error, loc).into();
            }
        };

        OptionErr::new(vec![op])
    }

    /// Creates a logic block starting from the given [`Op`].
    fn push_block(&mut self, op: Op) -> Op {
        self.name_scopes.push(op.clone());
        op
    }

    /// Pops the last opened logic block, returning a clone of the [`Op`] that started it.
    ///
    /// # Errors
    ///
    /// This function will return an error if no block is open.
    fn pop_block(&mut self, closing_type: KeywordType, loc: Loc) -> LazyResult<Op> {
        self.name_scopes.pop().with_err_ctx(move || {
            let error = format!("There are no open blocks to close with `{:?}`", closing_type);
            error_loc(&error, loc)
        })
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
            .map(|index| Op::new(OpType::PushBind, index, word.loc).into())
    }

    /// Searches for a `mem` that matches the given [`word`][LocWord]
    /// on the current [`Proc`].
    fn get_local_mem(&self, word: &LocWord, prog: &Program) -> Option<Vec<Op>> {
        self.current_proc(prog)
            .and_then(|proc| proc.local_mem_names.iter().find(|mem| word == mem.as_str()))
            .map(|local| Op::new(OpType::PushLocalMem, local.offset(), word.loc).into())
    }

    /// Searches for a `variable` that matches the given [`word`][LocWord]
    /// and parses `store` and `pointer` information.
    fn get_variable(
        &self, word: &LocWord, var_typ: Option<VarWordType>, prog: &Program,
    ) -> OptionErr<Vec<Op>> {
        use OpType::*;
        choice!(
            OptionErr,
            self.current_proc(prog)
                .map(|proc| &proc.local_vars)
                .map_or_else(OptionErr::default, |vars| self
                    .try_get_var(word, vars, PushLocal, var_typ, prog)),
            self.try_get_var(word, &prog.global_vars, PushGlobal, var_typ, prog),
        )
    }

    fn try_get_var(
        &self, word: &LocWord, vars: &[StructType], push_type: OpType,
        var_typ: Option<VarWordType>, prog: &Program,
    ) -> OptionErr<Vec<Op>> {
        let (store, pointer) = match var_typ {
            Some(VarWordType::Store) => (true, false),
            Some(VarWordType::Pointer) => (false, true),
            _ => Default::default(),
        };

        if word.contains(".") {
            return word.try_get_var_field(vars, push_type, store, pointer);
        }

        let struct_type = vars
            .iter()
            .any(|val| val.name() == word.name)
            .then(|| self.try_get_struct_type(word, prog))
            .flatten()
            .or_return(OptionErr::default)?;

        OptionErr::new(match pointer {
            true => {
                let stk_id = prog.get_data_type_id(struct_type.name()).unwrap() as i32;
                word.get_var_pointer(vars, push_type, stk_id)
            }
            _ => word.get_var_fields(vars, push_type, struct_type, store),
        })
    }

    /// Searches for a `struct` that matches the given `&str`,
    /// returning its type.
    fn try_get_struct_type<'a>(&self, word: &str, prog: &'a Program) -> Option<&'a StructDef> {
        self.structs
            .iter()
            .find(|stk| word == stk.as_str())
            .and_then(|stk| {
                prog.words
                    .get(stk.index())
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
                (1, TokenType::DataType(Value::Int)) => {
                    self.parse_memory(word, prog).try_success()?
                }

                (1, TokenType::Word) => {
                    return choice!(
                        OptionErr,
                        self.parse_proc_ctx(prog.get_word(tok.operand).to_owned(), word, prog),
                        self.parse_struct_ctx(i, word, prog),
                        invalid_token(tok, "context declaration")
                    )
                }

                (_, TokenType::Keyword) => {
                    choice!(
                        OptionErr,
                        self.parse_keyword_ctx(&mut colons, word, tok, prog),
                        self.parse_end_ctx(colons, i, word, prog),
                    )?;
                }

                (0, _) => return self.parse_word_ctx(&tok, word, prog),

                _ => Err(invalid_token(tok, "context declaration"))?,
            }
            i += 1;
        }
        OptionErr::default()
    }

    fn parse_proc_ctx(
        &mut self, found_word: String, word: &LocWord, prog: &mut Program,
    ) -> OptionErr<Vec<Op>> {
        prog.get_type_kind(found_word)
            .or_return(OptionErr::default)?;
        self.parse_procedure(word, prog)
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
                    Ok((_, _)) => {
                        todo(word.loc)?; //Refactor broke stuff

                        // if &result == Value::Any {
                        //     Err(error_loc("Undefined variable value is not allowed", word.loc))?;
                        // }

                        // self.skip(eval);
                        // let struct_word = ValueType::new(word.to_string(), &result);
                        // self.register_var(struct_word, prog);

                        success!();
                    }
                    Err(either) => match either {
                        Either::Left(tok) => Err(invalid_token(tok, "context declaration"))?,
                        Either::Right(err) => Err(err)?,
                    },
                }
            }

            (_, KeywordType::Colon) => {
                *colons += 1;
                OptionErr::default()
            }

            (_, KeywordType::End) => {
                let error = format!(
                    "Missing body or contract necessary to infer the type of the word: `{}`",
                    word.name
                );
                return error_loc(&error, word.loc).into();
            }

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
            if &eval != Value::Any {
                self.skip(skip);
                prog.consts
                    .push(ValueType::new(word.to_string(), &eval).into());
                self.name_scopes
                    .register(word.to_string(), ParseContext::ConstStruct);
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
    ) -> LazyResult<Op> {
        let loc = name.loc;

        if self.inside_proc() {
            Err(error_loc("Cannot define a procedure inside of another procedure", loc))?;
        };

        let proc = Proc::new(name, contract);
        let operand = prog.procs.len();
        self.enter_proc(operand);
        prog.procs.push(proc);
        self.name_scopes
            .register(name.to_string(), ParseContext::ProcName);

        Ok(self.push_block(Op::new(OpType::PrepProc, operand as i32, loc)))
    }

    fn parse_procedure(&mut self, word: &LocWord, prog: &mut Program) -> OptionErr<Vec<Op>> {
        let mut ins = Vec::new();
        let mut outs = Vec::new();
        let mut arrow = false;
        let loc = word.loc;

        self.expect_keyword(KeywordType::Colon, "`:` after keyword `proc`", loc)?;
        let tok_err = "proc contract or `:` after procedure definition";

        while let Some(tok) = self.next() {
            if let Some(key) = tok.get_keyword() {
                match key {
                    KeywordType::Arrow => {
                        if arrow {
                            let error = "Duplicated `->` found on procedure definition";
                            return error_loc(error, loc).into();
                        }
                        arrow = true;
                    }
                    KeywordType::Colon => {
                        (self.define_proc(word, Contract::new(ins, outs), prog)).into_success()?
                    }
                    _ => return invalid_option(Some(tok), tok_err, loc).into(),
                }
            } else {
                let Some(kind) = prog.get_kind_from_token(&tok) else {
                    return invalid_option(Some(tok), tok_err, loc).into();
                };

                match kind {
                    Either::Left(stk) => {
                        for member in stk.members() {
                            push_by_condition(
                                arrow,
                                member.get_value().get_type(),
                                &mut outs,
                                &mut ins,
                            );
                        }
                    }
                    Either::Right(type_ptr) => {
                        push_by_condition(arrow, type_ptr, &mut outs, &mut ins)
                    }
                }
            }
        }
        invalid_option(None, tok_err, word.loc).into()
    }

    fn parse_memory(&mut self, word: &LocWord, prog: &mut Program) -> LazyResult<()> {
        let loc = word.loc;

        self.expect_keyword(KeywordType::Colon, "`:` after `mem`", loc)?;
        let value = self.expect_by(|tok| tok == Value::Int, "memory size after `:`", loc)?;
        self.expect_keyword(KeywordType::End, "`end` after memory size", loc)?;

        let size = ((value.operand + 3) / 4) * 4;
        let ctx = prog.push_mem_by_context(self.get_index(), word, size);
        self.name_scopes.register(word.to_string(), ctx);

        Ok(())
    }

    fn parse_struct(&mut self, word: &LocWord, prog: &mut Program) -> OptionErr<Vec<Op>> {
        let loc = word.loc;

        self.expect_keyword(KeywordType::Colon, "`:` after keyword `struct`", loc)?;
        let mut members = Vec::new();

        while let Some(tok) = self.ir_tokens.get(0) {
            if tok.token_type == TokenType::Keyword {
                self.expect_keyword(KeywordType::End, "`end` after struct declaration", loc)?;
                prog.structs_types
                    .push(StructDef::new(word.to_string(), members));
                success!();
            }

            let next = self.expect_by(|n| n == TokenType::Word, "struct member name", loc)?;
            let name_type = self.expect_by(|n| n == TokenType::Word, "struct member type", loc)?;

            let found_type = prog.get_word(name_type.operand).to_owned();
            let Some(type_kind) = prog.get_type_kind(found_type) else {
                return invalid_option(Some(name_type), "struct member type", loc).into()
            };

            let found_word = prog.get_word(next.operand).to_owned();

            match type_kind {
                Either::Left(stk_typ) => {
                    let ref_members = stk_typ.members();

                    if ref_members.len() == 1 {
                        let typ = ref_members[0].get_value();
                        members.push(StructType::Unit((found_word, typ).into()));
                    } else {
                        todo(word.loc)?; //Refactor broke stuff

                        // for member in ref_members {
                        //     let member = member.get_value();
                        //     let member_name = format!("{}.{}", found_word, member.name());
                        //     members.push(StructType::Unit((member_name, member).into()));
                        // }
                    }
                }
                Either::Right(typ_ptr) => {
                    members.push(StructType::Unit((found_word, typ_ptr).into()))
                }
            }
            continue;
        }

        error_loc("Expected struct members or `end` after struct declaration", loc).into()
    }

    fn parse_const_or_var(
        &mut self, word: &LocWord, operand: i32, stk: StructDef, prog: &mut Program,
    ) -> LazyResult<()> {
        self.expect_keyword(KeywordType::Colon, "`:` after variable type definition", word.loc)?;

        let assign = self
            .expect_by(
                |tok| equals_any!(tok, KeywordType::Colon, KeywordType::Equal),
                "`:` or `=` after keyword `:`",
                word.loc,
            )?
            .get_keyword()
            .unwrap();

        let (mut result, eval) = match self.compile_eval_n(stk.members().len(), prog).value {
            Ok(value) => value,
            Err(either) => match either {
                Either::Right(err) => Err(err)?,
                Either::Left(tok) => Err(error_loc(
                    "Failed to parse an valid struct value at compile-time evaluation",
                    tok.loc,
                ))?,
            },
        };

        let end_token = self.skip(eval).unwrap();
        let mut members = stk.members().to_vec();

        if result.len() == 1 {
            let eval = result.pop().unwrap();

            if &eval == Value::Any {
                if self.inside_proc() {
                    members.reverse();
                }

                let struct_word = StructDef::new(word.name.to_owned(), members);
                self.register_const(&assign, StructType::Root(struct_word), prog);
            } else {
                let member_type = members.last().unwrap().get_value().get_type();
                if !equals_any!(member_type, Value::Any, eval.token_type) {
                    lazybail!(
                        |f| concat!(
                            "{}Expected type `{}` on the stack at the end of ",
                            "the compile-time evaluation, but found: `{}`"
                        ),
                        f.format(Fmt::Loc(end_token.loc)),
                        f.format(Fmt::Typ(member_type)),
                        f.format(Fmt::Typ(eval.token_type))
                    )
                }

                let struct_word = ValueType::new(word.to_string(), &eval);
                self.register_const(&assign, StructType::Unit(struct_word), prog);
            }
        } else {
            let contract: Vec<TokenType> = members.iter().map(Typed::get_type).collect();
            result.expect_exact(&contract, end_token.loc)?;

            if self.inside_proc() {
                members.reverse();
                if assign == KeywordType::Equal {
                    result.reverse()
                }
            }
            let mut def_members = vec![];
            let mut item = result.into_iter();

            for member in members {
                match member {
                    StructType::Unit(u) => {
                        let value = ValueType::new(u.name().to_owned(), &item.next().unwrap());
                        def_members.push(StructType::Unit(value));
                    }
                    _ => todo(word.loc)?, //Refactor broke stuff
                }
            }

            let struct_word = StructType::Root(StructDef::new(word.name.to_owned(), def_members));
            self.register_const(&assign, struct_word, prog);
        }

        let ctx = match assign {
            KeywordType::Colon => ParseContext::ConstStruct,
            KeywordType::Equal => ParseContext::Variable,
            _ => unreachable!(),
        };

        self.name_scopes.register(word.to_string(), ctx);
        self.structs.push(IndexWord::new(word, operand as usize));
        Ok(())
    }

    pub fn lex_file(&mut self, path: &PathBuf, prog: &mut Program) -> LazyResult<&mut Self> {
        let lex = &mut match prog.new_lexer(path) {
            Ok(ok) => ok,
            Err(err) => Err(err)?,
        };

        while let Some(token) = prog.lex_next_token(lex).value? {
            if &token != KeywordType::Include {
                self.ir_tokens.push_back(token);
                continue;
            }

            let tok = expect_token_by(
                prog.lex_next_token(lex).value?,
                |tok| tok == TokenType::Str,
                "include file name",
                token.loc,
            )?;

            let include_path = prog.get_string(tok.operand).as_str();

            let include = get_dir(path)
                .with_ctx(|_| format!("failed to get file directory path"))?
                .join(include_path);

            info!("Including file: {:?}", include);
            self.lex_file(&include, prog)?;
        }
        Ok(self)
    }
}

impl LocWord {
    fn get_var_pointer(&self, vars: &[StructType], push_type: OpType, stk_id: i32) -> Vec<Op> {
        let index = get_field_pos(vars, &self).unwrap().0 as i32 + 1;

        vec![
            (Op::new(push_type, index as i32, self.loc)),
            Op::from((IntrinsicType::Cast(-stk_id), self.loc)),
        ]
    }

    fn get_var_fields(
        &self, vars: &[StructType], push_type: OpType, struct_type: &StructDef, store: bool,
    ) -> Vec<Op> {
        let mut result = Vec::new();
        let loc = self.loc;

        let index = get_field_pos(vars, self).unwrap().0 as i32 + 1;

        let mut members = struct_type.units();
        if store {
            members.reverse();
        }

        let is_local = push_type == OpType::PushLocal;
        let (index, step) = fold_bool!(is_local == store, (index - 1, 1), (index, -1));
        let id_range = range_step_from(index, step);

        let members = members.iter().map(|m| m.get_type()).map(i32::from);

        for (operand, type_id) in id_range.zip(members) {
            if store {
                result.push(Op::new(OpType::ExpectType, type_id, loc))
            }

            result.push(Op::new(push_type, operand, loc));

            if store {
                result.push(Op::from((IntrinsicType::Store32, loc)))
            } else {
                result.push(Op::from((IntrinsicType::Load32, loc)));
                result.push(Op::from((IntrinsicType::Cast(type_id), loc)));
            }
        }

        result
    }

    fn try_get_var_field(
        &self, vars: &[StructType], push_type: OpType, store: bool, pointer: bool,
    ) -> OptionErr<Vec<Op>> {
        let fields: Vec<_> = self.name.split(".").collect();
        let (index, i) = get_field_pos(vars, fields[0]).or_return(OptionErr::default)?;

        let mut fields = fields.into_iter().skip(1);
        let mut offset = index as i32;
        let mut var = &vars[i];
        let loc = self.loc;

        while let Some(v) = fields.next() {
            let StructType::Root(root) = var else {
                return todo(loc).unwrap_err().into();
            };

            let Some(pos) = root.members().iter().position(|m| m.name().eq(v)) else {
                let error =
                    format!("The variable `{}` does not contain the field `{v}`", var.name());
                return error_loc(&error, loc).into();
            };

            offset += pos as i32;
            var = &root.members()[pos];
        }

        let StructType::Unit(typ) = var else {
            //Not possible to `.` access a struct root yet, maybe unpack all fields?
            return todo(loc).unwrap_err().into();
        };

        let type_id = i32::from(typ.get_type());
        let mut result = Vec::new();

        if store {
            result.push(Op::new(OpType::ExpectType, type_id, loc))
        }

        result.push(Op::new(push_type, offset, loc));

        if store {
            result.push(Op::from((IntrinsicType::Store32, loc)))
        } else if pointer {
            result.push(Op::from((IntrinsicType::Cast(-type_id), loc)));
        } else {
            result.push(Op::from((IntrinsicType::Load32, loc)));
            result.push(Op::from((IntrinsicType::Cast(type_id), loc)));
        }

        OptionErr::new(result)
    }
}

impl Program {
    /// Searches for a `global mem` that matches the given [`LocWord`] name.
    fn get_global_mem(&self, word: &LocWord) -> Option<Vec<Op>> {
        self.get_memory()
            .iter()
            .find(|mem| mem.as_str() == word.as_str())
            .map(|global| Op::new(OpType::PushGlobalMem, global.offset(), word.loc).into())
    }

    /// Searches for a `const` that matches the given[`word`][LocWord]
    /// and parses it to an [`Op`].
    fn get_const_struct(&self, word: &LocWord) -> Option<Vec<Op>> {
        self.get_const_by_name(word).map(|tword| match tword {
            StructType::Root(r) => r.units().iter().map(|&a| Op::from((a, word.loc))).collect(),
            _ => vec![Op::from((tword, word.loc))],
        })
    }

    fn get_intrinsic(&self, word: &LocWord) -> Option<Vec<Op>> {
        self.get_intrinsic_type(word)
            .map(|i| vec![Op::from((i, word.loc))])
    }

    fn get_proc_name(&self, word: &LocWord) -> Option<Vec<Op>> {
        self.procs
            .iter()
            .position(|proc| word == proc.name.as_str())
            .map(|index| vec![Op::new(OpType::Call, index as i32, word.loc)])
    }

    fn get_type_kind(&self, type_name: String) -> Option<Either<&StructDef, TokenType>> {
        match self.get_type_name(&type_name) {
            Some(stk_typ) => Some(Either::Left(stk_typ)),
            None => self
                .get_data_pointer(&type_name)
                .map(|typ_ptr| Either::Right(typ_ptr)),
        }
    }

    fn get_type_name(&self, word: &str) -> Option<&StructDef> {
        self.structs_types.iter().find(|s| s.name() == word)
    }

    fn get_data_pointer(&self, word: &str) -> Option<TokenType> {
        word.strip_prefix('*')
            .and_then(|word| self.get_data_type_id(word))
            .map(|i| TokenType::DataPtr(Value::from(i - 1)))
    }

    fn get_struct_type(&self, tok: &IRToken) -> Option<&StructDef> {
        fold_bool!(tok == TokenType::Word, self.get_type_name(self.get_word(tok.operand)))
    }

    fn get_kind_from_token(&self, tok: &IRToken) -> Option<Either<&StructDef, TokenType>> {
        fold_bool!(
            tok == TokenType::Word,
            self.get_type_kind(self.get_word(tok.operand).to_string())
        )
    }

    fn push_mem_by_context(
        &mut self, proc_index: Option<usize>, word: &str, size: i32,
    ) -> ParseContext {
        match proc_index.and_then(|i| self.procs.get_mut(i)) {
            Some(proc) => {
                proc.push_mem(word, size);
                ParseContext::LocalMem
            }
            None => {
                self.push_mem(word, size);
                ParseContext::GlobalMem
            }
        }
    }

    pub fn compile_file(&mut self, path: &PathBuf) -> firelib::anyhow::Result<&mut Self> {
        info!("Compiling file: {:?}", path);

        Parser::new()
            .lex_file(path, self)
            .and_then(|parser| parser.parse_tokens(self))
            .try_or_apply(&|fmt| self.format(fmt))?;

        info!("Compilation done");
        Ok(self)
    }
}
use std::{collections::VecDeque, path::PathBuf};

use either::Either;
use firelib::{lazy::LazyCtx, lexer::Loc, utils::*, ShortCircuit, TrySuccess};

use super::{types::*, utils::*};
use crate::compiler::{
    program::*,
    typechecking::expect::Compare,
    types::{core::*, data::*, enums::*, proc::*},
    utils::err_loc,
};

#[derive(Default)]
pub struct Parser {
    ir_tokens: VecDeque<IRToken>,
    name_scopes: NameScopes,
    structs: Vec<IndexWord>,
    current_proc: Option<usize>,
}

impl Visitor for Parser {
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

    pub fn peek(&self) -> Option<&IRToken> {
        self.ir_tokens.get(0)
    }

    pub fn get(&self, index: usize) -> Option<&IRToken> {
        self.ir_tokens.get(index)
    }

    pub fn get_cloned(&self, index: usize) -> Option<IRToken> {
        self.ir_tokens.get(index).cloned()
    }

    /// Pops and returns the next [`IRToken`] of this [`Parser`].
    pub fn next(&mut self) -> Option<IRToken> {
        self.ir_tokens.pop_front()
    }

    /// Pops `n` elements and returns the last [`IRToken`] of this [`Parser`].
    pub fn skip(&mut self, n: usize) -> Option<IRToken> {
        let mut result = None;
        for _ in 0..n {
            result = self.next();
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
                prog.ops.append(&mut op);
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
            );
        };

        let op = Op::from(match tok.token_type {
            TokenType::Keyword => return self.define_keyword_op(&tok, prog),

            TokenType::Str => (OpType::PushStr, &tok, tok.loc),

            TokenType::Data(data) => match data {
                ValueType::Typ(val) => match val {
                    Value::Type(_) => lazybail!(
                        |f| "{}Value type not valid here: `{}`",
                        f.format(Fmt::Loc(tok.loc)),
                        f.format(Fmt::Typ(val.get_type()))
                    ),
                    _ => (OpType::PushData(val), &tok, tok.loc),
                },
                ValueType::Ptr(ptr) => lazybail!(
                    |f| "{}Data pointer type not valid here: `{}`",
                    f.format(Fmt::Loc(tok.loc)),
                    f.format(Fmt::Typ(ptr.get_type()))
                ),
            },

            TokenType::Word => {
                let word = &LocWord::new(&tok, tok.loc);

                choice!(
                    OptionErr,
                    prog.get_intrinsic(word),
                    self.lookup_context(word, prog),
                    self.define_context(word, prog)
                )?;

                let error =
                    format!("Word was not declared on the program: `{}`", word.as_str(prog));
                return err_loc(error, tok.loc).into();
            }
        });

        OptionErr::new(vec![op])
    }

    fn lookup_context(&self, word: &LocWord, prog: &mut Program) -> OptionErr<Vec<Op>> {
        let Some(ctx) = self.name_scopes.lookup(word, prog) else {
            return self.lookup_modfied_var(word, prog);
        };

        match ctx {
            ParseContext::ProcName => prog.get_proc_name(word),
            ParseContext::GlobalMem => prog.get_global_mem(word),
            ParseContext::Binding => self.get_binding(word, prog),
            ParseContext::LocalMem => self.get_local_mem(word, prog),
            ParseContext::ConstStruct => prog.get_const_struct(word),
            ParseContext::LocalVar => return self.get_local_var(word, None, prog),
            ParseContext::GlobalVar => return self.get_global_var(word, None, prog),
        }
        .into()
    }

    fn lookup_modfied_var(&self, word: &LocWord, prog: &mut Program) -> OptionErr<Vec<Op>> {
        let (rest, var_typ) = match word.as_str(prog).split_at(1) {
            ("!", rest) => (rest, VarWordType::Store),
            (".", rest) => {
                let Some(key) = prog.get_key(rest) else {
                    todo!()
                };

                let op = Op::new(OpType::OffsetLoad, key.operand(), word.loc);

                return OptionErr::new(vec![op]);
            }
            _ => return OptionErr::default(),
        };

        let Some(key) = prog.get_key(rest) else {
            todo!()
        };

        let local = match self.name_scopes.lookup(&key, prog) {
            Some(ParseContext::LocalVar) => true,
            Some(ParseContext::GlobalVar) => false,
            _ => return OptionErr::default(),
        };

        let word = LocWord::new(key, word.loc);

        if local {
            self.get_local_var(&word, Some(var_typ), prog)
        } else {
            self.get_global_var(&word, Some(var_typ), prog)
        }
    }

    fn define_keyword_op(&mut self, tok: &IRToken, prog: &Program) -> OptionErr<Vec<Op>> {
        let key = tok.as_keyword();
        let loc = tok.loc;

        let op = match key {
            KeywordType::Drop => (OpType::Drop, loc).into(),
            KeywordType::Dup => (OpType::Dup, loc).into(),
            KeywordType::Swap => (OpType::Swap, loc).into(),
            KeywordType::Over => (OpType::Over, loc).into(),
            KeywordType::Rot => (OpType::Rot, loc).into(),
            KeywordType::Equal => (OpType::Equal, loc).into(),
            KeywordType::At => (OpType::Unpack, loc).into(),

            KeywordType::Dot => {
                let Some(next_token) = self.next() else {
                    todo!()
                };

                match next_token.token_type {
                    TokenType::Keyword => {
                        expect_token_by(
                            Some(next_token),
                            |t| t == KeywordType::Ref,
                            "word or `*` after `.`",
                            loc,
                        )?;
                        let word = self.expect_word("word after `.*`", loc)?;
                        (OpType::Offset, word, loc).into()
                    }
                    TokenType::Word => (OpType::OffsetLoad, next_token, loc).into(),
                    _ => todo!(),
                }
            }

            KeywordType::Ref => {
                let ref_word = &self.expect_word("type after `*`", loc)?;
                let var_typ = Some(VarWordType::Pointer);

                return match self.name_scopes.lookup(ref_word, prog) {
                    Some(ParseContext::LocalVar) => self.get_local_var(ref_word, var_typ, prog),
                    Some(ParseContext::GlobalVar) => self.get_global_var(ref_word, var_typ, prog),
                    _ => todo!(),
                };
            }

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

                Op { op_type: OpType::PrepInline, operand, .. } => {
                    self.exit_proc();
                    (OpType::EndInline, operand, loc).into()
                }

                block => Err(format_block("Expected `end` to close a valid block", block, loc))?,
            },

            KeywordType::Include |
            KeywordType::Inline |
            KeywordType::Arrow |
            KeywordType::Proc |
            KeywordType::Mem |
            KeywordType::Struct => {
                let error = format!("Keyword type is not valid here: `{key:?}`");
                return err_loc(error, loc).into();
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
            err_loc(format!("There are no open blocks to close with `{closing_type:?}`"), loc)
        })
    }

    /// Searches for a `binding` that matches the given [`word`][LocWord]
    /// on the current [`Proc`].
    fn get_binding(&self, word: &LocWord, prog: &Program) -> Option<Vec<Op>> {
        self.current_proc_data(prog)
            .and_then(|proc| {
                proc.bindings
                    .iter()
                    .position(|bind| word.eq(bind))
                    .map(|index| proc.bindings.len() - 1 - index)
            })
            .map(|index| vec![Op::new(OpType::PushBind, index, word.loc)])
    }

    /// Searches for a `mem` that matches the given [`word`][LocWord]
    /// on the current [`Proc`].
    fn get_local_mem(&self, word: &LocWord, prog: &Program) -> Option<Vec<Op>> {
        self.current_proc_data(prog)
            .and_then(|proc| proc.local_mems.iter().find(|mem| word.eq(mem)))
            .map(|local| vec![Op::new(OpType::PushLocalMem, local.offset(), word.loc)])
    }

    /// Searches for a local `variable` that matches the given [`word`][LocWord]
    /// and parses `store` and `pointer` information.
    fn get_local_var(
        &self, word: &LocWord, var_typ: Option<VarWordType>, prog: &Program,
    ) -> OptionErr<Vec<Op>> {
        let proc = self.current_proc_data(prog).or_return(OptionErr::default)?;
        self.try_get_var(word, &proc.local_vars, OpType::PushLocal, var_typ, prog)
    }

    /// Searches for a global `variable` that matches the given [`word`][LocWord]
    /// and parses `store` and `pointer` information.
    fn get_global_var(
        &self, word: &LocWord, var_typ: Option<VarWordType>, prog: &Program,
    ) -> OptionErr<Vec<Op>> {
        self.try_get_var(word, &prog.global_vars, OpType::PushGlobal, var_typ, prog)
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

        if word.as_str(prog).contains('.') {
            return prog.try_get_var_field(word, vars, push_type, store, pointer);
        }

        let struct_type = vars
            .iter()
            .any(|val| word.eq(val))
            .then(|| self.try_get_struct_type(word, prog))
            .flatten()
            .or_return(OptionErr::default)?;

        OptionErr::new(if pointer {
            let stk_id = prog.get_struct_type_id(struct_type).unwrap() as i32;
            vars.get_pointer(word, push_type, stk_id)
        } else {
            vars.get_fields(word, push_type, struct_type, store)
        })
    }

    /// Searches for a `struct` that matches the given [`StrKey`],
    /// returning its type.
    fn try_get_struct_type<'a>(&self, word: &StrKey, prog: &'a Program) -> Option<&'a StructDef> {
        self.structs
            .iter()
            .find(|stk| word.eq(stk))
            .and_then(|stk| prog.get_type_def(stk))
    }

    /// Tries to define the parsing context to use
    /// based on the preceding [`IRTokens`][IRToken].
    fn define_context(&mut self, word: &LocWord, prog: &mut Program) -> OptionErr<Vec<Op>> {
        let mut i = 0;
        let mut colons = 0;
        while let Some(tok) = self.get(i) {
            match (colons, tok.token_type) {
                (1, TokenType::Data(ValueType::Typ(Value::Int))) => {
                    self.parse_memory(word, prog).try_success()?;
                }

                (1, TokenType::Word) => {
                    return choice!(
                        OptionErr,
                        self.parse_proc_ctx(i, word, prog),
                        self.parse_struct_ctx(i, word, prog),
                        invalid_token(self.get_cloned(i).unwrap(), "context declaration")
                    )
                }

                (_, TokenType::Keyword) => {
                    choice!(
                        OptionErr,
                        self.parse_keyword_ctx(&mut colons, i, word, prog),
                        self.parse_end_ctx(colons, i, word, prog),
                    )?;
                }

                (0, TokenType::Word) => return self.parse_word_ctx(i, word, prog),

                _ => Err(invalid_token(tok.clone(), "context declaration"))?,
            }
            i += 1;
        }
        OptionErr::default()
    }

    fn parse_proc_ctx(
        &mut self, index: usize, word: &LocWord, prog: &mut Program,
    ) -> OptionErr<Vec<Op>> {
        let tok = self.get(index).unwrap();
        prog.get_type_def(tok).or_return(OptionErr::default)?;
        self.parse_procedure(word, prog, false)
    }

    fn parse_struct_ctx(
        &mut self, top_index: usize, word: &LocWord, prog: &mut Program,
    ) -> OptionErr<Vec<Op>> {
        if let Ok([n1, n2]) = self.ir_tokens.get_range_ref(top_index + 1) {
            if prog.try_get_struct_def(n1).is_some() &&
                equals_any!(n2, KeywordType::End, TokenType::Word)
            {
                return self.parse_struct(word, prog);
            }
        }
        OptionErr::default()
    }

    fn parse_keyword_ctx(
        &mut self, colons: &mut i32, index: usize, word: &LocWord, prog: &mut Program,
    ) -> OptionErr<Vec<Op>> {
        match (*colons, self.get(index).unwrap().as_keyword()) {
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
                self.parse_procedure(word, prog, false)
            }

            (0, KeywordType::Inline) => {
                self.next();
                self.parse_procedure(word, prog, true)
            }

            (0, KeywordType::Ref) => {
                self.next();
                let ref_word = self.expect_word("type after `*`", word.loc)?;

                let Some(type_ptr) = prog.get_type_ptr(&ref_word) else {
                    return unexpected_token(ref_word.into(), "struct type").into();
                };

                let name = format!("*{}", ref_word.as_str(prog));
                let word_id = prog.get_or_intern(&name);

                let value = ValueUnit::new(&StrKey::default(), type_ptr);
                let stk = StructDef::new(&word_id, vec![StructType::Unit(value)]);

                prog.structs_types.push(stk.clone()); //Todo: Check if the `*` type is already registered

                self.parse_const_or_var(word, word_id, stk, prog)
                    .try_success()?;
            }

            (1, KeywordType::Equal) => {
                self.skip(2);
                match self.compile_eval(prog).value {
                    Ok((_, _)) => {
                        todo!("Refactor broke stuff");

                        // if &result == Value::Any {
                        //     Err(error_loc("Undefined variable value is not allowed", word.loc))?;
                        // }

                        // self.skip(eval);
                        // let struct_word = ValueType::new(word.to_string(), &result);
                        // self.register_var(struct_word, prog);
                        // success!();
                    }
                    Err(either) => match either {
                        Either::Left(tok) => invalid_token(tok, "context declaration").into(),
                        Either::Right(err) => err.into(),
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
                    word.as_str(prog)
                );
                err_loc(error, word.loc).into()
            }

            _ => OptionErr::default(),
        }
    }

    fn parse_end_ctx(
        &mut self, colons: i32, ctx_size: usize, word: &LocWord, prog: &mut Program,
    ) -> OptionErr<Vec<Op>> {
        (colons == 2).or_return(OptionErr::default)?;

        self.parse_static_ctx(ctx_size, word, prog)?;
        self.define_proc(word, Contract::default(), prog, false)
            .into_success()?
    }

    fn parse_static_ctx(
        &mut self, ctx_size: usize, word: &LocWord, prog: &mut Program,
    ) -> OptionErr<Vec<Op>> {
        (ctx_size == 1).or_return(|| self.parse_procedure(word, prog, false))?;

        self.skip(2);
        if let Ok((eval, skip)) = self.compile_eval(prog).value {
            if &eval != Value::Any {
                self.skip(skip);
                let value = ValueUnit::new(word, &eval).into();
                prog.consts.push(value);

                self.name_scopes.register(word, ParseContext::ConstStruct);
                success!();
            }
        }
        OptionErr::default()
    }

    fn parse_word_ctx(
        &mut self, index: usize, word: &LocWord, prog: &mut Program,
    ) -> OptionErr<Vec<Op>> {
        let tok = self.get_cloned(index).unwrap();

        let Some(stk) = prog.try_get_struct_def(&tok).cloned() else {
            return unexpected_token(tok, "struct type").into();
        };

        self.next();
        self.parse_const_or_var(word, tok, stk, prog)
            .try_success()?;
    }

    fn define_proc(
        &mut self, name: &LocWord, contract: Contract, prog: &mut Program, inline: bool,
    ) -> LazyResult<Op> {
        let loc = name.loc;

        if self.inside_proc() {
            let error = "Cannot define a procedure inside of another procedure";
            return Err(err_loc(error, loc));
        };

        let inline_ip = fold_bool!(inline, Some(prog.ops.len()), None);
        let proc = Proc::new(name, contract, inline_ip);

        let operand = prog.procs.len();
        self.enter_proc(operand);
        prog.procs.push(proc);

        self.name_scopes.register(name, ParseContext::ProcName);

        let prep = fold_bool!(inline, OpType::PrepInline, OpType::PrepProc);
        Ok(self.push_block(Op::new(prep, operand as i32, loc)))
    }

    fn parse_procedure(
        &mut self, word: &LocWord, prog: &mut Program, include: bool,
    ) -> OptionErr<Vec<Op>> {
        let mut ins = Vec::new();
        let mut outs = Vec::new();
        let mut arrow = false;
        let loc = word.loc;

        self.expect_keyword(KeywordType::Colon, "`:` after procedure declaration", loc)?;
        let tok_err = "proc contract or `:` after procedure definition";

        while let Some(tok) = self.next() {
            match tok.token_type {
                TokenType::Keyword => match tok.as_keyword() {
                    KeywordType::Arrow => {
                        if arrow {
                            let error = "Duplicated `->` found on procedure definition";
                            return err_loc(error, loc).into();
                        }
                        arrow = true;
                    }

                    KeywordType::Colon => self
                        .define_proc(word, Contract::new(ins, outs), prog, include)
                        .into_success()?,

                    KeywordType::Ref => {
                        let typ = self.expect_word("type after `*`", loc)?;

                        let Some(type_ptr) = prog.get_type_ptr(&typ) else {
                            return unexpected_token(typ.into(), tok_err).into();
                        };

                        push_by_condition(arrow, type_ptr.get_type(), &mut outs, &mut ins);
                    }

                    _ => return unexpected_token(tok, tok_err).into(),
                },
                TokenType::Word => {
                    let Some(type_def) = prog.get_type_def(&tok) else {
                        return unexpected_token(tok, tok_err).into();
                    };

                    for typ in type_def.units().iter().map(Typed::get_type) {
                        push_by_condition(arrow, typ, &mut outs, &mut ins);
                    }
                }
                _ => return unexpected_token(tok, tok_err).into(),
            }
        }
        unexpected_end(tok_err, loc).into()
    }

    fn parse_memory(&mut self, word: &LocWord, prog: &mut Program) -> LazyResult<()> {
        let loc = word.loc;

        self.expect_keyword(KeywordType::Colon, "`:` after `mem`", loc)?;
        let value = self.expect_by(|tok| tok == Value::Int, "memory size after `:`", loc)?;
        self.expect_keyword(KeywordType::End, "`end` after memory size", loc)?;

        let size = word_aligned(value) as usize;
        let ctx = prog.push_mem_by_context(self.get_index(), word, size);
        self.name_scopes.register(word, ctx);

        Ok(())
    }

    fn parse_struct(&mut self, word: &LocWord, prog: &mut Program) -> OptionErr<Vec<Op>> {
        let loc = word.loc;

        self.expect_keyword(KeywordType::Colon, "`:` after keyword `struct`", loc)?;
        let mut members = Vec::new();

        while let Some(tok) = self.peek() {
            if tok.token_type == TokenType::Keyword {
                self.expect_keyword(KeywordType::End, "`end` after struct declaration", loc)?;
                prog.structs_types.push(StructDef::new(word, members));
                success!();
            }

            let member_name = self.expect_word("struct member name", loc)?;
            let name_type = self.expect_word("struct member type", loc)?;
            let as_ref = false; // Todo: Add pointers support

            let Some(type_kind) = prog.get_type_kind(&name_type, as_ref) else {
                return unexpected_token(name_type.into(), "struct member type").into()
            };

            match type_kind {
                Either::Left(type_def) => {
                    let ref_members = type_def.members();

                    if ref_members.len() == 1 {
                        let StructType::Unit(typ) = &ref_members[0] else {
                            todo!()
                        };

                        members.push(StructType::Unit((member_name.str_key(), typ).into()));
                    } else {
                        let value = prog.get_struct_value_id(type_def).unwrap();
                        let root = StructRef::new(&member_name, ref_members.to_vec(), value);
                        members.push(StructType::Root(root));
                    }
                }
                Either::Right(type_ptr) => {
                    members.push(StructType::Unit((member_name.str_key(), type_ptr).into()));
                }
            }
        }

        err_loc("Expected struct members or `end` after struct declaration", loc).into()
    }

    fn parse_const_or_var<O: Operand>(
        &mut self, word: &LocWord, word_id: O, stk: StructDef, prog: &mut Program,
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

        let (mut result, eval) = match self.compile_eval_n(stk.count(), prog).value {
            Ok(value) => value,
            Err(either) => {
                return match either {
                    Either::Right(err) => Err(err),
                    Either::Left(tok) => Err(err_loc(
                        "Failed to parse an valid struct value at compile-time evaluation",
                        tok.loc,
                    )),
                }
            }
        };

        let end_token = self.skip(eval).unwrap();
        let members = if self.inside_proc() {
            Either::Left(stk.members().iter().rev())
        } else {
            Either::Right(stk.members().iter())
        };

        if result.len() == 1 {
            let eval = result.pop().unwrap();

            if &eval == Value::Any {
                let members: Vec<StructType> = members.cloned().collect();
                let value = prog.get_struct_value_id(&stk).unwrap();
                let struct_word = StructRef::new(word, members, value);
                self.register_const_or_var(assign, StructType::Root(struct_word), prog);
            } else {
                let member_type = match members.last().unwrap() {
                    StructType::Root(_) => todo!(),
                    StructType::Unit(typ) => typ.get_type(),
                };

                if !equals_any!(member_type, Value::Any, eval.token_type) {
                    lazybail!(
                        |f| concat!(
                            "{}Expected type `{}` on the stack at the end of ",
                            "the compile-time evaluation, but found: `{}`"
                        ),
                        f.format(Fmt::Loc(end_token.loc)),
                        f.format(Fmt::Typ(member_type)),
                        f.format(Fmt::Typ(eval.token_type))
                    );
                }

                let struct_word = ValueUnit::new(word, &eval);
                self.register_const_or_var(assign, StructType::Unit(struct_word), prog);
            }
        } else {
            let contract: Vec<TokenType> = if self.inside_proc() {
                Either::Left(members.clone().rev())
            } else {
                Either::Right(members.clone())
            }
            .flat_map(StructType::units)
            .map(Typed::get_type)
            .collect();

            result.expect_exact(&contract, end_token.loc)?;

            if self.inside_proc() && assign == KeywordType::Equal {
                result.reverse();
            }

            let mut def_members = vec![];
            let mut item = result.into_iter();

            for member in members {
                match member {
                    StructType::Unit(unit) => {
                        let value = ValueUnit::new(unit, item.next().unwrap());
                        def_members.push(StructType::Unit(value));
                    }
                    StructType::Root(root) => {
                        let mut new = vec![];

                        let root_members = if self.inside_proc() {
                            Either::Left(root.members().iter().rev())
                        } else {
                            Either::Right(root.members().iter())
                        };

                        for member in root_members {
                            let StructType::Unit(typ) = member else {
                                todo!()
                            };

                            let value = ValueUnit::new(typ, item.next().unwrap());
                            new.push(StructType::Unit(value));
                        }

                        let root = StructRef::new(root, new, root.get_ref_type());
                        def_members.push(StructType::Root(root));
                    }
                }
            }

            let value = prog.get_struct_value_id(&stk).unwrap();
            let struct_ref = StructRef::new(word, def_members, value);
            self.register_const_or_var(assign, StructType::Root(struct_ref), prog);
        }

        let ctx = match (assign, self.inside_proc()) {
            (KeywordType::Colon, _) => ParseContext::ConstStruct,
            (KeywordType::Equal, true) => ParseContext::LocalVar,
            (KeywordType::Equal, false) => ParseContext::GlobalVar,
            _ => unreachable!(),
        };

        self.name_scopes.register(word, ctx);
        self.structs.push(IndexWord::new(word, word_id));

        Ok(())
    }

    pub fn register_const_or_var(
        &mut self, assign: KeywordType, struct_word: StructType, prog: &mut Program,
    ) {
        match assign {
            KeywordType::Colon => prog.consts.push(struct_word),
            KeywordType::Equal => self.register_var(struct_word, prog),
            _ => unreachable!(),
        }
    }

    pub fn register_var(&mut self, struct_word: StructType, prog: &mut Program) {
        match self.current_proc_mut(prog) {
            Some(proc) => {
                let Some(data) = proc.get_data_mut() else {
                    todo!();
                };

                data.local_vars.push(struct_word);
            }
            None => prog.global_vars.push(struct_word),
        }
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

            let include_path = prog.get_data_str(tok);

            let include = get_dir(path)
                .with_ctx(|_| "failed to get file directory path".to_string())?
                .join(include_path);

            info!("Including file: {:?}", include);
            self.lex_file(&include, prog)?;
        }
        Ok(self)
    }
}

impl Program {
    fn try_get_var_field(
        &self, word: &LocWord, vars: &[StructType], push_type: OpType, store: bool, pointer: bool,
    ) -> OptionErr<Vec<Op>> {
        let fields: Vec<_> = word.as_str(self).split('.').collect();
        let loc = word.loc;

        let Some(first) = self.get_key(fields[0]) else {
            todo!()
        };

        let (mut offset, i) = vars.get_offset(&first).or_return(OptionErr::default)?;

        let fields = fields.into_iter().skip(1);
        let mut var = &vars[i];

        for field_name in fields {
            let StructType::Root(root) = var else {
                todo!()
            };

            let Some(field_key) = self.get_key(field_name) else {
                todo!()
            };

            let Some((diff, pos)) = root.members().get_offset(&field_key) else {
                let error = format!("The variable `{}` does not contain the field `{field_name}`",
                    var.as_str(self));
                return err_loc(error, loc).into();
            };

            offset += diff;
            var = &root.members()[pos];
        }

        let mut result = Vec::new();

        let typ = match var {
            StructType::Unit(unit) => unit,
            StructType::Root(root) => {
                if store {
                    todo!();
                }

                if push_type == OpType::PushLocal {
                    offset += 1;
                }

                let type_id = root.get_ref_type().operand();

                result.extend([
                    Op::new(push_type, offset as i32, loc),
                    Op::from((IntrinsicType::Cast(-type_id), loc)),
                ]);

                if !pointer {
                    result.push((OpType::Unpack, loc).into());
                }

                return OptionErr::new(result);
            }
        };

        let type_id = typ.value_type().operand();

        if store {
            result.push(Op::new(OpType::ExpectType, type_id, loc));
        }

        result.push(Op::new(push_type, offset as i32, loc));

        if store {
            result.push(Op::from((IntrinsicType::Store32, loc)));
        } else if pointer {
            result.push(Op::from((IntrinsicType::Cast(-type_id), loc)));
        } else {
            result.extend([
                Op::from((IntrinsicType::Load32, loc)),
                Op::from((IntrinsicType::Cast(type_id), loc)),
            ]);
        }

        OptionErr::new(result)
    }

    /// Searches for a `global mem` that matches the given [`LocWord`] name.
    fn get_global_mem(&self, word: &LocWord) -> Option<Vec<Op>> {
        self.get_memory()
            .iter()
            .find(|mem| word.eq(mem))
            .map(|global| vec![Op::new(OpType::PushGlobalMem, global.offset(), word.loc)])
    }

    /// Searches for a `const` that matches the given[`word`][LocWord]
    /// and parses it to an [`Op`].
    fn get_const_struct(&self, word: &LocWord) -> Option<Vec<Op>> {
        self.get_const_by_name(word).map(|tword| match tword {
            StructType::Root(root) => root
                .units()
                .iter()
                .map(|&value| Op::from((value, word.loc)))
                .collect(),
            StructType::Unit(_) => vec![Op::from((tword, word.loc))],
        })
    }

    fn get_intrinsic(&self, word: &LocWord) -> Option<Vec<Op>> {
        self.get_intrinsic_type(word.as_str(self))
            .map(|i| vec![Op::from((i, word.loc))])
    }

    fn get_proc_name(&self, word: &LocWord) -> Option<Vec<Op>> {
        self.procs
            .iter()
            .enumerate()
            .find(|(_, proc)| word.eq(&proc.name))
            .map(|(index, proc)| {
                let call = match &proc.mode {
                    Mode::Inline(..) => OpType::CallInline,
                    Mode::Declare(_) => OpType::Call,
                };
                vec![Op::new(call, index as i32, word.loc)]
            })
    }

    fn get_type_kind(&self, word: &LocWord, as_ref: bool) -> Option<Either<&StructDef, ValueType>> {
        if as_ref {
            self.get_type_ptr(word).map(Either::Right)
        } else {
            self.get_type_def(word).map(Either::Left)
        }
    }

    fn get_type_def<O: Operand>(&self, word_id: O) -> Option<&StructDef> {
        self.structs_types
            .iter()
            .find(|def| word_id.str_key().eq(def))
    }

    fn get_type_ptr(&self, word: &LocWord) -> Option<ValueType> {
        self.structs_types
            .iter()
            .position(|def| word.eq(def))
            .map(|i| ValueType::Ptr(Value::from(i)))
    }

    fn try_get_struct_def(&self, tok: &IRToken) -> Option<&StructDef> {
        fold_bool!(tok == TokenType::Word, self.get_type_def(tok))
    }

    fn push_mem_by_context(
        &mut self, proc_index: Option<usize>, word: &StrKey, size: usize,
    ) -> ParseContext {
        let Some(proc) = proc_index.and_then(|i| self.procs.get_mut(i)) else {
            self.push_mem(word, size);
            return ParseContext::GlobalMem;
        };

        let Some(data) = proc.get_data_mut() else {
            todo!();
        };

        data.push_mem(word, size);
        ParseContext::LocalMem
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

use std::{collections::VecDeque, path::PathBuf};

use ashfire_types::{core::*, data::*, enums::*, proc::*};
use ashlib::Either;
use firelib::{lazy::LazyCtx, lexer::Loc, utils::*, ShortCircuit, TrySuccess};

use super::{types::*, utils::*};
use crate::compiler::{program::*, typechecking::expect::Compare, utils::err_loc};

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

    pub fn structs(&self) -> &[IndexWord] {
        &self.structs
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
            ParseContext::Binding => prog.get_binding(word, self),
            ParseContext::LocalMem => prog.get_local_mem(word, self),
            ParseContext::ConstStruct => prog.get_const_struct(word),
            ParseContext::LocalVar => return prog.get_local_var(word, VarWordType::None, self),
            ParseContext::GlobalVar => return prog.get_global_var(word, VarWordType::None, self),
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

                return OptionErr::new(vec![
                    (OpType::OffsetLoad, key, word.loc).into(),
                    (OpType::Unpack, word.loc).into(),
                ]);
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
            prog.get_local_var(&word, var_typ, self)
        } else {
            prog.get_global_var(&word, var_typ, self)
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
                    TokenType::Word => {
                        return OptionErr::new(vec![
                            (OpType::OffsetLoad, next_token, loc).into(),
                            (OpType::Unpack, loc).into(),
                        ])
                    }
                    _ => todo!(),
                }
            }

            KeywordType::Ref => {
                let ref_word = &self.expect_word("type after `*`", loc)?;
                let var_typ = VarWordType::Pointer;

                return match self.name_scopes.lookup(ref_word, prog) {
                    Some(ParseContext::LocalVar) => prog.get_local_var(ref_word, var_typ, self),
                    Some(ParseContext::GlobalVar) => prog.get_global_var(ref_word, var_typ, self),
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

            KeywordType::If => self.push_block((OpType::IfStart, prog.current_ip(), loc).into()),

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
            KeywordType::Import |
            KeywordType::Export |
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
        self.parse_procedure(word, prog, ModeType::Declare)
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
                self.parse_procedure(word, prog, ModeType::Declare)
            }

            (0, KeywordType::Import) => {
                self.next();
                self.parse_procedure(word, prog, ModeType::Import)
            }

            (0, KeywordType::Export) => {
                self.next();
                self.parse_procedure(word, prog, ModeType::Export)
            }

            (0, KeywordType::Inline) => {
                self.next();
                self.parse_procedure(word, prog, ModeType::Inline(prog.current_ip()))
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
        self.define_proc(word, Contract::default(), prog, ModeType::Declare)
            .into_success()?
    }

    fn parse_static_ctx(
        &mut self, ctx_size: usize, word: &LocWord, prog: &mut Program,
    ) -> OptionErr<Vec<Op>> {
        (ctx_size == 1).or_return(|| self.parse_procedure(word, prog, ModeType::Declare))?;

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
        &mut self, name: &LocWord, contract: Contract, prog: &mut Program, mode: ModeType,
    ) -> LazyResult<Op> {
        let loc = name.loc;

        if self.inside_proc() {
            let error = "Cannot define a procedure inside of another procedure";
            return Err(err_loc(error, loc));
        };

        let proc = Proc::new(name, contract, mode);

        let operand = prog.procs.len();
        self.enter_proc(operand);
        prog.procs.push(proc);

        self.name_scopes.register(name, ParseContext::ProcName);

        let prep = fold_bool!(
            matches!(mode, ModeType::Declare | ModeType::Export),
            OpType::PrepProc,
            OpType::PrepInline
        );
        Ok(self.push_block(Op::new(prep, operand as i32, loc)))
    }

    fn parse_procedure(
        &mut self, word: &LocWord, prog: &mut Program, mode: ModeType,
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
                        .define_proc(word, Contract::new(ins, outs), prog, mode)
                        .into_success()?,

                    KeywordType::Ref => {
                        let typ = self.expect_word("type after `*`", loc)?;

                        let Some(type_ptr) = prog.get_type_ptr(&typ) else {
                            return unexpected_token(typ.into(), tok_err).into();
                        };

                        type_ptr
                            .get_type()
                            .conditional_push(arrow, &mut outs, &mut ins);
                    }

                    _ => return unexpected_token(tok, tok_err).into(),
                },
                TokenType::Word => {
                    let Some(type_def) = prog.get_type_def(&tok) else {
                        return unexpected_token(tok, tok_err).into();
                    };

                    for typ in type_def.units().map(Typed::get_type) {
                        typ.conditional_push(arrow, &mut outs, &mut ins);
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

        let ctx = prog.push_mem_by_context(self, word, value.index());
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

                        members.push(StructType::unit(&member_name, *typ.value_type()));
                    } else {
                        let value = prog.get_struct_value_id(type_def).unwrap();
                        let root = StructRef::new(&member_name, ref_members.to_vec(), value);
                        members.push(StructType::Root(root));
                    }
                }
                Either::Right(type_ptr) => {
                    members.push(StructType::unit(&member_name, type_ptr));
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
        let members = stk.ordered_members(self.inside_proc());

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
            let contract: Vec<TokenType> = members
                .clone()
                .conditional_rev(self.inside_proc())
                .flat_map(StructType::units)
                .map(Typed::get_type)
                .collect();

            result.expect_exact(&contract, end_token.loc)?;

            if self.inside_proc() && assign == KeywordType::Equal {
                result.reverse();
            }

            let mut eval_items = result.into_iter();

            let def_members = members
                .provide_collect(self.inside_proc(), &mut eval_items)
                .unwrap();

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
            Err(err) => return Err(err.into()),
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
                .with_ctx(|_| "failed to get file directory path".to_owned())?
                .join(include_path);

            if prog
                .get_key(include.to_str().unwrap())
                .map(|key| prog.included_files.contains(&key)) ==
                Some(true)
            {
                continue;
            }

            info!("Including file: {:?}", include);
            self.lex_file(&include, prog)?;
        }
        Ok(self)
    }
}

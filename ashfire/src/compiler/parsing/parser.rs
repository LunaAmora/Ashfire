use std::{collections::VecDeque, path::Path};

use ashfire_types::{core::*, data::*, enums::*, proc::*};
use ashlib::Either;
use firelib::{
    lazy::LazyCtx,
    lexer::{Lexer, Loc},
    utils::*,
    TrySuccess,
};

use super::{types::*, utils::*};
use crate::compiler::{
    program::*,
    typechecking::expect::{expect_type, Compare},
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
            if let Some(mut op) = self.define_op(&token, prog).value? {
                prog.ops.append(&mut op);
            }
        }
        Ok(())
    }

    fn define_op(&mut self, tok: &IRToken, prog: &mut Program) -> OptionErr<Vec<Op>> {
        let &IRToken { token_type, operand, loc } = tok;

        if !(matches!(token_type, TokenType::Keyword | TokenType::Word) || self.inside_proc()) {
            lazybail!(
                |f| "{}Token type cannot be used outside of a procedure: `{}`",
                f.format(Fmt::Loc(loc)),
                f.format(Fmt::Typ(token_type))
            );
        };

        let op = Op::from(match token_type {
            TokenType::Keyword => return self.define_keyword_op(tok.as_keyword(), loc, prog),

            TokenType::Str => (OpType::PushStr, operand, loc),

            TokenType::Data(data) => match data {
                ValueType::Typ(val) => match val {
                    Value::Type(_) => lazybail!(
                        |f| "{}Value type not valid here: `{}`",
                        f.format(Fmt::Loc(loc)),
                        f.format(Fmt::Typ(val.get_type()))
                    ),
                    _ => (OpType::PushData(val), operand, loc),
                },
                ValueType::Ptr(ptr) => lazybail!(
                    |f| "{}Data pointer type not valid here: `{}`",
                    f.format(Fmt::Loc(loc)),
                    f.format(Fmt::Typ(ptr.get_type()))
                ),
            },

            TokenType::Word => {
                let word = &LocWord::new(operand, loc);

                choice!(
                    OptionErr,
                    prog.get_intrinsic(word),
                    self.lookup_context(word, prog),
                    self.define_context(word, prog)
                )?;

                let error =
                    format!("Word was not declared on the program: `{}`", word.as_str(prog));
                return err_loc(error, loc).into();
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
            _ => todo!(),
        };

        let word = LocWord::new(key, word.loc);

        if local {
            prog.get_local_var(&word, var_typ, self)
        } else {
            prog.get_global_var(&word, var_typ, self)
        }
    }

    fn define_keyword_op(
        &mut self, key: KeywordType, loc: Loc, prog: &Program,
    ) -> OptionErr<Vec<Op>> {
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
                op => {
                    Err(format_block("`do` can only come in a `while` or `case` block", &op, loc))?
                }
            },

            KeywordType::Let => todo!(),
            KeywordType::Case => todo!(),

            KeywordType::Colon => match self.pop_block(key, loc)? {
                Op { op_type: OpType::CaseStart, .. } => todo!(),
                op => Err(format_block(
                    "`:` can only be used on word or `case` block definition",
                    &op,
                    loc,
                ))?,
            },

            KeywordType::If => self.push_block((OpType::IfStart, prog.current_ip(), loc).into()),

            KeywordType::Else => match self.pop_block(key, loc)? {
                Op { op_type: OpType::IfStart, .. } => self.push_block((OpType::Else, loc).into()),
                Op { op_type: OpType::CaseOption, .. } => todo!(),
                op => {
                    Err(format_block("`else` can only come in a `if` or `case` block", &op, loc))?
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

                op => Err(format_block("Expected `end` to close a valid block", &op, loc))?,
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
        let mut colons: u8 = 0;
        while let Some(tok) = self.get(i) {
            match (colons, tok.token_type) {
                (1, TokenType::Data(ValueType::Typ(Value::Int))) => {
                    self.parse_memory(word, prog).try_success()?;
                }

                (1, TokenType::Word) if prog.get_type_def(tok).is_none() => {
                    return self.parse_struct(word, prog);
                }

                (_, TokenType::Word) => {}

                (0, TokenType::Keyword) => {
                    self.parse_expected_type(&mut colons, i, word, prog)?;
                }

                (1, TokenType::Keyword) => {
                    self.parse_keyword_ctx(i, word, prog)?;
                }

                _ => return Err(invalid_context(tok.clone(), word.as_str(prog))).into(),
            }
            i += 1;
        }
        OptionErr::default()
    }

    fn parse_expected_type(
        &mut self, colons: &mut u8, top_index: usize, word: &LocWord, prog: &mut Program,
    ) -> OptionErr<Vec<Op>> {
        let tok = self.get(top_index).unwrap();
        match tok.as_keyword() {
            KeywordType::Mem => {
                self.next();
                self.parse_memory(word, prog).try_success()?;
            }

            KeywordType::Struct => {
                self.next();
                self.parse_struct(word, prog)
            }

            KeywordType::Proc => {
                self.next();
                self.parse_procedure(word, prog, ModeType::Declare)
            }

            KeywordType::Import => {
                self.next();
                self.parse_procedure(word, prog, ModeType::Import)
            }

            KeywordType::Export => {
                self.next();
                self.parse_procedure(word, prog, ModeType::Export)
            }

            KeywordType::Inline => {
                self.next();
                self.parse_procedure(word, prog, ModeType::Inline(prog.current_ip()))
            }

            KeywordType::Colon => {
                *colons += 1;
                OptionErr::default()
            }

            KeywordType::End => {
                let error = format!(
                    "Missing body or contract necessary to infer the type of the word: `{}`",
                    word.as_str(prog)
                );
                err_loc(error, word.loc).into()
            }

            _ => Err(invalid_context(tok.clone(), word.as_str(prog))).into(),
        }
    }

    fn parse_keyword_ctx(
        &mut self, top_index: usize, word: &LocWord, prog: &mut Program,
    ) -> OptionErr<Vec<Op>> {
        let tok = self.get(top_index).unwrap();
        match tok.as_keyword() {
            KeywordType::Equal => {
                self.skip(1);
                self.parse_var(word, prog, true).try_success()?
            }

            KeywordType::Colon if top_index == 1 => self.parse_end_ctx(word, prog),

            KeywordType::Colon | KeywordType::Arrow => {
                self.parse_procedure(word, prog, ModeType::Declare)
            }

            KeywordType::End => {
                if top_index == 0 {
                    todo!("Unitialized, unknown type");
                } else {
                    self.skip(1);
                    self.parse_var(word, prog, false).try_success()?
                }
            }

            KeywordType::Ref => OptionErr::default(),

            _ => Err(invalid_context(tok.clone(), word.as_str(prog))).into(),
        }
    }

    fn parse_end_ctx(&mut self, word: &LocWord, prog: &mut Program) -> OptionErr<Vec<Op>> {
        self.skip(2);
        self.parse_static_ctx(word, prog)?;
        self.define_proc(word, Contract::default(), prog, ModeType::Declare)
            .into_success()?
    }

    fn parse_static_ctx(&mut self, word: &LocWord, prog: &mut Program) -> OptionErr<Vec<Op>> {
        if let Ok((eval, skip)) = self.compile_eval(prog, word.loc).value {
            if &eval != Value::Any {
                self.skip(skip);
                let struct_type = ValueUnit::new(word, &eval).into();
                prog.register_const(struct_type);

                self.name_scopes.register(word, ParseContext::ConstStruct);
                success!();
            }
        }
        OptionErr::default()
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
        Ok(self.push_block(Op::new(prep, operand, loc)))
    }

    fn parse_procedure(
        &mut self, word: &LocWord, prog: &mut Program, mode: ModeType,
    ) -> OptionErr<Vec<Op>> {
        let mut ins = Vec::new();
        let mut outs = Vec::new();
        let mut arrow = false;
        let loc = word.loc;

        self.expect_keyword(KeywordType::Colon, "`:` after procedure declaration", loc)?;
        while let Some(tok) = self.next() {
            match tok.get_keyword() {
                Some(KeywordType::Arrow) => {
                    if arrow {
                        let error = "Duplicated `->` found on procedure definition";
                        return err_loc(error, loc).into();
                    }
                    arrow = true;
                }

                Some(KeywordType::Colon) => self
                    .define_proc(word, Contract::new(ins, outs), prog, mode)
                    .into_success()?,

                _ => {
                    let type_kind = self.check_type_kind(tok, "variable type", prog)?;

                    match type_kind {
                        Either::Left(type_def) => {
                            for typ in type_def.units().map(Typed::get_type) {
                                typ.conditional_push(arrow, &mut outs, &mut ins);
                            }
                        }
                        Either::Right(type_ptr) => {
                            type_ptr
                                .get_type()
                                .conditional_push(arrow, &mut outs, &mut ins);
                        }
                    }
                }
            }
        }

        unexpected_end("proc contract or `:` after procedure definition", loc).into()
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
            let type_kind = self.expect_type_kind("struct member type", prog, loc)?;

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
                        members.push(StructType::root(&member_name, ref_members.to_vec(), value));
                    }
                }
                Either::Right(type_ptr) => {
                    members.push(StructType::unit(&member_name, type_ptr));
                }
            }
        }

        err_loc("Expected struct members or `end` after struct declaration", loc).into()
    }

    fn parse_var(
        &mut self, word: &LocWord, prog: &mut Program, initialize: bool,
    ) -> LazyResult<()> {
        let loc = word.loc;
        let type_kind = self.expect_type_kind("variable type", prog, loc)?;

        if initialize {
            self.expect_keyword(KeywordType::Equal, "`=` after variable type", loc)?;

            match type_kind {
                Either::Left(type_def) => {
                    self.eval_const_or_var(false, word, type_def.clone(), prog)
                }
                Either::Right(_) => {
                    todo!()
                    // let name = format!("*{}", ref_word.as_str(prog));
                    // let word_id = prog.get_or_intern(&name);

                    // let value = ValueUnit::new(&StrKey::default(), type_ptr);
                    // let stk = StructDef::new(&word_id, vec![StructType::Unit(value)]);

                    // prog.structs_types.push(stk.clone()); //Todo: Check if the `*` type is already registered

                    // self.parse_const_or_var(word, word_id, stk, prog)
                    //     .try_success()?;
                }
            }
        } else {
            self.expect_keyword(KeywordType::End, "`end` after variable type", loc)?;

            let Either::Left(type_def) = type_kind else {
                return Err(err_loc("error", loc))
            };

            let ref_members: Vec<_> = type_def
                .clone()
                .ordered_members(self.inside_proc())
                .collect();

            if ref_members.len() == 1 {
                todo!()
            } else {
                let type_name = &type_def.str_key();

                let reftype = prog.get_struct_value_id(type_name).unwrap();
                let struct_type = StructType::root(word, ref_members, reftype);
                self.register_const_or_var(false, word, type_name, struct_type, prog);
            }
            Ok(())
        }
    }

    fn eval_const_or_var(
        &mut self, is_constant: bool, word: &LocWord, stk: StructDef, prog: &mut Program,
    ) -> LazyResult<()> {
        let (mut result, eval) = match self.compile_eval_n(stk.count(), prog, word.loc).value {
            Ok(value) => value,
            Err(either) => {
                return Err(either.either(
                    |tok| err_loc("Failed to parse an valid struct value at compile-time", tok.loc),
                    |err| err,
                ))
            }
        };

        let end_token = self.skip(eval).unwrap();
        let stk_name = stk.str_key();

        let struct_type = if result.len() == 1 {
            let eval = result.pop().unwrap();
            let members = stk.ordered_members(self.inside_proc());

            match members.last().unwrap() {
                StructType::Root(_) => todo!(),
                StructType::Unit(typ) => expect_type(&eval, typ.get_type(), end_token.loc)?,
            };

            StructType::Unit(ValueUnit::new(word, &eval))
        } else {
            let contract: Vec<_> = stk.units().map(Typed::get_type).collect();
            result.expect_exact(&contract, end_token.loc)?;

            let mut eval_items = result
                .into_iter()
                .conditional_rev(self.inside_proc() && !is_constant);

            let def_members = stk.transpose(self.inside_proc(), &mut eval_items).unwrap();
            let value = prog.get_struct_value_id(&stk_name).unwrap();

            StructType::root(word, def_members, value)
        };

        self.register_const_or_var(is_constant, word, &stk_name, struct_type, prog);
        Ok(())
    }

    fn register_const_or_var(
        &mut self, is_constant: bool, word: &LocWord, ref_name: &StrKey, struct_type: StructType,
        prog: &mut Program,
    ) {
        let ctx = if is_constant {
            prog.register_const(struct_type);
            ParseContext::ConstStruct
        } else {
            self.register_var(struct_type, prog)
        };

        self.name_scopes.register(word, ctx);
        self.structs.push(IndexWord::new(word, ref_name));
    }

    pub fn register_var(&mut self, struct_word: StructType, prog: &mut Program) -> ParseContext {
        if let Some(proc) = self.current_proc_mut(prog) {
            let data = proc.get_data_mut().unwrap();
            data.local_vars.push(struct_word);
            ParseContext::LocalVar
        } else {
            prog.global_vars.push(struct_word);
            ParseContext::GlobalVar
        }
    }

    pub fn lex_file(&mut self, path: &Path, prog: &mut Program) -> LazyResult<&mut Self> {
        let lex = match prog.new_file_lexer(path) {
            Ok(ok) => ok,
            Err(err) => return Err(err.into()),
        };

        self.read_lexer(prog, lex, path)
    }

    pub fn read_lexer(
        &mut self, prog: &mut Program, mut lex: Lexer, path: &Path,
    ) -> LazyResult<&mut Self> {
        while let Some(token) = prog.lex_next_token(&mut lex).value? {
            if &token != KeywordType::Include {
                self.ir_tokens.push_back(token);
                continue;
            }

            let tok = expect_token_by(
                prog.lex_next_token(&mut lex).value?,
                |tok| tok == TokenType::Word,
                "include file name",
                token.loc,
            )?;

            let include_path = prog.get_word(tok);

            let include = get_dir(path)
                .with_ctx(|_| "failed to get file directory path".to_owned())?
                .join(include_path)
                .with_extension("fire");

            if prog.contains_source(include.with_extension("").to_str().unwrap()) ||
                prog.contains_source(include.to_str().unwrap())
            {
                continue;
            }

            info!("Including file: {:?}", include);
            self.lex_file(&include, prog)?;
        }
        Ok(self)
    }
}

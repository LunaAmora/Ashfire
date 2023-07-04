use std::{collections::VecDeque, fs::File, ops::Index, path::Path};

use ashfire_types::{core::*, data::*, enums::*, proc::*};
use ashlib::Either;
use firelib::{
    lazy::LazyCtx,
    lexer::{Lexer, Loc},
    span::Span,
    utils::*,
    Context, TrySuccess,
};
use ControlOp::*;

use super::{types::*, utils::*};
use crate::compiler::{
    ctx::*,
    typechecking::expect::{expect_type, Compare},
    utils::err_loc,
};

#[derive(Default)]
pub struct Parser {
    ir_tokens: VecDeque<IRToken>,
    name_scopes: NameScopes,
    structs: Vec<TypedWord>,
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

impl Iterator for Parser {
    type Item = IRToken;

    /// Pops and returns the next [`IRToken`] of this [`Parser`].
    fn next(&mut self) -> Option<Self::Item> {
        self.ir_tokens.pop_front()
    }
}

impl Index<usize> for Parser {
    type Output = IRToken;

    fn index(&self, index: usize) -> &Self::Output {
        self.ir_tokens.get(index).expect("Index out of range")
    }
}

impl Parser {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn peek(&self) -> Option<&IRToken> {
        self.ir_tokens.get(0)
    }

    pub fn get(&self, index: usize) -> Option<&IRToken> {
        self.ir_tokens.get(index)
    }

    /// Pops `n` elements and returns the last [`IRToken`] of this [`Parser`].
    pub fn skip(&mut self, n: usize) -> Option<IRToken> {
        let mut result = None;
        for _ in 0..n {
            result = self.next();
        }
        result
    }

    /// Searches for a `struct` that matches the given [`Name`],
    /// returning its [`DataType`].
    pub fn find_data_type(&self, word: Name) -> Option<DataType> {
        self.structs
            .iter()
            .find(|stk| word.eq(stk))
            .map(Wrapper::value)
    }

    /// Parse each [`IRToken`] from this [`Parser`] to an [`Op`],
    /// appending to the given [`Ctx`].
    ///
    /// # Errors
    ///
    /// This function will return an error if any problem is encountered during parsing.
    pub fn parse_tokens(&mut self, ctx: &mut Ctx) -> LazyResult<()> {
        while let Some(token) = self.next() {
            if let Some(mut op) = self.define_op(&token, ctx).value? {
                ctx.ops_mut().append(&mut op);
            }
        }
        Ok(())
    }

    fn define_op(&mut self, &(tok_typ, loc): &IRToken, ctx: &mut Ctx) -> OptionErr<Vec<Op>> {
        if !(matches!(tok_typ, TokenType::Keyword(_) | TokenType::Word(_)) || self.inside_proc()) {
            lazybail!(
                |f| "{}Token type cannot be used outside of a procedure: `{}`",
                f.format(Fmt::Loc(loc)),
                f.format(Fmt::Typ(tok_typ))
            );
        };

        let op = match tok_typ {
            TokenType::Keyword(key) => return self.define_keyword_op(key, loc, ctx),

            TokenType::Str(data_key) => (OpType::DataOp(DataOp::PushStr, data_key), loc),

            TokenType::Data(Value(data, value)) => match data {
                DataType(id) => match id {
                    TypeId::INT | TypeId::BOOL | TypeId::PTR => {
                        (OpType::PushData(data, value), loc)
                    }

                    _ => lazybail!(
                        |f| "{}Value type not valid here: `{}`",
                        f.format(Fmt::Loc(loc)),
                        f.format(Fmt::Dat(data))
                    ),
                },
            },

            TokenType::Word(name) => {
                let word = &(name, loc);

                choice!(
                    OptionErr,
                    ctx.get_intrinsic(word),
                    self.lookup_context(word, ctx),
                    self.define_context(word, ctx)
                )?;

                lazybail!(
                    |f| "{}Word was not declared on the ctx: `{}`",
                    f.format(Fmt::Loc(loc)),
                    f.format(Fmt::Key(name))
                )
            }

            TokenType::Type(_) => todo!(),
        };

        OptionErr::new(vec![op])
    }

    fn lookup_context(&self, word: &LocWord, ctx: &mut Ctx) -> OptionErr<Vec<Op>> {
        let Some(parse_ctx) = self.name_scopes.lookup(word.name(), ctx) else {
            return self.lookup_modified_var(word, ctx);
        };

        match parse_ctx {
            ParseContext::ProcName => ctx.get_proc_name(word),
            ParseContext::GlobalMem => ctx.get_global_mem(word),
            ParseContext::Binding => ctx.get_binding(word, self),
            ParseContext::LocalMem => ctx.get_local_mem(word, self),
            ParseContext::ConstStruct => ctx.get_const_struct(word),
            ParseContext::LocalVar => return ctx.get_local_var(word, VarWordType::None, self),
            ParseContext::GlobalVar => return ctx.get_global_var(word, VarWordType::None, self),
        }
        .into()
    }

    fn lookup_modified_var(&self, &(name, loc): &LocWord, ctx: &mut Ctx) -> OptionErr<Vec<Op>> {
        let name_str = name.as_str(ctx);
        let (rest, var_typ) = match name_str.split_at(1) {
            ("!", rest) => (rest, VarWordType::Store),
            (".", rest) => {
                let Some(key) = ctx.get_key(rest) else {
                    todo!()
                };

                return OptionErr::new(vec![
                    (OpType::Offset(key), loc),
                    (OpType::UnpackType(None), loc),
                ]);
            }
            _ => return OptionErr::default(),
        };

        let Some(key) = ctx.get_key(rest) else {
            todo!()
        };

        let local = match self.name_scopes.lookup(key, ctx) {
            Some(ParseContext::LocalVar) => true,
            Some(ParseContext::GlobalVar) => false,
            _ => todo!(),
        };

        if local {
            ctx.get_local_var(&(key, loc), var_typ, self)
        } else {
            ctx.get_global_var(&(key, loc), var_typ, self)
        }
    }

    fn define_keyword_op(
        &mut self, key: KeywordType, loc: Loc, ctx: &mut Ctx,
    ) -> OptionErr<Vec<Op>> {
        let op = match key {
            KeywordType::Drop => Op::new((StackOp::Drop, loc)),
            KeywordType::Dup => Op::new((StackOp::Dup, loc)),
            KeywordType::Swap => Op::new((StackOp::Swap, loc)),
            KeywordType::Over => Op::new((StackOp::Over, loc)),
            KeywordType::Rot => Op::new((StackOp::Rot, loc)),
            KeywordType::Equal => Op::new((StackOp::Equal, loc)),
            KeywordType::At => (OpType::UnpackType(None), loc),

            KeywordType::Dot => {
                let Some(next_token) = self.next() else {
                    todo!()
                };

                match next_token.token_type() {
                    TokenType::Keyword(_) => {
                        expect_token_by(
                            Some(next_token),
                            |t| t == KeywordType::Ref,
                            "word or `*` after `.`",
                            loc,
                        )?;
                        let (word, _) = self.expect_word("word after `.*`", loc)?;
                        (OpType::Offset(word), loc)
                    }

                    TokenType::Word(name) => {
                        return OptionErr::new(vec![
                            (OpType::Offset(name), loc),
                            (OpType::UnpackType(None), loc),
                        ])
                    }
                    _ => todo!(),
                }
            }

            KeywordType::Ref => {
                let ref_word = &self.expect_word("type after `*`", loc)?;
                let var_typ = VarWordType::Pointer;

                return match self.name_scopes.lookup(ref_word.name(), ctx) {
                    Some(ParseContext::LocalVar) => ctx.get_local_var(ref_word, var_typ, self),
                    Some(ParseContext::GlobalVar) => ctx.get_global_var(ref_word, var_typ, self),
                    Some(ParseContext::Binding) => ctx.get_binding_ref(ref_word, self).into(),
                    _ => todo!(),
                };
            }

            KeywordType::While => self.push_control_block((While, 0, loc)),

            KeywordType::Do => match self.pop_control_block(key, loc)? {
                (While, ..) => self.push_control_block((Do, 0, loc)),
                (CaseMatch, operand, _) => self.push_control_block((CaseOption, operand, loc)),
                op => {
                    Err(format_block("`do` can only come in a `while` or `case` block", op, loc))?
                }
            },

            KeywordType::Let => return self.parse_bindings(loc, ctx),

            KeywordType::Case => todo!(),

            KeywordType::Colon => match self.pop_control_block(key, loc)? {
                (CaseStart, ..) => todo!(),
                op => Err(format_block(
                    "`:` can only be used on word or `case` block definition",
                    op,
                    loc,
                ))?,
            },

            KeywordType::If => self.push_control_block((IfStart, ctx.current_ip(), loc)),

            KeywordType::Else => match self.pop_control_block(key, loc)? {
                (IfStart, ..) => self.push_control_block((Else, 0, loc)),
                (CaseOption, ..) => todo!(),
                op => Err(format_block("`else` can only come in a `if` or `case` block", op, loc))?,
            },

            KeywordType::End => match self.pop_control_block(key, loc)? {
                (IfStart, ..) => Op::new((EndIf, loc)),
                (Else, ..) => Op::new((EndElse, loc)),
                (Do, ..) => Op::new((EndWhile, loc)),

                (BindStack, operand, _) => (OpType::ControlOp(PopBind, operand), loc),

                (CaseOption, ..) => todo!(),

                (PrepProc, operand, _) => {
                    self.exit_proc();
                    (OpType::ControlOp(EndProc, operand), loc)
                }

                (PrepInline, operand, _) => {
                    self.exit_proc();
                    (OpType::ControlOp(EndInline, operand), loc)
                }

                op => Err(format_block("Expected `end` to close a valid block", op, loc))?,
            },

            KeywordType::Include |
            KeywordType::Inline |
            KeywordType::Import |
            KeywordType::Export |
            KeywordType::Arrow |
            KeywordType::Proc |
            KeywordType::Mem |
            KeywordType::In |
            KeywordType::Struct => {
                let error = format!("Keyword type is not valid here: `{key:?}`");
                return err_loc(error, loc).into();
            }
        };

        OptionErr::new(vec![op])
    }

    /// Creates a logic block starting from the given [`OpType::ControlOp`].
    fn push_control_block(&mut self, block @ (op, value, loc): Block) -> Op {
        self.name_scopes.push(block);
        (OpType::ControlOp(op, value), loc)
    }

    /// Pops the last opened logic block, returning a the [`OpType::ControlOp`]
    /// that started it.
    ///
    /// # Errors
    ///
    /// This function will return an error if no block is open.
    fn pop_control_block(&mut self, closing_type: KeywordType, loc: Loc) -> LazyResult<Block> {
        self.name_scopes.pop().with_err_ctx(move || {
            err_loc(format!("There are no open blocks to close with `{closing_type:?}`"), loc)
        })
    }

    /// Tries to define the parsing context to use
    /// based on the preceding [`IRTokens`][IRToken].
    fn define_context(&mut self, word: &LocWord, ctx: &mut Ctx) -> OptionErr<Vec<Op>> {
        let mut i = 0;
        let mut colons: u8 = 0;
        while let Some(tok) = self.get(i) {
            match (colons, tok.token_type()) {
                (1, TokenType::Data(Value(DataType(TypeId::INT), _))) => {
                    self.parse_memory(word, ctx).try_success()?;
                }

                (1, TokenType::Word(name)) if ctx.get_data_type(name).is_none() => {
                    return self.parse_struct(word, ctx);
                }

                (_, TokenType::Word(_)) => {}

                (0, TokenType::Keyword(_)) => {
                    self.parse_expected_type(&mut colons, i, word, ctx)?;
                }

                (1, TokenType::Keyword(_)) => {
                    self.parse_keyword_ctx(i, word, ctx)?;
                }

                _ => return invalid_context(*tok, word.as_str(ctx)).into(),
            }
            i += 1;
        }
        OptionErr::default()
    }

    fn parse_expected_type(
        &mut self, colons: &mut u8, top_index: usize, word: &LocWord, ctx: &mut Ctx,
    ) -> OptionErr<Vec<Op>> {
        let tok = &self[top_index];
        match tok.as_keyword() {
            KeywordType::Mem => {
                self.next();
                self.parse_memory(word, ctx).try_success()?;
            }

            KeywordType::Struct => {
                self.next();
                self.parse_struct(word, ctx)
            }

            KeywordType::Proc => {
                self.next();
                self.parse_procedure(word, ctx, ModeType::Declare)
            }

            KeywordType::Import => {
                self.next();
                self.parse_procedure(word, ctx, ModeType::Import)
            }

            KeywordType::Export => {
                self.next();
                self.parse_procedure(word, ctx, ModeType::Export)
            }

            KeywordType::Inline => {
                self.next();
                self.parse_procedure(word, ctx, ModeType::Inline(ctx.current_ip()))
            }

            KeywordType::Colon => {
                *colons += 1;
                OptionErr::default()
            }

            KeywordType::End => {
                let error = format!(
                    "Missing body or contract necessary to infer the type of the word: `{}`",
                    word.as_str(ctx)
                );
                err_loc(error, word.loc()).into()
            }

            _ => invalid_context(*tok, word.as_str(ctx)).into(),
        }
    }

    fn parse_keyword_ctx(
        &mut self, top_index: usize, word: &LocWord, ctx: &mut Ctx,
    ) -> OptionErr<Vec<Op>> {
        let tok = &self[top_index];
        match tok.as_keyword() {
            KeywordType::Equal => {
                self.skip(1);
                self.parse_var(word, ctx, true).try_success()?
            }

            KeywordType::Colon if top_index == 1 => self.parse_end_ctx(word, ctx),

            KeywordType::Colon | KeywordType::Arrow => {
                self.parse_procedure(word, ctx, ModeType::Declare)
            }

            KeywordType::End => {
                if top_index == 0 {
                    todo!("Uninitialized, unknown type");
                } else {
                    self.skip(1);
                    self.parse_var(word, ctx, false).try_success()?
                }
            }

            KeywordType::Ref => OptionErr::default(),

            _ => invalid_context(*tok, word.as_str(ctx)).into(),
        }
    }

    fn parse_end_ctx(&mut self, word: &LocWord, ctx: &mut Ctx) -> OptionErr<Vec<Op>> {
        self.skip(2);
        self.parse_static_ctx(word, ctx)?;
        self.define_proc(word, Contract::default(), ctx, ModeType::Declare)
            .into_success()?
    }

    fn parse_static_ctx(&mut self, word: &LocWord, ctx: &mut Ctx) -> OptionErr<Vec<Op>> {
        if let Ok((eval, skip)) = self.compile_eval(ctx, word.loc()).value {
            if eval != ANY {
                self.skip(skip);
                let primitive = Primitive::new(word.name(), eval);
                ctx.register_const(TypeDescr::Primitive(primitive));

                self.name_scopes
                    .register(word.name(), ParseContext::ConstStruct);
                success!();
            }
        }
        OptionErr::default()
    }

    fn define_proc(
        &mut self, &(name, loc): &LocWord, contract: Contract, ctx: &mut Ctx, mode: ModeType,
    ) -> LazyResult<Op> {
        if self.inside_proc() {
            let error = "Cannot define a procedure inside of another procedure";
            return Err(err_loc(error, loc));
        };

        let proc = Proc::new(name, contract, mode);

        let operand = ctx.push_proc(proc);
        self.enter_proc(operand);

        self.name_scopes.register(name, ParseContext::ProcName);

        let prep = if matches!(mode, ModeType::Declare | ModeType::Export) {
            PrepProc
        } else {
            PrepInline
        };

        Ok(self.push_control_block((prep, operand, loc)))
    }

    fn parse_procedure(
        &mut self, word: &LocWord, ctx: &mut Ctx, mode: ModeType,
    ) -> OptionErr<Vec<Op>> {
        let mut ins = Vec::new();
        let mut outs = Vec::new();
        let mut arrow = false;
        let loc = word.loc();

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
                    .define_proc(word, Contract::new(ins, outs), ctx, mode)
                    .into_success()?,

                _ => {
                    let kind = self.check_type_kind(tok, "variable type", ctx)?;

                    for typ in ctx.get_type_descr(kind).units().map(|prim| prim.get_type()) {
                        typ.conditional_push(arrow, &mut outs, &mut ins);
                    }
                }
            }
        }

        unexpected_end("proc contract or `:` after procedure definition", loc).into()
    }

    fn parse_memory(&mut self, word: &LocWord, ctx: &mut Ctx) -> LazyResult<()> {
        let loc = word.loc();

        self.expect_keyword(KeywordType::Colon, "`:` after `mem`", loc)?;
        let (mem_size, _) =
            self.expect_by_option(|tok| tok.get_data(INT), "memory size after `:`", loc)?;
        self.expect_keyword(KeywordType::End, "`end` after memory size", loc)?;

        let size = mem_size.try_into().expect("ICE"); //Todo: explain this error
        let ctx = ctx.push_mem_by_context(self, word.name(), size);
        self.name_scopes.register(word.name(), ctx);

        Ok(())
    }

    fn parse_struct(&mut self, word: &LocWord, ctx: &mut Ctx) -> OptionErr<Vec<Op>> {
        let loc = word.loc();

        self.expect_keyword(KeywordType::Colon, "`:` after keyword `struct`", loc)?;
        let mut members = Vec::new();

        while let Some(tok) = self.peek() {
            if tok.is_keyword() {
                self.expect_keyword(KeywordType::End, "`end` after struct declaration", loc)?;
                ctx.register_type(StructFields(word.name(), members));
                success!();
            }

            let (member_name, _) = self.expect_word("struct member name", loc)?;
            let kind = self.expect_type_kind("struct member type", ctx, loc)?;

            match &*ctx.get_type_descr(kind) {
                TypeDescr::Structure(StructType(fields, _)) => {
                    let value = ctx.get_fields_data_type(fields);
                    members.push(TypeDescr::structure(member_name, fields.to_vec(), value));
                }
                TypeDescr::Primitive(prim) => {
                    members.push(TypeDescr::primitive(member_name, prim.get_type()));
                }
                TypeDescr::Reference(_) => todo!(),
            }
        }

        unexpected_end("struct members or `end` after struct declaration", loc).into()
    }

    fn parse_var(&mut self, word: &LocWord, ctx: &mut Ctx, initialize: bool) -> LazyResult<()> {
        let loc = word.loc();
        let kind = self.expect_type_kind("variable type", ctx, loc)?;

        if initialize {
            self.expect_keyword(KeywordType::Equal, "`=` after variable type", loc)?;
            self.eval_const_or_var(false, word, kind, ctx)
        } else {
            self.expect_keyword(KeywordType::End, "`end` after variable type", loc)?;

            let type_def = ctx.get_type_descr(kind).clone();

            let ref_members: Vec<_> = match type_def.clone() {
                TypeDescr::Structure(StructType(fields, _)) => {
                    fields.ordered_members(self.inside_proc()).collect()
                }
                TypeDescr::Primitive(_) => todo!(),
                TypeDescr::Reference(ptr) => {
                    let struct_type =
                        TypeDescr::reference(word.name(), ptr.get_type(), ptr.ptr_type());
                    self.register_const_or_var(false, word, ptr.ptr_type(), struct_type, ctx);
                    return Ok(());
                }
            };

            if ref_members.len() == 1 {
                todo!()
            } else {
                let struct_type =
                    TypeDescr::structure(word.name(), ref_members, type_def.get_type());
                self.register_const_or_var(false, word, type_def.get_type(), struct_type, ctx);
            }
            Ok(())
        }
    }

    fn parse_bindings(&mut self, loc: Loc, ctx: &mut Ctx) -> OptionErr<Vec<Op>> {
        let mut bindings = vec![];

        while let Some(tok) = self.peek() {
            if tok.is_keyword() {
                self.expect_keyword(KeywordType::In, "`in` after let bind declaration", loc)?;

                bindings.reverse();
                let current_proc_mut = self
                    .current_proc_mut(ctx)
                    .expect("Expected to be used inside a procedure");

                let proc_bindings = &mut current_proc_mut.binds;
                proc_bindings.push(Binds(bindings));

                let bind_block = (BindStack, proc_bindings.len() - 1, loc);
                let result = self.push_control_block(bind_block);

                return OptionErr::new(vec![result]);
            }

            let LabelKind((word, _), typ) = self.expect_label_kind("bind", loc, ctx)?;
            self.name_scopes.register(word, ParseContext::Binding);

            bindings.push((word, typ));
        }

        unexpected_end("bind labels after let bind declaration", loc).into()
    }

    fn eval_const_or_var(
        &mut self, is_constant: bool, word: &LocWord, reftype: DataType, ctx: &mut Ctx,
    ) -> LazyResult<()> {
        let stk = ctx.get_type_descr(reftype).clone();
        let &(name, loc) = word;

        let (result, skip) = match self.compile_eval_n(stk.count(), ctx, loc).value {
            Ok(value) => value,
            Err(either) => {
                return Err(either.either(
                    |(_, loc)| {
                        err_loc("Failed to parse an valid struct value at compile-time", loc)
                    },
                    |err| err,
                ))
            }
        };

        let (_, end_loc) = self
            .skip(skip)
            .expect("Should not fail if `compile_eval_n` was successful");

        let struct_type = match result {
            Either::Left(tok) => {
                match &stk {
                    TypeDescr::Primitive(_) => todo!(),

                    TypeDescr::Structure(StructType(fields, _)) => {
                        expect_type(tok, &fields[0], end_loc)?;
                    }

                    TypeDescr::Reference(ptr) => {
                        expect_type(tok, ptr, end_loc)?;
                    }
                };

                TypeDescr::Primitive(Primitive::new(name, tok))
            }

            Either::Right(tokens) => {
                let contract: Vec<_> = stk.units().map(|p| p.get_type()).collect();
                tokens.expect_exact(&contract, end_loc)?;

                let mut eval_items = tokens
                    .into_iter()
                    .conditional_rev(self.inside_proc() && !is_constant);

                let def_members: Vec<_> = match stk {
                    TypeDescr::Structure(StructType(fields, _)) => fields
                        .transpose(self.inside_proc(), &mut eval_items)
                        .expect(
                            "Should not fail if `fields` and `eval_items` have the same length",
                        ),
                    TypeDescr::Primitive(_) => todo!(),
                    TypeDescr::Reference(_) => todo!(),
                };

                TypeDescr::structure(name, def_members, reftype)
            }
        };

        self.register_const_or_var(is_constant, word, reftype, struct_type, ctx);
        Ok(())
    }

    fn register_const_or_var(
        &mut self, is_constant: bool, word: &LocWord, reftype: DataType, struct_type: TypeDescr,
        ctx: &mut Ctx,
    ) {
        let ctx = if is_constant {
            ctx.register_const(struct_type);
            ParseContext::ConstStruct
        } else {
            self.register_var(struct_type, ctx)
        };

        self.name_scopes.register(word.name(), ctx);
        self.structs.push(TypedWord::new(word.name(), reftype));
    }

    pub fn register_var(&mut self, struct_word: TypeDescr, ctx: &mut Ctx) -> ParseContext {
        if let Some(proc) = self.current_proc_mut(ctx) {
            let Some(data) = proc.get_data_mut() else {
                todo!("Cannot use variables inside inlined procedures")
            };

            data.local_vars.push(struct_word);
            ParseContext::LocalVar
        } else {
            ctx.push_global_var(struct_word);
            ParseContext::GlobalVar
        }
    }

    pub fn lex_path(&mut self, path: &Path, ctx: &mut Ctx) -> LazyResult<&mut Self> {
        let module_name = path.get_dir()?;
        let source_name = path.get_file()?;

        self.lex_include(path, module_name, source_name, ctx)
    }

    pub fn lex_source(
        &mut self, source: &str, module: &str, ctx: &mut Ctx,
    ) -> LazyResult<&mut Self> {
        let path = Path::new(module).join(source).with_extension("fire");

        let module_name = path.get_dir()?;
        let source_name = path.get_file()?;

        if ctx.has_source(source_name, module_name) {
            return Ok(self);
        }

        self.lex_include(&path, module_name, source_name, ctx)
    }

    pub fn lex_include(
        &mut self, path: &Path, module: &str, source: &str, ctx: &mut Ctx,
    ) -> LazyResult<&mut Self> {
        debug!("Including: {:?}", path);
        let file = File::open(path).with_context(|| format!("Could not read file `{path:?}`"))?;

        let lex = ctx.new_lexer(file, source, module);
        self.read_lexer(ctx, lex, module)
    }

    pub fn read_lexer(
        &mut self, ctx: &mut Ctx, mut lex: Lexer, module: &str,
    ) -> LazyResult<&mut Self> {
        while let Some(token) = ctx.lex_next_token(&mut lex).value? {
            if &token != KeywordType::Include {
                self.ir_tokens.push_back(token);
                continue;
            }

            let (tok, _) = expect_token_by_option(
                ctx.lex_next_token(&mut lex).value?,
                IRTokenExt::get_word,
                "include source name",
                token.loc(),
            )?;

            let include_path = ctx.get_word(tok);
            self.lex_source(&include_path, module, ctx)?;
        }
        Ok(self)
    }
}

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
    program::*,
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
        let &(token_type, loc) = tok;

        if !(matches!(token_type, TokenType::Keyword(_) | TokenType::Word(_)) || self.inside_proc())
        {
            lazybail!(
                |f| "{}Token type cannot be used outside of a procedure: `{}`",
                f.format(Fmt::Loc(loc)),
                f.format(Fmt::TTyp(token_type))
            );
        };

        let op = match token_type {
            TokenType::Keyword(key) => return self.define_keyword_op(key, loc, prog),

            TokenType::Str(data_key) => (OpType::DataOp(DataOp::PushStr, data_key), loc),

            TokenType::Data(Value(data, value)) => match data {
                DataType(id) => match id {
                    TypeId::INT | TypeId::BOOL | TypeId::PTR => {
                        (OpType::PushData(data, value), loc)
                    }

                    _ => lazybail!(
                        |f| "{}Value type not valid here: `{}`",
                        f.format(Fmt::Loc(loc)),
                        f.format(Fmt::DTyp(data))
                    ),
                },
            },

            TokenType::Word(name) => {
                let word = &(name, loc);

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

            TokenType::Type(_) => todo!(),
        };

        OptionErr::new(vec![op])
    }

    fn lookup_context(&self, word: &LocWord, prog: &mut Program) -> OptionErr<Vec<Op>> {
        let Some(ctx) = self.name_scopes.lookup(word.name(), prog) else {
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
                    (OpType::Offset(key), word.loc()),
                    (OpType::UnpackType(None), word.loc()),
                ]);
            }
            _ => return OptionErr::default(),
        };

        let Some(key) = prog.get_key(rest) else {
            todo!()
        };

        let local = match self.name_scopes.lookup(key, prog) {
            Some(ParseContext::LocalVar) => true,
            Some(ParseContext::GlobalVar) => false,
            _ => todo!(),
        };

        let field_word = (key, word.loc());

        if local {
            prog.get_local_var(&field_word, var_typ, self)
        } else {
            prog.get_global_var(&field_word, var_typ, self)
        }
    }

    fn define_keyword_op(
        &mut self, key: KeywordType, loc: Loc, prog: &mut Program,
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

                return match self.name_scopes.lookup(ref_word.name(), prog) {
                    Some(ParseContext::LocalVar) => prog.get_local_var(ref_word, var_typ, self),
                    Some(ParseContext::GlobalVar) => prog.get_global_var(ref_word, var_typ, self),
                    Some(ParseContext::Binding) => prog.get_binding_ref(ref_word, self).into(),
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

            KeywordType::Let => return self.parse_bindings(loc, prog),

            KeywordType::Case => todo!(),

            KeywordType::Colon => match self.pop_control_block(key, loc)? {
                (CaseStart, ..) => todo!(),
                op => Err(format_block(
                    "`:` can only be used on word or `case` block definition",
                    op,
                    loc,
                ))?,
            },

            KeywordType::If => self.push_control_block((IfStart, prog.current_ip(), loc)),

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
    fn define_context(&mut self, word: &LocWord, prog: &mut Program) -> OptionErr<Vec<Op>> {
        let mut i = 0;
        let mut colons: u8 = 0;
        while let Some(tok) = self.get(i) {
            match (colons, tok.token_type()) {
                (1, TokenType::Data(Value(DataType(TypeId::INT), _))) => {
                    self.parse_memory(word, prog).try_success()?;
                }

                (1, TokenType::Word(name)) if prog.get_data_type(name).is_none() => {
                    return self.parse_struct(word, prog);
                }

                (_, TokenType::Word(_)) => {}

                (0, TokenType::Keyword(_)) => {
                    self.parse_expected_type(&mut colons, i, word, prog)?;
                }

                (1, TokenType::Keyword(_)) => {
                    self.parse_keyword_ctx(i, word, prog)?;
                }

                _ => return invalid_context(*tok, word.as_str(prog)).into(),
            }
            i += 1;
        }
        OptionErr::default()
    }

    fn parse_expected_type(
        &mut self, colons: &mut u8, top_index: usize, word: &LocWord, prog: &mut Program,
    ) -> OptionErr<Vec<Op>> {
        let tok = &self[top_index];
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
                err_loc(error, word.loc()).into()
            }

            _ => invalid_context(*tok, word.as_str(prog)).into(),
        }
    }

    fn parse_keyword_ctx(
        &mut self, top_index: usize, word: &LocWord, prog: &mut Program,
    ) -> OptionErr<Vec<Op>> {
        let tok = &self[top_index];
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

            _ => invalid_context(*tok, word.as_str(prog)).into(),
        }
    }

    fn parse_end_ctx(&mut self, word: &LocWord, prog: &mut Program) -> OptionErr<Vec<Op>> {
        self.skip(2);
        self.parse_static_ctx(word, prog)?;
        self.define_proc(word, Contract::default(), prog, ModeType::Declare)
            .into_success()?
    }

    fn parse_static_ctx(&mut self, word: &LocWord, prog: &mut Program) -> OptionErr<Vec<Op>> {
        if let Ok((eval, skip)) = self.compile_eval(prog, word.loc()).value {
            if eval != ANY {
                self.skip(skip);
                let primitive = Primitive::new(word.name(), eval);
                prog.register_const(TypeDescr::Primitive(primitive));

                self.name_scopes
                    .register(word.name(), ParseContext::ConstStruct);
                success!();
            }
        }
        OptionErr::default()
    }

    fn define_proc(
        &mut self, name: &LocWord, contract: Contract, prog: &mut Program, mode: ModeType,
    ) -> LazyResult<Op> {
        let loc = name.loc();

        if self.inside_proc() {
            let error = "Cannot define a procedure inside of another procedure";
            return Err(err_loc(error, loc));
        };

        let proc = Proc::new(name.name(), contract, mode);

        let operand = prog.procs.len();
        self.enter_proc(operand);
        prog.procs.push(proc);

        self.name_scopes
            .register(name.name(), ParseContext::ProcName);

        let prep = if matches!(mode, ModeType::Declare | ModeType::Export) {
            PrepProc
        } else {
            PrepInline
        };

        Ok(self.push_control_block((prep, operand, loc)))
    }

    fn parse_procedure(
        &mut self, word: &LocWord, prog: &mut Program, mode: ModeType,
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
                    .define_proc(word, Contract::new(ins, outs), prog, mode)
                    .into_success()?,

                _ => {
                    let kind = self.check_type_kind(tok, "variable type", prog)?;
                    match prog.get_type_descr(kind) {
                        TypeDescr::Reference(ptr) => {
                            ptr.get_type().conditional_push(arrow, &mut outs, &mut ins);
                        }

                        type_def => {
                            for typ in type_def.units().map(|prim| prim.get_type()) {
                                typ.conditional_push(arrow, &mut outs, &mut ins);
                            }
                        }
                    }
                }
            }
        }

        unexpected_end("proc contract or `:` after procedure definition", loc).into()
    }

    fn parse_memory(&mut self, word: &LocWord, prog: &mut Program) -> LazyResult<()> {
        let loc = word.loc();

        self.expect_keyword(KeywordType::Colon, "`:` after `mem`", loc)?;
        let (mem_size, _) =
            self.expect_by_option(|tok| tok.get_data(INT), "memory size after `:`", loc)?;
        self.expect_keyword(KeywordType::End, "`end` after memory size", loc)?;

        let size = mem_size.try_into().expect("ICE"); //Todo: explain this error
        let ctx = prog.push_mem_by_context(self, word.name(), size);
        self.name_scopes.register(word.name(), ctx);

        Ok(())
    }

    fn parse_struct(&mut self, word: &LocWord, prog: &mut Program) -> OptionErr<Vec<Op>> {
        let loc = word.loc();

        self.expect_keyword(KeywordType::Colon, "`:` after keyword `struct`", loc)?;
        let mut members = Vec::new();

        while let Some(tok) = self.peek() {
            if tok.is_keyword() {
                self.expect_keyword(KeywordType::End, "`end` after struct declaration", loc)?;
                prog.register_struct(StructFields(word.name(), members));
                success!();
            }

            let (member_name, _) = self.expect_word("struct member name", loc)?;
            let kind = self.expect_type_kind("struct member type", prog, loc)?;

            match prog.get_type_descr(kind) {
                TypeDescr::Structure(StructType(fields, _)) => {
                    let value = prog.get_fields_data_type(fields);
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

    fn parse_var(
        &mut self, word: &LocWord, prog: &mut Program, initialize: bool,
    ) -> LazyResult<()> {
        let loc = word.loc();
        let kind = self.expect_type_kind("variable type", prog, loc)?;

        if initialize {
            self.expect_keyword(KeywordType::Equal, "`=` after variable type", loc)?;
            self.eval_const_or_var(false, word, kind, prog)
        } else {
            self.expect_keyword(KeywordType::End, "`end` after variable type", loc)?;

            let type_def = prog.get_type_descr(kind);

            let ref_members: Vec<_> = match type_def.clone() {
                TypeDescr::Structure(StructType(fields, _)) => {
                    fields.ordered_members(self.inside_proc()).collect()
                }
                TypeDescr::Primitive(_) => todo!(),
                TypeDescr::Reference(ptr) => {
                    let struct_type =
                        TypeDescr::reference(word.name(), ptr.get_type(), ptr.ptr_type());
                    self.register_const_or_var(false, word, ptr.ptr_type(), struct_type, prog);
                    return Ok(());
                }
            };

            if ref_members.len() == 1 {
                todo!()
            } else {
                let struct_type =
                    TypeDescr::structure(word.name(), ref_members, type_def.get_type());
                self.register_const_or_var(false, word, type_def.get_type(), struct_type, prog);
            }
            Ok(())
        }
    }

    fn parse_bindings(&mut self, loc: Loc, prog: &mut Program) -> OptionErr<Vec<Op>> {
        let mut bindings = vec![];

        while let Some(tok) = self.peek() {
            if tok.is_keyword() {
                self.expect_keyword(KeywordType::In, "`in` after let bind declaration", loc)?;

                bindings.reverse();
                let current_proc_mut = self
                    .current_proc_mut(prog)
                    .expect("Expected to be used inside a procedure");

                let proc_bindings = &mut current_proc_mut.binds;
                proc_bindings.push(Binds(bindings));

                let bind_block = (BindStack, proc_bindings.len() - 1, loc);
                let result = self.push_control_block(bind_block);

                return OptionErr::new(vec![result]);
            }

            let LabelKind((word, _), typ) = self.expect_label_kind("bind", loc, prog)?;
            self.name_scopes.register(word, ParseContext::Binding);

            bindings.push((word, typ));
        }

        unexpected_end("bind labels after let bind declaration", loc).into()
    }

    fn eval_const_or_var(
        &mut self, is_constant: bool, word: &LocWord, reftype: DataType, prog: &mut Program,
    ) -> LazyResult<()> {
        let stk = prog.get_type_descr(reftype).clone();
        let &(name, loc) = word;

        let (result, skip) = match self.compile_eval_n(stk.count(), prog, loc).value {
            Ok(value) => value,
            Err(either) => {
                return Err(either.either(
                    |tok| {
                        err_loc("Failed to parse an valid struct value at compile-time", tok.loc())
                    },
                    |err| err,
                ))
            }
        };

        let end_loc = self
            .skip(skip)
            .expect("Should not fail if `compile_eval_n` was sucessfull")
            .loc();

        let struct_type = match result {
            Either::Left(tok) => {
                match &stk {
                    TypeDescr::Primitive(_) => todo!(),

                    TypeDescr::Structure(StructType(fields, _)) => {
                        expect_type(&tok, &fields[0], end_loc)?;
                    }

                    TypeDescr::Reference(ptr) => {
                        expect_type(&tok, ptr, end_loc)?;
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
                            "Should not fail if `fields` and `eval_items` have the same lenght",
                        ),
                    TypeDescr::Primitive(_) => todo!(),
                    TypeDescr::Reference(_) => todo!(),
                };

                TypeDescr::structure(name, def_members, reftype)
            }
        };

        self.register_const_or_var(is_constant, word, reftype, struct_type, prog);
        Ok(())
    }

    fn register_const_or_var(
        &mut self, is_constant: bool, word: &LocWord, reftype: DataType, struct_type: TypeDescr,
        prog: &mut Program,
    ) {
        let ctx = if is_constant {
            prog.register_const(struct_type);
            ParseContext::ConstStruct
        } else {
            self.register_var(struct_type, prog)
        };

        self.name_scopes.register(word.name(), ctx);
        self.structs.push(TypedWord::new(word.name(), reftype));
    }

    pub fn register_var(&mut self, struct_word: TypeDescr, prog: &mut Program) -> ParseContext {
        if let Some(proc) = self.current_proc_mut(prog) {
            let Some(data) = proc.get_data_mut() else {
                todo!("Cannot use variables inside inlined procedures")
            };

            data.local_vars.push(struct_word);
            ParseContext::LocalVar
        } else {
            prog.global_vars.push(struct_word);
            ParseContext::GlobalVar
        }
    }

    pub fn lex_path(&mut self, path: &Path, prog: &mut Program) -> LazyResult<&mut Self> {
        let module_name = path.get_dir()?;
        let source_name = path.get_file()?;

        self.lex_include(path, module_name, source_name, prog)
    }

    pub fn lex_source(
        &mut self, source: &str, module: &str, prog: &mut Program,
    ) -> LazyResult<&mut Self> {
        let path = Path::new(module).join(source).with_extension("fire");

        let module_name = path.get_dir()?;
        let source_name = path.get_file()?;

        if prog.has_source(source_name, module_name) {
            return Ok(self);
        }

        self.lex_include(&path, module_name, source_name, prog)
    }

    pub fn lex_include(
        &mut self, path: &Path, module: &str, source: &str, prog: &mut Program,
    ) -> LazyResult<&mut Self> {
        debug!("Including: {:?}", path);
        let file = File::open(path).with_context(|| format!("Could not read file `{path:?}`"))?;

        let lex = prog.new_lexer(file, source, module);
        self.read_lexer(prog, lex, module)
    }

    pub fn read_lexer(
        &mut self, prog: &mut Program, mut lex: Lexer, module: &str,
    ) -> LazyResult<&mut Self> {
        while let Some(token) = prog.lex_next_token(&mut lex).value? {
            if &token != KeywordType::Include {
                self.ir_tokens.push_back(token);
                continue;
            }

            let (tok, _) = expect_token_by_option(
                prog.lex_next_token(&mut lex).value?,
                IRTokenExt::get_word,
                "include source name",
                token.loc(),
            )?;

            let include_path = prog.get_word(tok);
            self.lex_source(&include_path, module, prog)?;
        }
        Ok(self)
    }
}

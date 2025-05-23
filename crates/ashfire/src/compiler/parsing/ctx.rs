use std::{io::Read, path::Path};

use IndexOp::*;
use ashfire_types::{core::*, data::*, enums::*, proc::ModeData};
use firelib::{Result, ShortCircuit, lazy::LazyCtx, lexer::Loc};

use super::{parser::Parser, types::*};
use crate::{compiler::ctx::*, firelib::span::Span};

impl Ctx {
    /// Searches for a `binding` to load that matches the given [`word`][LocWord]
    /// on the current [`Proc`].
    pub fn get_binding(&self, (word, loc): &LocWord, parser: &Parser) -> Option<Vec<Op>> {
        parser
            .current_proc(self)
            .and_then(|proc| proc.bindings().position(|(key, _)| word.eq(key)))
            .map(|index| vec![(OpType::IndexOp(LoadBind, index), *loc)])
    }

    /// Searches for a `binding` reference that matches the given [`word`][LocWord]
    /// on the current [`Proc`].
    pub fn get_binding_ref(&self, (word, loc): &LocWord, parser: &Parser) -> Option<Vec<Op>> {
        parser
            .current_proc(self)
            .and_then(|proc| proc.bindings().position(|(key, _)| word.eq(key)))
            .map(|index| vec![(OpType::IndexOp(PushBind, index), *loc)])
    }

    /// Searches for a `mem` that matches the given [`word`][LocWord]
    /// on the current [`Proc`].
    pub fn get_local_mem(&self, (word, loc): &LocWord, parser: &Parser) -> Option<Vec<Op>> {
        parser
            .current_proc_data(self)
            .and_then(|proc| proc.local_mems.iter().find(|mem| word.eq(mem)))
            .map(|mem| vec![(OpType::MemOp(MemOp::PushLocalMem, mem.value()), *loc)])
    }

    /// Searches for a `global mem` that matches the given [`LocWord`] name.
    pub fn get_global_mem(&self, (word, loc): &LocWord) -> Option<Vec<Op>> {
        self.get_memory()
            .iter()
            .find(|mem| word.eq(mem))
            .map(|mem| vec![(OpType::MemOp(MemOp::PushGlobalMem, mem.value()), *loc)])
    }

    /// Searches for a `const` that matches the given[`word`][LocWord]
    /// and parses it to an [`Op`].
    pub fn get_const_struct(&self, word: &LocWord) -> Option<Vec<Op>> {
        let &(name, loc) = word;
        self.get_const_by_name(name).map(|tword| match tword {
            TypeDescr::Structure(StructType(fields, _)) => {
                fields.units().map(|prim| Op::new((&prim, loc))).collect()
            }
            TypeDescr::Primitive(prim) => vec![Op::new((prim, loc))],
            TypeDescr::Reference(_) => todo!(),
        })
    }

    pub fn get_intrinsic(&self, word: &LocWord) -> Option<Vec<Op>> {
        self.get_intrinsic_type(&word.as_string(self))
            .map(|intr| vec![Op::new((intr, word.loc()))])
    }

    pub fn get_proc_name(&self, (word, loc): &LocWord) -> Option<Vec<Op>> {
        self.procs()
            .iter()
            .enumerate()
            .find(|(_, proc)| word.eq(&proc.name))
            .map(|(index, proc)| {
                let call = match &proc.mode_data {
                    ModeData::Inlined(..) => CallInline,
                    _ => Call,
                };
                vec![(OpType::IndexOp(call, index), *loc)]
            })
    }

    /// Searches for a local `variable` that matches the given [`word`][LocWord]
    /// and parses `store` and `pointer` information.
    pub fn get_local_var(
        &self, word: &LocWord, var_typ: VarWordType, parser: &Parser,
    ) -> OptionErr<Vec<Op>> {
        let proc = parser
            .current_proc_data(self)
            .or_return(OptionErr::default)?;
        self.try_get_var(word, &proc.local_vars, MemOp::PushLocal, var_typ, parser)
    }

    /// Searches for a global `variable` that matches the given [`word`][LocWord]
    /// and parses `store` and `pointer` information.
    pub fn get_global_var(
        &self, word: &LocWord, var_typ: VarWordType, parser: &Parser,
    ) -> OptionErr<Vec<Op>> {
        self.try_get_var(word, self.global_vars(), MemOp::PushGlobal, var_typ, parser)
    }

    fn try_get_var(
        &self, word: &LocWord, vars: &[TypeDescr], push_type: MemOp, var_typ: VarWordType,
        parser: &Parser,
    ) -> OptionErr<Vec<Op>> {
        let &(name, loc) = word;

        if word.as_str(self).contains('.') {
            let (var, offset) = self
                .try_get_field(word, vars)
                .value?
                .or_return(OptionErr::default)?;
            return self.unpack_type(var, push_type, offset, var_typ, loc);
        }

        let data_type = vars
            .iter()
            .any(|val| name == val.name())
            .then(|| parser.find_data_type(name))
            .flatten()
            .or_return(OptionErr::default)?;

        OptionErr::new(if var_typ == VarWordType::Pointer {
            let stk_id = self.get_type_ptr(data_type);
            vars.get_pointer(word, push_type, stk_id)
        } else {
            let type_descr = self.get_type_descr(data_type);
            vars.get_fields(word, push_type, type_descr, var_typ == VarWordType::Store)
        })
    }

    fn unpack_type(
        &self, descr: &TypeDescr, push_type: MemOp, mut offset: u16, var_typ: VarWordType, loc: Loc,
    ) -> OptionErr<Vec<Op>> {
        let mut result = Vec::new();
        match descr {
            TypeDescr::Primitive(prim) => {
                let prim_type = prim.get_type();
                result.push((OpType::MemOp(push_type, offset), loc));

                if var_typ == VarWordType::Store {
                    result.insert(0, (OpType::ExpectType(prim_type), loc));
                    result.push(Op::new((IntrinsicType::Store32, loc)));
                } else if var_typ == VarWordType::Pointer {
                    let ptr_id = self.get_type_ptr(prim_type);
                    result.push(Op::new((IntrinsicType::Cast(ptr_id), loc)));
                } else {
                    result.extend([
                        Op::new((IntrinsicType::Load32, loc)),
                        Op::new((IntrinsicType::Cast(prim_type), loc)),
                    ]);
                }
            }

            TypeDescr::Structure(StructType(_, data_type)) => {
                if var_typ == VarWordType::Store {
                    todo!();
                }

                if push_type == MemOp::PushLocal {
                    offset += WORD_USIZE;
                }

                let ptr_id = self.get_type_ptr(*data_type);

                result.extend([
                    (OpType::MemOp(push_type, offset), loc),
                    Op::new((IntrinsicType::Cast(ptr_id), loc)),
                ]);

                if var_typ != VarWordType::Pointer {
                    result.push((OpType::UnpackType(None), loc));
                }
            }

            TypeDescr::Reference(_) => todo!(),
        }

        OptionErr::new(result)
    }

    fn try_get_field<'t>(
        &self, word: &LocWord, vars: &'t [TypeDescr],
    ) -> OptionErr<(&'t TypeDescr, u16)> {
        let name = word.as_str(self);
        let names: Vec<_> = name.split('.').collect();
        let loc = word.loc();

        let Some(first) = self.get_key(names[0]) else {
            todo!()
        };

        let (mut offset, i) = vars.get_offset(first).or_return(OptionErr::default)?;

        let field_names = names.into_iter().skip(1);
        let mut var = &vars[i];

        for field_name in field_names {
            let TypeDescr::Structure(StructType(fields, _)) = var else {
                todo!()
            };

            let Some(field_key) = self.get_key(field_name) else {
                todo!()
            };

            let var_name = var.name();

            let (diff, index) = fields.get_offset(field_key).with_err_ctx(lazyctx!(
                |f| "{}The variable `{}` does not contain the field `{}`",
                f(Fmt::Loc(loc)),
                f(Fmt::Key(var_name)),
                f(Fmt::Key(field_key))
            ))?;

            offset += diff;
            var = &fields[index];
        }

        OptionErr::new((var, offset * WORD_USIZE))
    }

    pub fn push_mem_by_context(&mut self, parser: &Parser, word: Name, size: u16) -> ParseContext {
        let Some(proc) = parser.current_proc_mut(self) else {
            self.push_mem(word, size);
            return ParseContext::GlobalMem;
        };

        let Some(data) = proc.get_data_mut() else {
            todo!();
        };

        data.push_mem(word, size);
        ParseContext::LocalMem
    }

    pub fn include(
        &mut self, parser: &mut Parser, reader: impl Read, source: &str, module: &str,
    ) -> Result<()> {
        let lex = self.new_lexer(reader, source, module);
        parser
            .read_lexer(self, lex, module)
            .try_or_apply(&|fmt| self.format(fmt))?;
        Ok(())
    }

    pub fn compile_buffer(&mut self, source: &str, reader: impl Read) -> Result<&mut Self> {
        let mut parser = Parser::new();
        self.include(&mut parser, reader, source, "")?;
        self.compile_parser(parser)
    }

    pub fn compile_parser(&mut self, mut parser: Parser) -> Result<&mut Self> {
        parser
            .parse_tokens(self)
            .try_or_apply(&|fmt| self.format(fmt))?;

        info!("Compilation done");
        Ok(self)
    }

    pub fn compile_file(&mut self, path: &Path) -> Result<&mut Self> {
        info!("Compiling file: {}", path.display());

        let mut parser = Parser::new();
        self.include_path(&mut parser, path)?;
        self.compile_parser(parser)
    }

    pub fn include_path(&mut self, parser: &mut Parser, path: &Path) -> Result<()> {
        parser
            .lex_path(path, self)
            .try_or_apply(&|fmt| self.format(fmt))?;
        Ok(())
    }
}

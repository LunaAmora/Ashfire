use std::{io::Read, path::Path};

use ashfire_types::{core::*, data::*, enums::*, proc::Mode};
use firelib::{lazy::LazyCtx, lexer::Loc, ShortCircuit};

use super::{parser::Parser, types::*};
use crate::compiler::{program::*, utils::err_loc};

impl Program {
    /// Searches for a `binding` to load that matches the given [`word`][LocWord]
    /// on the current [`Proc`].
    pub fn get_binding(&self, word: &LocWord, parser: &Parser) -> Option<Vec<Op>> {
        parser
            .current_proc(self)
            .and_then(|proc| proc.bindings().position(|(key, _)| word.eq(key)))
            .map(|index| vec![Op(OpType::LoadBind, index as i32, word.loc())])
    }

    /// Searches for a `binding` reference that matches the given [`word`][LocWord]
    /// on the current [`Proc`].
    pub fn get_binding_ref(&self, word: &LocWord, parser: &Parser) -> Option<Vec<Op>> {
        parser
            .current_proc(self)
            .and_then(|proc| proc.bindings().position(|(key, _)| word.eq(key)))
            .map(|index| vec![Op(OpType::PushBind, index as i32, word.loc())])
    }

    /// Searches for a `mem` that matches the given [`word`][LocWord]
    /// on the current [`Proc`].
    pub fn get_local_mem(&self, word: &LocWord, parser: &Parser) -> Option<Vec<Op>> {
        parser
            .current_proc_data(self)
            .and_then(|proc| proc.local_mems.iter().find(|mem| word.eq(mem)))
            .map(|local| vec![Op(OpType::PushLocalMem, local.offset(), word.loc())])
    }

    /// Searches for a `global mem` that matches the given [`LocWord`] name.
    pub fn get_global_mem(&self, word: &LocWord) -> Option<Vec<Op>> {
        self.get_memory()
            .iter()
            .find(|mem| word.eq(mem))
            .map(|global| vec![Op(OpType::PushGlobalMem, global.offset(), word.loc())])
    }

    /// Searches for a `const` that matches the given[`word`][LocWord]
    /// and parses it to an [`Op`].
    pub fn get_const_struct(&self, word: &LocWord) -> Option<Vec<Op>> {
        self.get_const_by_name(word).map(|tword| match tword {
            TypeDescr::Structure(StructType(fields, _)) => fields
                .units()
                .map(|val| Op::from((&val, word.loc())))
                .collect(),
            TypeDescr::Primitive(val) => vec![Op::from((val, word.loc()))],
            TypeDescr::Reference(..) => todo!(),
        })
    }

    /// Searches for a `struct` that matches the given [`StrKey`],
    /// returning its type.
    fn try_get_struct_descr(&self, word: &StrKey, parser: &Parser) -> Option<&TypeDescr> {
        parser
            .structs()
            .iter()
            .find(|stk| word.eq(stk))
            .map(|name| self.get_type_descr(name.1))
    }

    pub fn get_intrinsic(&mut self, word: &LocWord) -> Option<Vec<Op>> {
        self.get_intrinsic_type(&word.as_string(self))
            .map(|i| vec![Op::from((i, word.loc()))])
    }

    pub fn get_proc_name(&self, word: &LocWord) -> Option<Vec<Op>> {
        self.procs
            .iter()
            .enumerate()
            .find(|(_, proc)| word.eq(&proc.name))
            .map(|(index, proc)| {
                let call = if let Mode::Inlined(..) = &proc.mode {
                    OpType::CallInline
                } else {
                    OpType::Call
                };
                vec![Op(call, index as i32, word.loc())]
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
        self.try_get_var(word, &proc.local_vars, OpType::PushLocal, var_typ, parser)
    }

    /// Searches for a global `variable` that matches the given [`word`][LocWord]
    /// and parses `store` and `pointer` information.
    pub fn get_global_var(
        &self, word: &LocWord, var_typ: VarWordType, parser: &Parser,
    ) -> OptionErr<Vec<Op>> {
        self.try_get_var(word, &self.global_vars, OpType::PushGlobal, var_typ, parser)
    }

    fn try_get_var(
        &self, word: &LocWord, vars: &[TypeDescr], push_type: OpType, var_typ: VarWordType,
        parser: &Parser,
    ) -> OptionErr<Vec<Op>> {
        if word.as_str(self).contains('.') {
            let (var, offset) = self
                .try_get_field(word, vars)
                .value?
                .or_return(OptionErr::default)?;
            return self.unpack_struct(var, push_type, offset, var_typ, word.loc());
        }

        let type_descr = vars
            .iter()
            .any(|val| val.name().eq(word))
            .then(|| self.try_get_struct_descr(word, parser))
            .flatten()
            .or_return(OptionErr::default)?;

        OptionErr::new(if var_typ == VarWordType::Pointer {
            let value = type_descr.type_id();
            let stk_id = self.try_get_type_ptr(value).unwrap();

            vars.get_pointer(word, push_type, stk_id)
        } else {
            vars.get_fields(word, push_type, type_descr, var_typ == VarWordType::Store)
        })
    }

    fn unpack_struct(
        &self, stk: &TypeDescr, push_type: OpType, mut offset: usize, var_typ: VarWordType,
        loc: Loc,
    ) -> OptionErr<Vec<Op>> {
        let mut result = Vec::new();
        match stk {
            TypeDescr::Primitive(Primitive(.., typ @ TypeId(id))) => {
                result.push(Op(push_type, offset as i32, loc));

                if var_typ == VarWordType::Store {
                    result.insert(0, Op(OpType::ExpectType, id.operand(), loc));
                    result.push(Op::from((IntrinsicType::Store32, loc)));
                } else if var_typ == VarWordType::Pointer {
                    let TypeId(ptr_id) = self.try_get_type_ptr(*typ).unwrap();
                    result.push(Op::from((IntrinsicType::Cast(ptr_id), loc)));
                } else {
                    result.extend([
                        Op::from((IntrinsicType::Load32, loc)),
                        Op::from((IntrinsicType::Cast(*id), loc)),
                    ]);
                }
            }

            TypeDescr::Structure(StructType(_, type_id)) => {
                if var_typ == VarWordType::Store {
                    todo!();
                }

                if push_type == OpType::PushLocal {
                    offset += 1;
                }

                let TypeId(ptr_id) = self.try_get_type_ptr(*type_id).unwrap();

                result.extend([
                    Op(push_type, offset as i32, loc),
                    Op::from((IntrinsicType::Cast(ptr_id), loc)),
                ]);

                if var_typ != VarWordType::Pointer {
                    result.push(Op(OpType::Unpack, 0, loc));
                }
            }

            TypeDescr::Reference(_) => todo!(),
        };

        OptionErr::new(result)
    }

    fn try_get_field<'a>(
        &self, word: &LocWord, vars: &'a [TypeDescr],
    ) -> OptionErr<(&'a TypeDescr, usize)> {
        let fields: Vec<_> = word.as_str(self).split('.').collect();
        let loc = word.loc();

        let Some(first) = self.get_key(fields[0]) else {
            todo!()
        };

        let (mut offset, i) = vars.get_offset(&first).or_return(OptionErr::default)?;

        let fields = fields.into_iter().skip(1);
        let mut var = &vars[i];

        for field_name in fields {
            let TypeDescr::Structure(StructType(fields, _)) = var else {
                todo!()
            };

            let Some(field_key) = self.get_key(field_name) else {
                todo!()
            };

            let Some((diff, index)) = fields.get_offset(&field_key) else {
                let error = format!("The variable `{}` does not contain the field `{field_name}`",
                    var.name().as_str(self));
                return err_loc(error, loc).into();
            };

            offset += diff;
            var = &fields[index];
        }

        OptionErr::new((var, offset))
    }

    pub fn push_mem_by_context(
        &mut self, parser: &Parser, word: &StrKey, size: usize,
    ) -> ParseContext {
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

    pub fn compile_buffer(
        &mut self, path: &Path, source: &str, reader: impl Read + 'static,
    ) -> firelib::Result<&mut Self> {
        info!("Compiling buffer: {:?}", source);

        let lexer = self.new_lexer(source, reader);

        Parser::new()
            .read_lexer(self, lexer, path)
            .and_then(|parser| parser.parse_tokens(self))
            .try_or_apply(&|fmt| self.format(fmt))?;

        info!("Compilation done");
        Ok(self)
    }

    pub fn compile_file(&mut self, path: &Path) -> firelib::Result<&mut Self> {
        info!("Compiling file: {:?}", path);

        Parser::new()
            .lex_file(path, self)
            .and_then(|parser| parser.parse_tokens(self))
            .try_or_apply(&|fmt| self.format(fmt))?;

        info!("Compilation done");
        Ok(self)
    }
}

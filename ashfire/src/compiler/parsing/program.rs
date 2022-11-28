use std::path::Path;

use ashfire_types::{core::*, data::*, enums::*, proc::Mode};
use ashlib::Either;
use firelib::{lazy::LazyCtx, ShortCircuit};

use super::{parser::Parser, types::*};
use crate::compiler::{program::*, utils::err_loc};

impl Program {
    /// Searches for a `binding` that matches the given [`word`][LocWord]
    /// on the current [`Proc`].
    pub fn get_binding(&self, word: &LocWord, parser: &Parser) -> Option<Vec<Op>> {
        parser
            .current_proc_data(self)
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
    pub fn get_local_mem(&self, word: &LocWord, parser: &Parser) -> Option<Vec<Op>> {
        parser
            .current_proc_data(self)
            .and_then(|proc| proc.local_mems.iter().find(|mem| word.eq(mem)))
            .map(|local| vec![Op::new(OpType::PushLocalMem, local.offset(), word.loc)])
    }

    /// Searches for a `global mem` that matches the given [`LocWord`] name.
    pub fn get_global_mem(&self, word: &LocWord) -> Option<Vec<Op>> {
        self.get_memory()
            .iter()
            .find(|mem| word.eq(mem))
            .map(|global| vec![Op::new(OpType::PushGlobalMem, global.offset(), word.loc)])
    }

    /// Searches for a `const` that matches the given[`word`][LocWord]
    /// and parses it to an [`Op`].
    pub fn get_const_struct(&self, word: &LocWord) -> Option<Vec<Op>> {
        self.get_const_by_name(word).map(|tword| match tword {
            StructType::Root(root) => root.units().map(|val| Op::from((val, word.loc))).collect(),
            StructType::Unit(val) => vec![Op::from((val, word.loc))],
        })
    }

    pub fn get_type_kind(
        &self, word: &LocWord, as_ref: bool,
    ) -> Option<Either<&StructDef, ValueType>> {
        if as_ref {
            self.get_type_ptr(word).map(Either::Right)
        } else {
            self.get_type_def(word).map(Either::Left)
        }
    }

    /// Searches for a `struct` that matches the given [`StrKey`],
    /// returning its type.
    fn try_get_struct_type(&self, word: &StrKey, parser: &Parser) -> Option<&StructDef> {
        parser
            .structs()
            .iter()
            .find(|stk| word.eq(stk))
            .and_then(|stk| self.get_type_def(stk))
    }

    pub fn get_type_def<O: Operand>(&self, word_id: O) -> Option<&StructDef> {
        self.structs_types
            .iter()
            .find(|def| word_id.str_key().eq(def))
    }

    pub fn get_type_ptr(&self, word: &LocWord) -> Option<ValueType> {
        self.structs_types
            .iter()
            .position(|def| word.eq(def))
            .map(|i| ValueType::Ptr(Value::from(i)))
    }

    pub fn try_get_struct_def(&self, tok: &IRToken) -> Option<&StructDef> {
        fold_bool!(tok == TokenType::Word, self.get_type_def(tok))
    }

    pub fn get_intrinsic(&self, word: &LocWord) -> Option<Vec<Op>> {
        self.get_intrinsic_type(word.as_str(self))
            .map(|i| vec![Op::from((i, word.loc))])
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
                vec![Op::new(call, index as i32, word.loc)]
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
        &self, word: &LocWord, vars: &[StructType], push_type: OpType, var_typ: VarWordType,
        parser: &Parser,
    ) -> OptionErr<Vec<Op>> {
        if word.as_str(self).contains('.') {
            return self
                .try_get_field(word, vars)
                .value?
                .map(|(var, offset)| unpack_struct(var, push_type, offset, var_typ, word.loc))
                .into();
        }

        let struct_type = vars
            .iter()
            .any(|val| word.eq(val))
            .then(|| self.try_get_struct_type(word, parser))
            .flatten()
            .or_return(OptionErr::default)?;

        OptionErr::new(if var_typ == VarWordType::Pointer {
            let stk_id = self.get_struct_type_id(struct_type).unwrap() as i32;
            vars.get_pointer(word, push_type, stk_id)
        } else {
            vars.get_fields(word, push_type, struct_type, var_typ == VarWordType::Store)
        })
    }

    fn try_get_field<'a>(
        &self, word: &LocWord, vars: &'a [StructType],
    ) -> OptionErr<(&'a StructType, usize)> {
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

            let Some((diff, index)) = root.members().get_offset(&field_key) else {
                let error = format!("The variable `{}` does not contain the field `{field_name}`",
                    var.as_str(self));
                return err_loc(error, loc).into();
            };

            offset += diff;
            var = &root.members()[index];
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

use std::{
    collections::HashMap,
    str::{from_utf8, FromStr},
};

use ashfire_types::{
    core::*,
    data::*,
    enums::IntrinsicType,
    lasso::{Key, Rodeo},
    proc::{Data, Proc},
};
use firelib::lexer::Loc;

use super::utils;

pub type OptionErr<T> = utils::OptionErr<'static, T>;
pub type LazyResult<T> = utils::LazyResult<'static, T>;
pub type LazyError = utils::LazyError<'static>;

pub trait InternalString {
    fn as_str<'p>(&self, prog: &'p Program) -> &'p str;
    fn as_string(&self, prog: &Program) -> String;
}

impl InternalString for Name {
    fn as_str<'p>(&self, prog: &'p Program) -> &'p str {
        prog.interner.resolve(self)
    }

    fn as_string(&self, prog: &Program) -> String {
        prog.interner.resolve(self).to_owned()
    }
}

fn intern_all<const N: usize>(rodeo: &mut Rodeo, strings: [&'static str; N]) -> [Name; N] {
    strings
        .iter()
        .map(|s| rodeo.get_or_intern_static(s))
        .collect::<Vec<_>>()
        .try_into()
        .unwrap()
}

#[derive(Default)]
pub struct Program {
    pub ops: Vec<Op>,
    pub procs: Vec<Proc>,
    pub global_vars: Vec<TypeDescr>,
    pub block_contracts: HashMap<usize, (usize, usize)>,
    structs_types: Vec<TypeDescr>,
    included_sources: HashMap<Name, Name>,
    consts: Vec<TypeDescr>,
    interner: Rodeo,
    mem_size: usize,
    memory: Vec<OffsetWord>,
    data_size: usize,
    data: Vec<OffsetData>,
}

impl Program {
    pub fn new() -> Self {
        let mut interner = Rodeo::new();

        let names = [
            "", "any", "*any", "bool", "int", "ptr", "str", "len", "data",
        ];
        let [_, any, any_ptr, bool, int, ptr, str, len, data] = intern_all(&mut interner, names);

        let structs_types = vec![
            TypeDescr::primitive(any, TypeId::ANY),
            TypeDescr::reference(any_ptr, TypeId::ANY_PTR, TypeId::ANY),
            TypeDescr::primitive(bool, TypeId::BOOL),
            TypeDescr::primitive(int, TypeId::INT),
            TypeDescr::primitive(ptr, TypeId::PTR),
            TypeDescr::structure(
                str,
                vec![
                    TypeDescr::primitive(len, TypeId::INT),
                    TypeDescr::primitive(data, TypeId::PTR),
                ],
                TypeId::STR,
            ),
        ];

        Self { structs_types, interner, ..Default::default() }
    }

    pub fn has_source(&self, source: &str, module: &str) -> bool {
        if let (Some(src), Some(modl)) = (self.interner.get(source), self.interner.get(module)) {
            self.included_sources
                .get(&src)
                .map_or(false, |module| module == &modl)
        } else {
            false
        }
    }

    pub fn push_source(&mut self, source: &str, module: &str) -> usize {
        let mkey = self.interner.get_or_intern(module);
        let skey = self.interner.get_or_intern(source);

        //this should only insert a new key-value pair, should check if `has_source` first
        self.included_sources.insert(skey, mkey);
        skey.into_usize()
    }

    pub fn push_mem(&mut self, word: Name, size: usize) {
        let value = OffsetWord::new(word, self.mem_size);
        self.memory.push(value);
        self.mem_size += size;
    }

    pub fn push_data(&mut self, mut word: String, size: usize) -> usize {
        if word.ends_with("\\0") {
            word.push('0');
        };

        let name = self.get_or_intern(&word);
        let value = OffsetData::new(name, size, self.data_size as i32);
        self.data.push(value);
        self.data_size += size;
        self.data.len() - 1
    }

    pub fn register_const(&mut self, struct_type: TypeDescr) {
        self.consts.push(struct_type);
    }

    pub fn data_size(&self) -> usize {
        self.data_size
    }

    pub fn data_start(&self) -> i32 {
        word_aligned(self.mem_size)
    }

    pub fn global_vars_start(&self) -> i32 {
        self.data_start() + word_aligned(self.data_size)
    }

    pub fn global_vars_size(&self) -> i32 {
        self.global_vars.iter().fold(0, |acc, var| acc + var.size()) as i32
    }

    pub fn stack_start(&self) -> i32 {
        self.global_vars_start() + self.global_vars_size()
    }

    pub fn get_or_intern(&mut self, word: &str) -> Name {
        self.interner.get_or_intern(word)
    }

    pub fn get_key(&self, word: &str) -> Option<Name> {
        self.interner.get(word)
    }

    pub fn get_word(&self, index: usize) -> String {
        let name = Name::try_from_usize(index).unwrap();
        self.interner.resolve(&name).to_owned()
    }

    pub fn get_data(&self, index: usize) -> &OffsetData {
        &self.data[index]
    }

    pub fn get_data_str(&self, index: usize) -> &str {
        self.get_data(index).as_str(self)
    }

    pub fn get_proc(&self, index: usize) -> &Proc {
        &self.procs[index]
    }

    pub fn get_contract(&self, index: usize) -> (usize, usize) {
        self.block_contracts[&(index)]
    }

    pub fn get_all_data(&self) -> &[OffsetData] {
        &self.data
    }

    pub fn get_memory(&self) -> &[OffsetWord] {
        &self.memory
    }

    pub fn current_ip(&self) -> usize {
        self.ops.len()
    }

    fn data_name(&self, value: TypeId) -> String {
        match value {
            TypeId::INT => "Integer",
            TypeId::BOOL => "Boolean",
            TypeId::PTR => "Pointer",
            TypeId::STR => "String",
            TypeId::ANY => "Any",
            TypeId(n) => self.structs_types[n].name().as_str(self),
        }
        .to_owned()
    }

    pub fn type_name(&self, typ: TokenType) -> String {
        match typ {
            TokenType::Keyword => "Keyword",
            TokenType::Word => "Word or Intrinsic",
            TokenType::Data(data) => match data {
                ValueType(value) => return self.data_name(value),
            },
            TokenType::Str => "String Id",
        }
        .to_owned()
    }

    fn data_display(value: TypeId, operand: i32) -> String {
        match value {
            TypeId::BOOL => fold_bool!(operand != 0, "True", "False").to_owned(),
            TypeId::PTR => format!("*{operand}"),
            TypeId(_) => operand.to_string(),
        }
    }

    pub fn type_display(&self, tok: &IRToken) -> String {
        match tok.get_type() {
            TokenType::Keyword => format!("{:?}", tok.as_keyword()),
            TokenType::Word => self.get_word(tok.index()),
            TokenType::Str => self.get_data_str(tok.index()).to_owned(),
            TokenType::Data(ValueType(id)) => Self::data_display(id, tok.operand()),
        }
    }

    pub fn loc_fmt<L: Location>(&self, loc: L) -> String {
        let Loc { file_index, line, col } = loc.loc();
        self.included_sources
            .get_key_value(&Name::try_from_usize(file_index).unwrap())
            .map_or_else(String::new, |(src, modl)| {
                format!("{}/{}:{line}:{col} ", modl.as_str(self), src.as_str(self))
            })
    }

    pub fn format(&self, fmt: Fmt) -> String {
        match fmt {
            Fmt::Loc(loc) => self.loc_fmt(loc),
            Fmt::Tok(tok) => self.type_display(&tok),
            Fmt::Typ(typ) => self.type_name(typ),
        }
    }

    pub fn get_intrinsic_type(&mut self, word: &str) -> Option<IntrinsicType> {
        IntrinsicType::from_str(word).ok().or_else(|| {
            Some(match word.as_bytes() {
                [b'#', b'*', rest @ ..] => IntrinsicType::Cast(self.get_cast_type_ptr(rest)?),
                [b'#', rest @ ..] => IntrinsicType::Cast(self.get_cast_type(rest)?),
                _ => return None,
            })
        })
    }

    fn name_from_utf8(&self, rest: &[u8]) -> Option<Name> {
        from_utf8(rest).ok().and_then(|str| self.get_key(str))
    }

    fn get_cast_type(&self, rest: &[u8]) -> Option<TypeId> {
        self.name_from_utf8(rest).map(|key| self.get_type_id(key))?
    }

    fn get_cast_type_ptr(&mut self, rest: &[u8]) -> Option<TypeId> {
        self.get_cast_type(rest).map(|id| self.get_type_ptr(id))
    }

    pub fn get_type_index(&self, word: Name) -> Option<usize> {
        self.structs_types
            .iter()
            .position(|def| word.eq(&def.name()))
    }

    pub fn get_type_id(&self, word: Name) -> Option<TypeId> {
        self.get_type_index(word).map(TypeId)
    }

    pub fn get_type_id_by_str(&self, word: &str) -> Option<TypeId> {
        self.get_key(word).and_then(|key| self.get_type_id(key))
    }

    pub fn get_type_descr(&self, type_id: TypeId) -> &TypeDescr {
        &self.structs_types[type_id.0]
    }

    pub fn try_get_type_ptr(&self, type_id: TypeId) -> Option<TypeId> {
        let name = self.get_type_descr(type_id).name();
        let ptr_name = format!("*{}", name.as_str(self));
        self.get_type_id_by_str(&ptr_name)
    }

    pub fn get_type_ptr(&mut self, type_id: TypeId) -> TypeId {
        let name = self.get_type_descr(type_id).name();
        let ptr_name = format!("*{}", name.as_str(self));

        if let Some(id) = self.get_type_id_by_str(&ptr_name) {
            return id;
        }

        let word_id = self.get_or_intern(&ptr_name);
        let new_type_id = TypeId(self.structs_types.len());
        let stk = TypeDescr::reference(word_id, new_type_id, type_id);

        self.structs_types.push(stk);
        new_type_id
    }

    pub fn register_struct(&mut self, stk: StructFields) -> TypeId {
        let type_id = TypeId(self.structs_types.len());
        let descr = TypeDescr::Structure(StructType(stk, type_id));
        self.structs_types.push(descr);
        self.get_type_ptr(type_id); // Todo: Remove this Hack to auto register an ptr type
        type_id
    }

    /// Searches for a `const` that matches the given [`Name`].
    pub fn get_const_by_name(&self, word: Name) -> Option<&TypeDescr> {
        self.consts.iter().find(|cnst| word.eq(&cnst.name()))
    }

    #[cfg(debug_assertions)]
    #[allow(dead_code)]
    fn op_debug(&self, op: &Op) -> String {
        use ashfire_types::enums::{ControlOp, IndexOp, OpType};
        let &Op(op_type, _) = op;
        match op_type {
            OpType::Intrinsic(IntrinsicType::Cast(type_id)) => {
                format!("Intrinsic Cast [{}]", self.type_name(type_id.get_type()))
            }

            OpType::Intrinsic(intrinsic) => {
                format!("Intrinsic [{intrinsic:?}]")
            }

            OpType::IndexOp(op, index) => match op {
                IndexOp::Call => format!("Call [{}]", self.get_proc(index).name.as_str(self)),
                IndexOp::CallInline => {
                    format!("Inline [{}]", self.get_proc(index).name.as_str(self))
                }
                _ => format!("{op:?}: [{index}]"),
            },

            OpType::ControlOp(op, index) if matches!(op, ControlOp::EndProc) => {
                format!("EndProc [{}]", self.get_proc(index).name.as_str(self))
            }

            OpType::ControlOp(op, index) => format!("{op:?}: [{index}]"),

            _ => format!("{op_type:?}"),
        }
    }
}

pub trait Visitor {
    fn set_index(&mut self, i: Option<usize>);
    fn get_index(&self) -> Option<usize>;

    fn inside_proc(&self) -> bool {
        self.get_index().is_some()
    }

    fn current_proc<'p>(&self, program: &'p Program) -> Option<&'p Proc> {
        self.get_index().and_then(|i| program.procs.get(i))
    }

    fn current_proc_data<'p>(&self, program: &'p Program) -> Option<&'p Data> {
        self.current_proc(program).and_then(Proc::get_data)
    }

    fn current_proc_mut<'p>(&self, program: &'p mut Program) -> Option<&'p mut Proc> {
        self.get_index().and_then(|i| program.procs.get_mut(i))
    }

    fn visit_proc<'p>(&mut self, program: &'p Program, index: usize) -> &'p Proc {
        self.enter_proc(index);
        &program.procs[index]
    }

    fn enter_proc(&mut self, i: usize) {
        self.set_index(Some(i));
    }

    fn exit_proc(&mut self) {
        self.set_index(None);
    }
}

pub enum Fmt {
    Loc(Loc),
    Typ(TokenType),
    Tok(IRToken),
}
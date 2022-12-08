use std::{collections::HashMap, str::FromStr};

use ashfire_types::{
    core::*,
    data::*,
    enums::IntrinsicType,
    lasso::Rodeo,
    proc::{Data, Proc},
};
use firelib::{lazy, lexer::Loc};

pub type OptionErr<T, E = Fmt> = ashlib::OptionErr<T, E>;
pub type LazyResult<T, E = Fmt> = lazy::LazyResult<T, E>;
pub type LazyError<E = Fmt> = lazy::LazyError<E>;

pub trait InternalString {
    fn as_str<'a>(&self, prog: &'a Program) -> &'a str;
    fn as_string(&self, prog: &Program) -> String;
}

impl InternalString for StrKey {
    fn as_str<'a>(&self, prog: &'a Program) -> &'a str {
        prog.interner.resolve(self)
    }

    fn as_string(&self, prog: &Program) -> String {
        prog.interner.resolve(self).to_owned()
    }
}

fn intern_all<const N: usize>(rodeo: &mut Rodeo, strings: [&'static str; N]) -> [StrKey; N] {
    strings
        .iter()
        .map(|s| rodeo.get_or_intern_static(s))
        .collect::<Vec<_>>()
        .try_into()
        .ok()
        .unwrap()
}

#[derive(Default)]
pub struct Program {
    pub ops: Vec<Op>,
    pub procs: Vec<Proc>,
    pub global_vars: Vec<TypeDescr>,
    pub structs_types: Vec<StructField>,
    pub block_contracts: HashMap<usize, (usize, usize)>,
    included_sources: Vec<StrKey>,
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

        let names = ["", "any", "bool", "int", "ptr", "str", "len", "data"];
        let [_, any, bool, int, ptr, str, len, data] = intern_all(&mut interner, names);

        let structs_types = vec![
            StructField::new(any, TypeId::ANY),
            StructField::new(bool, TypeId::BOOL),
            StructField::new(int, TypeId::INT),
            StructField::new(ptr, TypeId::PTR),
            StructField(str, vec![
                TypeDescr::unit(&len, TypeId::INT),
                TypeDescr::unit(&data, TypeId::PTR),
            ]),
        ];

        Self { structs_types, interner, ..Default::default() }
    }

    pub fn contains_source(&self, source: &str) -> bool {
        let Some(key) = self.interner.get(source) else {
            return false;
        };

        self.included_sources.contains(&key)
    }

    pub fn push_source(&mut self, source: &str) -> usize {
        let key = self.interner.get_or_intern(source);
        self.included_sources.push(key);
        self.included_sources.len() - 1
    }

    pub fn push_mem(&mut self, word: &StrKey, size: usize) {
        let value = OffsetWord::new(*word, self.mem_size as i32);
        self.memory.push(value);
        self.mem_size += size;
    }

    pub fn push_data(&mut self, mut word: String, size: usize) -> usize {
        if word.ends_with("\\0") {
            word.push('0');
        };

        let name = self.get_or_intern(&word);
        let value = OffsetData::new(name, size as i32, self.data_size as i32);
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

    pub fn get_or_intern(&mut self, word: &str) -> StrKey {
        self.interner.get_or_intern(word)
    }

    pub fn get_key(&self, word: &str) -> Option<StrKey> {
        // info!("get_key: ({word})");
        self.interner.get(word)
    }

    pub fn get_word<O: Operand>(&self, index: O) -> &str {
        self.interner.resolve(&index.str_key())
    }

    pub fn get_data<O: Operand>(&self, index: O) -> &OffsetData {
        &self.data[index.index()]
    }

    pub fn get_data_str<O: Operand>(&self, index: O) -> &str {
        self.get_data(index).as_str(self)
    }

    pub fn get_proc<O: Operand>(&self, index: O) -> &Proc {
        &self.procs[index.index()]
    }

    pub fn get_contract<O: Operand>(&self, index: O) -> (usize, usize) {
        self.block_contracts[&(index.index())]
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
                ValueType::Typ(value) => return self.data_name(value),
                ValueType::Ptr(value) => return self.data_name(value) + " Pointer",
            },
            TokenType::Str => "String Id",
        }
        .to_owned()
    }

    fn data_display<O: Operand>(value: TypeId, operand: O) -> String {
        let operand = operand.operand();
        match value {
            TypeId::BOOL => fold_bool!(operand != 0, "True", "False").to_owned(),
            TypeId::PTR => format!("*{operand}"),
            TypeId(_) => operand.to_string(),
        }
    }

    pub fn type_display(&self, tok: IRToken) -> String {
        match tok.get_type() {
            TokenType::Keyword => format!("{:?}", tok.as_keyword()),
            TokenType::Word => self.get_word(tok).to_owned(),
            TokenType::Str => self.get_data_str(tok).to_owned(),
            TokenType::Data(data) => Self::data_display(data.get_value(), tok),
        }
    }

    pub fn loc_fmt<L: Location>(&self, loc: L) -> String {
        let Loc { file_index, line, col } = loc.loc();
        self.included_sources
            .get(file_index)
            .map_or_else(String::new, |l| format!("{}:{line}:{col} ", l.as_str(self)))
    }

    pub fn format(&self, fmt: Fmt) -> String {
        match fmt {
            Fmt::Loc(loc) => self.loc_fmt(loc),
            Fmt::Tok(tok) => self.type_display(tok),
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

    fn get_cast_type_ptr(&mut self, rest: &[u8]) -> Option<usize> {
        let word = &std::str::from_utf8(rest).ok()?;
        // info!("get_cast_type_ptr: ({word})");

        let key = self.get_key(word)?;
        let val = self.get_struct_value_id(&key)?;
        Some(self.get_type_ptr(val, key).get_value().0)
    }

    fn get_cast_type(&self, rest: &[u8]) -> Option<usize> {
        let word = &std::str::from_utf8(rest).ok()?;
        // info!("get_cast_type: ({word})");
        self.get_key(word)
            .as_ref()
            .map(|key| self.get_struct_type_id(key))?
    }

    pub fn get_struct_type_id(&self, word: &StrKey) -> Option<usize> {
        self.structs_types
            .iter()
            .position(|def| word.eq(def.name()))
    }

    pub fn get_struct_value_id(&self, word: &StrKey) -> Option<TypeId> {
        self.structs_types
            .iter()
            .position(|id| word.eq(id.name()))
            .map(TypeId)
    }

    /// Searches for a `const` that matches the given [`StrKey`].
    pub fn get_const_by_name(&self, word: &StrKey) -> Option<&TypeDescr> {
        self.consts.iter().find(|cnst| word.eq(cnst.name()))
    }

    #[cfg(debug_assertions)]
    #[allow(dead_code)]
    fn op_debug(&self, op: &Op) -> String {
        use ashfire_types::enums::OpType;
        let &Op(op_type, operand, ..) = op;
        match op_type {
            OpType::Intrinsic => match IntrinsicType::from(operand.index()) {
                IntrinsicType::Cast(n) => todo!("{n}"),
                // {
                //     format!("Intrinsic Cast [{}]", self.type_name(ValueType::from(n).get_type()))
                // }
                intrinsic => format!("Intrinsic [{intrinsic:?}]"),
            },
            OpType::Call => format!("Call [{}]", self.get_proc(operand).name.as_str(self)),
            OpType::EndProc => format!("EndProc [{}]", self.get_proc(operand).name.as_str(self)),
            OpType::CallInline => {
                format!("Inline [{}]", self.get_proc(operand).name.as_str(self))
            }
            _ => format!("{op_type:?} [{operand}]"),
        }
    }
}

pub trait Visitor {
    fn set_index(&mut self, i: Option<usize>);
    fn get_index(&self) -> Option<usize>;

    fn inside_proc(&self) -> bool {
        self.get_index().is_some()
    }

    fn current_proc<'a>(&self, program: &'a Program) -> Option<&'a Proc> {
        self.get_index().and_then(|i| program.procs.get(i))
    }

    fn current_proc_data<'a>(&self, program: &'a Program) -> Option<&'a Data> {
        self.current_proc(program).and_then(Proc::get_data)
    }

    fn current_proc_mut<'a>(&self, program: &'a mut Program) -> Option<&'a mut Proc> {
        self.get_index().and_then(|i| program.procs.get_mut(i))
    }

    fn visit_proc<'a>(&mut self, program: &'a Program, index: usize) -> &'a Proc {
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

use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    str::from_utf8,
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
    fn as_str<'p>(&self, ctx: &'p Ctx) -> Ref<'p, str>;
    fn as_string(&self, ctx: &Ctx) -> String;
    fn name(&self) -> Name;
}

impl<T: InternalString, O> InternalString for (T, O) {
    fn as_str<'p>(&self, ctx: &'p Ctx) -> Ref<'p, str> {
        self.0.as_str(ctx)
    }

    fn as_string(&self, ctx: &Ctx) -> String {
        self.0.as_string(ctx)
    }

    fn name(&self) -> Name {
        self.0.name()
    }
}

impl InternalString for Name {
    fn as_str<'p>(&self, ctx: &'p Ctx) -> Ref<'p, str> {
        let interner = ctx.interner.borrow();
        Ref::map(interner, |a| a.resolve(self))
    }

    fn as_string(&self, ctx: &Ctx) -> String {
        ctx.interner.borrow().resolve(self).to_owned()
    }

    fn name(&self) -> Name {
        *self
    }
}

fn intern_all<const N: usize>(rodeo: &mut Rodeo, strings: [&'static str; N]) -> [Name; N] {
    strings
        .iter()
        .map(|s| rodeo.get_or_intern_static(s))
        .collect::<Vec<_>>()
        .try_into()
        .expect("Failed to collect into a valid array")
}

#[derive(Default)]
pub struct Ctx {
    ops: Vec<Op>,
    procs: Vec<Proc>,
    global_vars: Vec<TypeDescr>,
    block_contracts: HashMap<usize, (usize, usize)>,
    included_sources: HashMap<Name, Name>,
    consts: Vec<TypeDescr>,
    mem_size: u16,
    memory: Vec<OffsetWord>,
    data_size: u16,
    data: Vec<OffsetData>,
    types: RefCell<Vec<TypeDescr>>,
    interner: RefCell<Rodeo>,
}

impl Ctx {
    pub fn new() -> Self {
        let mut interner = Rodeo::new();

        let names = [
            "", "any", "*any", "bool", "int", "ptr", "str", "len", "data",
        ];
        let [_, any, any_ptr, bool, int, ptr, str, len, data] = intern_all(&mut interner, names);

        let types = RefCell::new(vec![
            TypeDescr::primitive(any, ANY),
            TypeDescr::reference(any_ptr, ANY_PTR, ANY),
            TypeDescr::primitive(bool, BOOL),
            TypeDescr::primitive(int, INT),
            TypeDescr::primitive(ptr, PTR),
            TypeDescr::structure(
                str,
                vec![
                    TypeDescr::primitive(len, INT),
                    TypeDescr::primitive(data, PTR),
                ],
                STR,
            ),
        ]);

        Self {
            types,
            interner: interner.into(),
            ..Default::default()
        }
    }

    pub fn ops(&self) -> &[Op] {
        &self.ops
    }

    pub fn ops_mut(&mut self) -> &mut Vec<Op> {
        &mut self.ops
    }

    pub fn procs(&self) -> &[Proc] {
        &self.procs
    }

    pub fn push_proc(&mut self, proc: Proc) -> usize {
        let len = self.procs.len();
        self.procs.push(proc);
        len
    }

    pub fn global_vars(&self) -> &[TypeDescr] {
        &self.global_vars
    }

    pub fn push_global_var(&mut self, var: TypeDescr) {
        self.global_vars.push(var)
    }

    pub fn block_contracts(&mut self) -> &mut HashMap<usize, (usize, usize)> {
        &mut self.block_contracts
    }

    pub fn has_source(&self, source: &str, module: &str) -> bool {
        let interner = &self.interner.borrow();
        if let (Some(src), Some(modl)) = (interner.get(source), interner.get(module)) {
            self.included_sources
                .get(&src)
                .map_or(false, |module_src| module_src == &modl)
        } else {
            false
        }
    }

    pub fn push_source(&mut self, source: &str, module: &str) -> usize {
        let mut interner = self.interner.borrow_mut();
        let mkey = interner.get_or_intern(module);
        let skey = interner.get_or_intern(source);

        //this should only insert a new key-value pair, should check if `has_source` first
        self.included_sources.insert(skey, mkey);
        skey.into_usize()
    }

    pub fn push_mem(&mut self, word: Name, size: u16) {
        let value = OffsetWord::new(word, self.mem_size);
        self.memory.push(value);
        self.mem_size += size;
    }

    pub fn push_data(&mut self, mut word: String, size: u16) -> DataKey {
        if word.ends_with("\\0") {
            word.push('0');
        };

        let name = self.get_or_intern(&word);
        let value = OffsetData::new(name, size, self.data_size);
        self.data.push(value);
        self.data_size += size;
        DataKey(self.data.len() - 1)
    }

    pub fn register_const(&mut self, struct_type: TypeDescr) {
        self.consts.push(struct_type);
    }

    pub fn data_size(&self) -> u16 {
        self.data_size
    }

    pub fn data_start(&self) -> u16 {
        word_aligned(self.mem_size)
    }

    pub fn global_vars_start(&self) -> u16 {
        self.data_start() + word_aligned(self.data_size)
    }

    pub fn global_vars_size(&self) -> u16 {
        self.global_vars.iter().fold(0, |acc, var| acc + var.size())
    }

    pub fn stack_start(&self) -> u16 {
        self.global_vars_start() + self.global_vars_size()
    }

    pub fn get_or_intern(&self, word: &str) -> Name {
        self.interner.borrow_mut().get_or_intern(word)
    }

    pub fn get_key(&self, word: &str) -> Option<Name> {
        self.interner.borrow().get(word)
    }

    pub fn get_word(&self, name: Name) -> String {
        self.interner.borrow().resolve(&name).to_owned()
    }

    pub fn get_data(&self, DataKey(index): DataKey) -> &OffsetData {
        &self.data[index]
    }

    pub fn get_data_str(&self, index: DataKey) -> Ref<'_, str> {
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

    fn data_name(&self, DataType(id): DataType) -> String {
        match id {
            TypeId::INT => "Integer",
            TypeId::BOOL => "Boolean",
            TypeId::PTR => "Pointer",
            TypeId::STR => "String",
            TypeId::ANY => "Any",
            _ => return self.types.borrow()[id].name().as_string(self),
        }
        .to_owned()
    }

    pub fn type_name(&self, tok: TokenType) -> String {
        match tok {
            TokenType::Keyword(_) => "Keyword",
            TokenType::Word(_) => "Word or Intrinsic",
            TokenType::Data(Value(data_type, _)) | TokenType::Type(data_type) => {
                return self.data_name(data_type);
            }
            TokenType::Str(_) => "String Id",
        }
        .to_owned()
    }

    fn data_display(value: TypeId, operand: i32) -> String {
        match value {
            TypeId::BOOL => (if operand == 0 { "False" } else { "True" }).to_owned(),
            TypeId::PTR => format!("*{operand}"),
            _ => operand.to_string(),
        }
    }

    pub fn token_display(&self, (tok, _): IRToken) -> String {
        match tok {
            TokenType::Keyword(key) => format!("{key:?}"),
            TokenType::Word(name) => self.get_word(name),
            TokenType::Str(index) => self.get_data_str(index).to_owned(),
            TokenType::Data(Value(DataType(id), value)) => Self::data_display(id, value),
            TokenType::Type(_) => todo!(),
        }
    }

    pub fn loc_fmt<L: Location>(&self, loc: L) -> String {
        let Loc { file_index, line, col } = loc.loc();
        self.included_sources
            .get_key_value(&name_from_usize(file_index))
            .map_or_else(String::new, |(src, modl)| {
                format!("{}/{}:{line}:{col} ", modl.as_str(self), src.as_str(self))
            })
    }

    pub fn format(&self, fmt: Fmt) -> String {
        match fmt {
            Fmt::Tok(tok) => self.token_display(tok),
            Fmt::Typ(typ) => self.type_name(typ),
            Fmt::Dat(typ) => self.data_name(typ),
            Fmt::Key(key) => key.as_string(self),
            Fmt::Loc(loc) => self.loc_fmt(loc),
        }
    }

    pub fn get_intrinsic_type(&self, word: &str) -> Option<IntrinsicType> {
        word.parse().ok().or_else(|| {
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

    fn get_cast_type(&self, rest: &[u8]) -> Option<DataType> {
        self.name_from_utf8(rest)
            .map(|key| self.get_data_type(key))?
    }

    fn get_cast_type_ptr(&self, rest: &[u8]) -> Option<DataType> {
        self.get_cast_type(rest).map(|id| self.get_type_ptr(id))
    }

    pub fn get_fields_data_type(&self, fields: &StructFields) -> DataType {
        self.get_data_type(fields.name())
            .expect("Fields was not constructed from a valid type id")
    }

    pub fn get_data_type(&self, word: Name) -> Option<DataType> {
        self.types
            .borrow()
            .iter()
            .position(|def| word.eq(&def.name()))
            .map(DataType::new)
    }

    pub fn get_data_type_by_str(&self, word: &str) -> Option<DataType> {
        self.get_key(word).and_then(|key| self.get_data_type(key))
    }

    pub fn get_type_descr(&self, DataType(id): DataType) -> Ref<'_, TypeDescr> {
        Ref::map(self.types.borrow(), |types| &types[id])
    }

    pub fn get_type_ptr(&self, data_type: DataType) -> DataType {
        let name = self.get_type_descr(data_type).name();
        let ptr_name = format!("*{}", name.as_str(self));

        if let Some(id) = self.get_data_type_by_str(&ptr_name) {
            return id;
        }

        let word_id = self.get_or_intern(&ptr_name);
        let mut types = self.types.borrow_mut();
        let new_type = DataType::new(types.len());
        types.push(TypeDescr::reference(word_id, new_type, data_type));
        new_type
    }

    pub fn register_type(&self, fields: StructFields) -> DataType {
        let mut types = self.types.borrow_mut();
        let data_type = DataType::new(types.len());
        types.push(TypeDescr::Structure(StructType(fields, data_type)));
        data_type
    }

    /// Searches for a `const` that matches the given [`Name`].
    pub fn get_const_by_name(&self, word: Name) -> Option<&TypeDescr> {
        self.consts.iter().find(|cnst| word.eq(&cnst.name()))
    }

    #[cfg(debug_assertions)]
    #[allow(dead_code)]
    fn op_debug(&self, &(op_type, _): &Op) -> String {
        use ashfire_types::enums::{ControlOp, IndexOp, OpType};
        match op_type {
            OpType::Intrinsic(IntrinsicType::Cast(data_type)) => {
                format!("Intrinsic Cast [{}]", self.data_name(data_type))
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

    fn current_proc<'p>(&self, ctx: &'p Ctx) -> Option<&'p Proc> {
        self.get_index().and_then(|i| ctx.procs.get(i))
    }

    fn current_proc_data<'p>(&self, ctx: &'p Ctx) -> Option<&'p Data> {
        self.current_proc(ctx).and_then(Proc::get_data)
    }

    fn current_proc_mut<'p>(&self, ctx: &'p mut Ctx) -> Option<&'p mut Proc> {
        self.get_index().and_then(|i| ctx.procs.get_mut(i))
    }

    fn visit_proc<'p>(&mut self, ctx: &'p Ctx, index: usize) -> &'p Proc {
        self.enter_proc(index);
        &ctx.procs[index]
    }

    fn enter_proc(&mut self, i: usize) {
        self.set_index(Some(i));
    }

    fn exit_proc(&mut self) {
        self.set_index(None);
    }
}

#[derive(Clone, Copy)]
pub enum Fmt {
    Typ(TokenType),
    Dat(DataType),
    Tok(IRToken),
    Key(Name),
    Loc(Loc),
}

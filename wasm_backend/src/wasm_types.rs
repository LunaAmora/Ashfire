#[derive(Clone, Copy)]
pub enum WasmType {
    MutI32,
    I32,
}

pub struct WasmValue {
    pub wasm_type: WasmType,
    pub value: i32,
}

pub struct FuncType {
    pub param: Vec<WasmType>,
    pub result: Vec<WasmType>,
}

pub struct Memory {}

pub struct Import {
    pub module: String,
    pub label: String,
    pub import: Bind,
}

pub struct Export {
    pub label: String,
    pub export: Bind,
}

pub enum Ident {
    Id(usize),
    Label(String),
}

impl From<&str> for Ident {
    fn from(label: &str) -> Self {
        Ident::Label(label.to_owned())
    }
}

pub enum Bind {
    Global(Ident),
    Func(Ident),
    Mem(Ident),
}

pub struct Global {
    pub value: WasmValue,
}

pub struct Data {
    pub position: usize,
    pub bytes: Vec<u8>,
}

pub struct Func {
    pub contract: Ident,
    pub code: Vec<Instruction>,
}

pub enum Scope {
    Local,
    Global,
}

pub enum Instruction {
    Block(BlockType, Option<Ident>),
    Get(Scope, Ident),
    Set(Scope, Ident),
    I32(NumMethod),
    Const(i32),
    Call(Ident),
    BrIf(Ident),
    Br(Ident),
    Drop,
    End,
}

pub enum BlockType {
    If,
    Loop(Option<Ident>),
    Block(Option<Ident>),
}

#[allow(non_camel_case_types)]
pub enum NumMethod {
    add,
    sub,
    store,
    load,
    or,
    and,
    eq,
    lt_s,
    ge_s,
    le_s,
    gt_s,
}

pub enum WasmIR {
    OpenParen,
    CloseParen,
    Keyword(Keyword),
    Type(WasmType),
    Local(Instruction),
}

#[allow(non_camel_case_types)]
pub enum Keyword {
    module,
    func,
    param,
    result,
    import,
    export,
    global,
    memory,
    data,
    r#type,
}

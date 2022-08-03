pub struct Module {
    pub types: Vec<FuncType>,
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
    pub globals: Vec<Global>,
    pub data: Vec<Data>,
    pub funcs: Vec<Func>,
}

pub enum WasmType {
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

pub struct Import {
    pub module: String,
    pub label: String,
    pub import: Bind,
}

pub struct Export {
    pub label: String,
    pub export: Bind,
}

pub enum Bind {
    Global(Id),
    Func(Id),
    Mem(Id),
}

pub enum Id {
    Value(usize),
    Label(String),
}

pub struct Global {
    pub wasm_type: WasmType,
    pub value: WasmValue,
    pub mutable: bool,
}

pub struct Data {
    pub position: usize,
    pub bytes: Vec<u8>,
}

pub struct Func {
    pub contract: Id,
    pub code: Vec<Instruction>,
}

pub enum Instruction {
    Block(BlockType, Option<Id>),
    Global(VarMethod),
    Local(VarMethod),
    Const(WasmValue),
    I32(NumMethod),
    Call(Id),
    BrIf(Id),
    Br(Id),
    Drop,
    End,
}

pub enum BlockType {
    If,
    Loop(Option<String>),
    Block(Option<String>),
}

#[allow(non_camel_case_types)]
pub enum VarMethod {
    get,
    set,
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
    r#mut,
}

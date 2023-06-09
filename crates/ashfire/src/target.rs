#[derive(Debug, Copy, Clone)]
pub enum Target {
    Wasi,
    Wasm4,
}

impl Target {
    pub fn module(&self) -> &str {
        match self {
            Self::Wasi => "wasi_snapshot_preview1",
            Self::Wasm4 => "env",
        }
    }

    pub fn imports_mem(&self) -> bool {
        match self {
            Self::Wasi => false,
            Self::Wasm4 => true,
        }
    }
}

use std::path::PathBuf;

use firelib::Result;

#[derive(Debug, Copy, Clone)]
pub enum Target {
    Wasi,
    Wasm4,
}

pub struct Runner(Box<dyn FnOnce(PathBuf) -> Result<()>>);

impl Runner {
    pub fn run(self, path: PathBuf) -> Result<()> {
        self.0(path)
    }
}

impl Target {
    pub fn module(&self) -> &str {
        match self {
            Self::Wasi => "wasi_unstable",
            Self::Wasm4 => "env",
        }
    }

    pub fn imports_mem(&self) -> bool {
        match self {
            Self::Wasi => false,
            Self::Wasm4 => true,
        }
    }

    pub fn runner(&self, wasi_runtime: String) -> Runner {
        match self {
            Self::Wasi => Runner(Box::new(move |out| {
                cmd_wait!(wasi_runtime, out);
                Ok(())
            })),

            Self::Wasm4 => Runner(Box::new(|out| {
                cmd_wait!("w4", "run", out);
                Ok(())
            })),
        }
    }
}

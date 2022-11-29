use std::path::PathBuf;

use clap::ValueEnum;
use firelib::Result;

#[derive(Debug, Copy, Clone, ValueEnum)]
pub enum Target {
    Wasi,
    Wasm4,
}

pub struct TargetConfig {
    pub module: String,
    pub imports_mem: bool,
    pub runner: Option<Box<dyn FnOnce(PathBuf) -> Result<()>>>,
}

impl TargetConfig {
    pub fn new(target: Target, wasi_runtime: String, run: bool) -> Self {
        match target {
            Target::Wasi => Self::create(
                "wasi_unstable",
                false,
                run.then_some(move |out| {
                    cmd_wait!(wasi_runtime, out);
                    Ok(())
                }),
            ),
            Target::Wasm4 => Self::create(
                "env",
                true,
                run.then_some(|out| {
                    cmd_wait!("w4", "run", out);
                    Ok(())
                }),
            ),
        }
    }

    pub fn create(
        module: &str, imports_mem: bool,
        runner: Option<impl FnOnce(PathBuf) -> Result<()> + 'static>,
    ) -> Self {
        Self {
            module: module.to_owned(),
            imports_mem,
            runner: runner.map(|some| Box::new(some) as _),
        }
    }

    pub fn run(self, path: PathBuf) -> Result<()> {
        self.runner.map_or(Ok(()), |runner| runner(path))
    }
}

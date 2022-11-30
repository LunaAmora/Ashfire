#![feature(test)]
extern crate test;

#[cfg(test)]
mod tests {
    use std::{env, io, path::PathBuf};

    use ashfire_lib::{
        compile,
        target::{Target, TargetConfig},
    };
    use test::Bencher;

    const WASI_RUNTIME: &str = "Wasmtime";

    fn bech_folder() -> io::Result<PathBuf> {
        Ok(env::current_dir()?.join("../firelang"))
    }

    #[bench]
    fn bench_wasi(b: &mut Bencher) -> io::Result<()> {
        let target = TargetConfig::new(Target::Wasi, WASI_RUNTIME.to_owned(), false);
        let path = bech_folder()?.join("test.fire");

        b.iter(|| compile(&path, io::sink(), &target));
        Ok(())
    }

    #[bench]
    fn bench_wasm4(b: &mut Bencher) -> io::Result<()> {
        let target = TargetConfig::new(Target::Wasm4, WASI_RUNTIME.to_owned(), false);
        let path = bech_folder()?.join("w4.fire");

        b.iter(|| compile(&path, io::sink(), &target));
        Ok(())
    }
}
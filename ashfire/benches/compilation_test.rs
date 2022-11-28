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

    struct Discard();

    impl io::Write for Discard {
        fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
            Ok(buf.len())
        }

        fn flush(&mut self) -> io::Result<()> {
            Ok(())
        }
    }

    fn bech_folder() -> io::Result<PathBuf> {
        Ok(env::current_dir()?.join("../firelang"))
    }

    #[bench]
    fn bench_wasi(b: &mut Bencher) -> io::Result<()> {
        let target = TargetConfig::new(Target::Wasi, WASI_RUNTIME.to_owned(), false);
        let path = PathBuf::from(bech_folder()?).join("test.fire");

        b.iter(|| compile(&path, Discard(), &target));
        Ok(())
    }

    #[bench]
    fn bench_wasm4(b: &mut Bencher) -> io::Result<()> {
        let target = TargetConfig::new(Target::Wasm4, WASI_RUNTIME.to_owned(), false);
        let path = PathBuf::from(bech_folder()?).join("w4.fire");

        b.iter(|| compile(&path, Discard(), &target));
        Ok(())
    }
}

#![feature(test)]
extern crate test;

#[cfg(test)]
mod tests {
    use std::{env, io, path::PathBuf};

    use ashfire::{compile, target::Target};
    use test::Bencher;

    fn bech_folder() -> io::Result<PathBuf> {
        Ok(env::current_dir()?.join("../firelang"))
    }

    #[bench]
    fn bench_wasi(b: &mut Bencher) -> io::Result<()> {
        let path = bech_folder()?.join("test.fire");

        b.iter(|| compile(&path, io::sink(), Target::Wasi));
        Ok(())
    }

    #[bench]
    fn bench_wasm4(b: &mut Bencher) -> io::Result<()> {
        let path = bech_folder()?.join("w4.fire");

        b.iter(|| compile(&path, io::sink(), Target::Wasm4));
        Ok(())
    }
}

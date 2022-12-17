#![feature(test)]
extern crate test;

#[cfg(test)]
mod tests {
    use std::{io, path::Path};

    use ashfire::{compile, target::Target, LANG_FOLDER};
    use test::Bencher;

    #[bench]
    fn bench_wasi(b: &mut Bencher) {
        let path = Path::new(LANG_FOLDER).join("test.fire");
        b.iter(|| compile(&path, io::sink(), Target::Wasi));
    }

    #[bench]
    fn bench_wasm4(b: &mut Bencher) {
        let path = Path::new(LANG_FOLDER).join("w4.fire");
        b.iter(|| compile(&path, io::sink(), Target::Wasm4));
    }
}

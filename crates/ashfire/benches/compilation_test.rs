use std::{io, path::Path};

use ashfire::{LANG_FOLDER, compile, target::Target};
use criterion::{Criterion, criterion_group, criterion_main};

fn bench_wasi(c: &mut Criterion) {
    let path = Path::new(LANG_FOLDER).join("test.fire");
    c.bench_function("wasi test", |b| b.iter(|| compile(&path, io::sink(), Target::Wasi)));
}

fn bench_wasm4(c: &mut Criterion) {
    let path = Path::new(LANG_FOLDER).join("w4.fire");
    c.bench_function("wasm4 test", |b| b.iter(|| compile(&path, io::sink(), Target::Wasm4)));
}

criterion_group!(benches, bench_wasi, bench_wasm4);
criterion_main!(benches);

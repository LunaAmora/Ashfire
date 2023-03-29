use env_logger::Builder;

pub fn formatted_builder() -> Builder {
    let mut builder = Builder::new();

    builder.format(|f, record| {
        use std::io::Write;
        let level = f.default_styled_level(record.level());
        writeln!(f, "[{level}] {}", record.args())
    });

    builder
}

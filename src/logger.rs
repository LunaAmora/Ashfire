use env_logger::Builder;

pub fn formatted_builder() -> Builder {
    let mut builder = Builder::new();

    builder.format(|f, record| {
        use std::io::Write;
        let level = record.level();
        let style = f.default_level_style(level);
        writeln!(f, "[{style}{level}{style:#}] {}", record.args())
    });

    builder
}

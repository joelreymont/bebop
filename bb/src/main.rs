use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};
use bebop_compiler::{error::*, ir::*};
use bebop_parser::{parse, DefsParserEx, Parser};
use bebop_util::meta::*;
use clap::Parser as ArgParser;
use std::fs;

#[derive(ArgParser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    file: String,
}

fn parse_and_lift(s: &str) -> Result<Architecture, Error> {
    let ast = parse(DefsParserEx::new(), s).map_err(Error::ParserError)?;
    let mut arch = Architecture::new();
    arch.lift(ast)?;
    Ok(arch)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut colors = ColorGenerator::new();

    // Generate & choose some colours for each of our elements
    let a = colors.next();
    let args = Args::parse();

    let source = Source::from(
        fs::read_to_string(args.file).expect("Failed to read file"),
    );
    match parse_and_lift(source.text()) {
        Ok(_) => {}
        Err(e) => {
            let span = e.span();
            Report::build(ReportKind::Error, span.start..span.end)
                .with_code(3)
                .with_message("Failed to compile".to_string())
                .with_label(
                    Label::new(span.start..span.end)
                        .with_message(format!("{e:?}"))
                        .with_color(a),
                )
                .finish()
                .print(&source)
                .unwrap();
        }
    }
    Ok(())
}

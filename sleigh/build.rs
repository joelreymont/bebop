use std::path::{Path, PathBuf};

fn main() {
    let checked_in_grammar_path =
        Path::new(&concat!(env!("CARGO_MANIFEST_DIR"), "/src/grammar.rs"));

    println!("path = {:?}", checked_in_grammar_path);

    let out_dir = PathBuf::from(std::env::var_os("OUT_DIR").expect("missing OUT_DIR variable"));
    let out_parser_dir = out_dir.join("parser");
    std::fs::create_dir_all(&out_parser_dir).expect("failed to create $OUT_DIR/parser");

    match std::fs::copy(checked_in_grammar_path, out_parser_dir.join("grammar.rs")) {
        Ok(_) => {
            eprintln!("Found a pre-generated LALRPOP grammar, copying it over");
        }
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            eprintln!("Generating a fresh LALRPOP grammar");
            lalrpop::Configuration::new()
                .use_cargo_dir_conventions()
                .process_file("src/grammar.lalrpop")
                .unwrap();
        }
        Err(e) => panic!("{e}"),
    }
    println!("cargo:rerun-if-changed=src/grammar.lalrpop");
}

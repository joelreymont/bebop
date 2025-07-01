use lalrpop_util::lalrpop_mod;

pub mod ast;
pub mod error;
pub mod lexer;
pub mod meta;

lalrpop_mod!(
    #[allow(clippy::all)]
    #[allow(unused_parens)]
    #[allow(unused_imports)]
    pub grammar, "/grammar.rs");

use bebop_util::id::*;
use lalrpop_util::lalrpop_mod;

pub mod ast;
pub mod error;
pub mod lexer;

lalrpop_mod!(
    #[allow(clippy::all)]
    #[allow(unused_parens)]
    #[allow(unused_imports)]
    pub grammar, "/parser/grammar.rs");

pub struct ParserEx<T, U> {
    parser: T,
    marker: std::marker::PhantomData<U>,
}

pub trait Parser {
    type T;
    type U;

    fn new() -> Self;

    fn parse<'input>(
        &self,
        lexer: lexer::Lexer<'input>,
    ) -> Result<Self::U, error::ParserError>;
}

pub fn parse<T>(
    parser: impl Parser<U = T>,
    s: &str,
) -> Result<T, error::ParserError> {
    let lexer = lexer::Lexer::new(s);
    parser.parse(lexer)
}

pub type DefsParserEx =
    ParserEx<grammar::DefsParser, Vec<ast::Definition>>;

impl Parser for DefsParserEx {
    type T = grammar::DefsParser;
    type U = Vec<ast::Definition>;

    fn new() -> Self {
        ParserEx {
            parser: Self::T::new(),
            marker: std::marker::PhantomData,
        }
    }

    fn parse(
        &self,
        lexer: lexer::Lexer,
    ) -> Result<Self::U, error::ParserError> {
        Ok(self.parser.parse(lexer)?)
    }
}

pub type CtrStartParserEx =
    ParserEx<grammar::ConstructorStartParser, (LocId, ast::Display, bool)>;

impl Parser for CtrStartParserEx {
    type T = grammar::ConstructorStartParser;
    type U = (LocId, ast::Display, bool);

    fn new() -> Self {
        ParserEx {
            parser: Self::T::new(),
            marker: std::marker::PhantomData,
        }
    }

    fn parse(
        &self,
        lexer: lexer::Lexer,
    ) -> Result<Self::U, error::ParserError> {
        Ok(self.parser.parse(lexer)?)
    }
}

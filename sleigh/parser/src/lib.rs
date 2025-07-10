use bebop_sleigh_util::meta::*;
use lalrpop_util::lalrpop_mod;

pub mod ast;
pub mod error;
pub mod lexer;

lalrpop_mod!(
    #[allow(clippy::all)]
    #[allow(unused_parens)]
    #[allow(unused_imports)]
    pub grammar, "/grammar.rs");

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
        file_id: FileId,
        lexer: lexer::Lexer<'input>,
    ) -> Result<Self::U, error::ParserError>;
}

pub fn parse<T>(parser: impl Parser<U = T>, s: &str) -> Result<T, error::ParserError> {
    let lexer = lexer::Lexer::new(s);
    parser.parse(FileId::empty(), lexer)
}

pub type DefsParserEx = ParserEx<grammar::DefsParser, Vec<ast::Definition>>;

impl Parser for DefsParserEx {
    type T = grammar::DefsParser;
    type U = Vec<ast::Definition>;

    fn new() -> Self {
        ParserEx {
            parser: Self::T::new(),
            marker: std::marker::PhantomData,
        }
    }

    fn parse(&self, file_id: FileId, lexer: lexer::Lexer) -> Result<Self::U, error::ParserError> {
        Ok(self.parser.parse(file_id, lexer)?)
    }
}

pub type CtrStartParserEx =
    ParserEx<grammar::ConstructorStartParser, (Loc<ast::Ident>, ast::Display, bool)>;

impl Parser for CtrStartParserEx {
    type T = grammar::ConstructorStartParser;
    type U = (Loc<ast::Ident>, ast::Display, bool);

    fn new() -> Self {
        ParserEx {
            parser: Self::T::new(),
            marker: std::marker::PhantomData,
        }
    }

    fn parse(&self, file_id: FileId, lexer: lexer::Lexer) -> Result<Self::U, error::ParserError> {
        Ok(self.parser.parse(file_id, lexer)?)
    }
}

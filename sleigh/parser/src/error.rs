use crate::lexer::*;
use lalrpop_util::ParseError as LalrpopError;
use std::ops::Range;
use thiserror::Error;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LexerError {
    Generic(usize, usize),
}

#[derive(Debug, PartialEq, Eq, Clone, Error)]
pub enum ParserError {
    #[error("Lexer error")]
    Lexer(LexerError),

    #[error("Invalid token")]
    InvalidToken { span: Range<usize> },

    #[error("Unrecognized end-of-file")]
    UnrecognizedEOF { span: Range<usize> },

    #[error("Unrecognized token")]
    UnrecognizedToken {
        token: String,
        expected: Vec<String>,
        span: Range<usize>,
    },

    #[error("Extra token")]
    ExtraToken { span: Range<usize> },
}

pub type LalrParseError<'input> = LalrpopError<usize, Token<'input>, ParserError>;

impl From<LalrParseError<'_>> for ParserError {
    fn from(value: LalrParseError<'_>) -> Self {
        use LalrpopError::*;

        match value {
            InvalidToken { location } => Self::InvalidToken { span: 0..location },
            UnrecognizedEof { location, .. } => Self::UnrecognizedEOF { span: 0..location },
            UnrecognizedToken {
                token: (start, token, end),
                expected,
            } => Self::UnrecognizedToken {
                token: format!("{:?}", token),
                expected,
                span: start..end,
            },
            ExtraToken {
                token: (start, _, end),
            } => Self::ExtraToken { span: start..end },
            User { error } => error,
            // User { error } => match error {
            //     Lexical::Generic(_, _) => ParseError::Lexical(error),
            // },
        }
    }
}

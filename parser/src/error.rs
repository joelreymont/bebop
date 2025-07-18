use crate::lexer::*;
use bebop_util::meta::*;
use lalrpop_util::ParseError as LalrpopError;
use thiserror::Error;

#[derive(Debug, Clone)]
pub enum LexerError {
    Generic(Span),
}

impl Spanned for LexerError {
    fn span(&self) -> Span {
        match self {
            Self::Generic(span) => *span,
        }
    }
}

#[derive(Debug, Clone, Error)]
pub enum ParserError {
    #[error("Lexer error")]
    Lexer(LexerError),

    #[error("Invalid token")]
    InvalidToken(Span),

    #[error("Unrecognized end-of-file")]
    UnrecognizedEOF(Span),

    #[error("Unrecognized token")]
    UnrecognizedToken {
        token: String,
        expected: Vec<String>,
        span: Span,
    },

    #[error("Extra token")]
    ExtraToken(Span),
}

impl Spanned for ParserError {
    fn span(&self) -> Span {
        match self {
            Self::Lexer(e) => e.span(),
            Self::InvalidToken(span) => *span,
            Self::UnrecognizedEOF(span) => *span,
            Self::UnrecognizedToken { span, .. } => *span,
            Self::ExtraToken(span) => *span,
        }
    }
}

pub type LalrParseError<'input> =
    LalrpopError<usize, Token<'input>, ParserError>;

impl From<LalrParseError<'_>> for ParserError {
    fn from(value: LalrParseError<'_>) -> Self {
        use LalrpopError::*;
        match value {
            InvalidToken { location } => {
                Self::InvalidToken((0..location).into())
            }
            UnrecognizedEof { location, .. } => {
                Self::UnrecognizedEOF((0..location).into())
            }
            UnrecognizedToken {
                token: (start, token, end),
                expected,
            } => Self::UnrecognizedToken {
                token: format!("{token:?}"),
                expected,
                span: (start..end).into(),
            },
            ExtraToken {
                token: (start, _, end),
            } => Self::ExtraToken((start..end).into()),
            User { error } => error,
            // User { error } => match error {
            //     Lexical::Generic(_, _) => ParseError::Lexical(error),
            // },
        }
    }
}

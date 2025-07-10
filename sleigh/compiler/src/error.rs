use bebop_sleigh_parser::error::*;
use bebop_sleigh_util::meta::*;
use thiserror::Error;

#[derive(Debug, PartialEq, Eq, Clone, Error)]
pub enum LiftError {
    #[error("Unknown")]
    Unknown { span: Span },

    #[error("Invalid")]
    Invalid { span: Span },

    #[error("Duplicate ")]
    Duplicate { span: Span },

    #[error("Type mismatch")]
    TypeMismatch { span: Span },

    #[error("Internal error (type mismatch)")]
    InternalTypeMismatch,

    #[error("Parser error")]
    ParserError(ParserError),
}

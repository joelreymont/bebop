// use codespan_reporting::diagnostic::Label;
use std::ops::Range;

#[derive(Debug, PartialEq, Clone)]
pub enum LexicalError {
    Generic(Range<usize>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ParseError {
    Lexical(LexicalError),
}

use super::error::{LexicalError, ParseError};
use logos::Logos;
use serde::Serialize;
use std::num::ParseIntError;
use std::ops::Range;

pub fn parse_number(base: u32, slice: &str) -> Result<usize, ParseIntError> {
    usize::from_str_radix(slice, base)
}

#[derive(Serialize, Debug, PartialEq, Clone, Logos)]
pub enum NormalToken<'input> {
    // Forbid lone carriage returns
    #[regex("\r[^\n]")]
    Error,
    #[regex("((\r\n)+|[ \t\n]+)", logos::skip)]
    Ignored,
    #[regex("[0-9]+", |lex| parse_number(10, lex.slice()).ok())]
    DecInt(usize),
    #[regex("0[bB][01]+", |lex| parse_number(2, &lex.slice()[2..]).ok())]
    BinInt(usize),
    #[regex("0[xX][a-fA-F0-9]+", |lex| parse_number(16, &lex.slice()[2..]).ok())]
    HexInt(usize),
    #[regex("[a-zA-Z_\\.]([a-zA-Z_\\.]|[0-9])*")]
    Ident(&'input str),
    #[token("is", ignore(case))]
    Is,
    #[token("if", ignore(case))]
    If,
    #[token("alignment", ignore(case))]
    Alignment,
    #[token("attach", ignore(case))]
    Attach,
    #[token("big", ignore(case))]
    Big,
    #[token("default", ignore(case))]
    Default,
    #[token("little", ignore(case))]
    Little,
    #[token("build", ignore(case))]
    Build,
    #[token("call", ignore(case))]
    Call,
    #[token("dec", ignore(case))]
    Dec,
    #[token("define", ignore(case))]
    Define,
    #[token("endian", ignore(case))]
    Endian,
    #[token("export", ignore(case))]
    Export,
    #[token("goto", ignore(case))]
    Goto,
    #[token("hex", ignore(case))]
    Hex,
    #[token("local", ignore(case))]
    Local,
    #[token("macro", ignore(case))]
    Macro,
    #[token("offset", ignore(case))]
    Offset,
    #[token("pcodeop", ignore(case))]
    PCodeOp,
    #[token("return", ignore(case))]
    Return,
    #[token("register", ignore(case))]
    Register,
    #[token("ram_space", ignore(case))]
    RamSpace,
    #[token("rom_space", ignore(case))]
    RomSpace,
    #[token("register_space", ignore(case))]
    RegisterSpace,
    #[token("signed", ignore(case))]
    Signed,
    #[token("size", ignore(case))]
    Size,
    #[token("space", ignore(case))]
    Space,
    #[token("token", ignore(case))]
    Token,
    #[token("type", ignore(case))]
    Type,
    #[token("unimpl", ignore(case))]
    Unimpl,
    #[token("wordsize", ignore(case))]
    WordSize,
    #[token("...")]
    Ellipsis,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("!")]
    Bang,
    #[token("~")]
    Tilde,
    #[token(";")]
    Semi,
    #[token("=")]
    Assign,
    #[token("<")]
    LT,
    #[token(">")]
    GT,
    #[token("==")]
    EQ,
    #[token("!=")]
    NE,
    #[token("<=")]
    LE,
    #[token(">=")]
    GE,
    #[token("||")]
    Or,
    #[token("&&")]
    And,
    #[token("^^")]
    Xor,
    #[token("|")]
    Pipe,
    #[token("&")]
    Ampersand,
    #[token("^")]
    Caret,
    #[token("<<")]
    LShift,
    #[token(">>")]
    RShift,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("$or")]
    SpecOr,
    #[token("$and")]
    SpecAnd,
    #[token("$xor")]
    SpecXor,
    #[token("f<")]
    FLT,
    #[token("f>")]
    FGT,
    #[token("f==")]
    FEQ,
    #[token("f!=")]
    FNE,
    #[token("f<=")]
    FLE,
    #[token("f>=")]
    FGE,
    #[token("f+")]
    FPlus,
    #[token("f-")]
    FMinus,
    #[token("f*")]
    FMul,
    #[token("f/")]
    FDiv,
    #[token("s<")]
    SLT,
    #[token("s>")]
    SGT,
    #[token("s<=")]
    SLE,
    #[token("s>=")]
    SGE,
    #[token("s<<")]
    SLShift,
    #[token("s>>")]
    SRShift,
    #[token("s/")]
    SDiv,
    #[token("s%")]
    SMod,
    #[regex("#[^\n]*")]
    Comment,
}

#[derive(Serialize, Logos, Debug, PartialEq, Clone)]
pub enum DisplayToken<'input> {
    // Forbid lone carriage returns
    #[regex("\r[^\n]")]
    Error,
    #[regex("[a-zA-Z_\\.]([a-zA-Z_\\.]|[0-9])*")]
    Ident(&'input str),
    #[regex("[^ a-zA-Z_\\.\\^][^ \\^]*")]
    Text(&'input str),
    #[token("^")]
    Caret,
    #[token(" ")]
    Whitespace,
    #[regex("((\r\n)+|[\t\n]+)", logos::skip, priority = 10)]
    Ignored,
}

#[derive(Serialize, Debug, PartialEq, Clone)]
pub enum Token<'input> {
    Normal(NormalToken<'input>),
    Display(DisplayToken<'input>),
}

pub type SpannedToken<'input> = (usize, Token<'input>, usize);

type NormalLexer<'input> = logos::Lexer<'input, NormalToken<'input>>;
type DisplayLexer<'input> = logos::Lexer<'input, DisplayToken<'input>>;

pub enum ModalLexer<'input> {
    Normal(NormalLexer<'input>),
    Display(DisplayLexer<'input>),
}

// Wrap the `next()` function of the underlying lexer.
impl<'input> Iterator for ModalLexer<'input> {
    type Item = Result<Token<'input>, ()>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            ModalLexer::Normal(lexer) => Some(lexer.next()?.map(Token::Normal)),
            ModalLexer::Display(lexer) => Some(lexer.next()?.map(Token::Display)),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Mode {
    Normal,
    Display,
}

pub struct Lexer<'input> {
    pub lexer: Option<ModalLexer<'input>>,
    pub mode: Mode,
}

impl<'input> Lexer<'input> {
    pub fn new(s: &'input str) -> Self {
        Lexer {
            lexer: Some(ModalLexer::Normal(NormalToken::lexer(s))),
            mode: Mode::Normal,
        }
    }

    pub fn switch_to_normal(&mut self) {
        match self.lexer.take() {
            Some(ModalLexer::Display(lexer)) => {
                self.lexer = Some(ModalLexer::Normal(lexer.morph()));
            }
            _ => panic!("lexer::switch_to_normal"),
        }
    }

    pub fn switch_to_display(&mut self) {
        match self.lexer.take() {
            Some(ModalLexer::Normal(lexer)) => {
                self.lexer = Some(ModalLexer::Display(lexer.morph()));
            }
            _ => panic!("lexer::switch_to_display"),
        }
    }

    fn handle_normal_token(
        &mut self,
        span: Range<usize>,
        token: NormalToken<'input>,
    ) -> Option<Result<SpannedToken<'input>, ParseError>> {
        match token {
            NormalToken::Comment => return self.next(),
            NormalToken::Error => {
                return Some(Err(ParseError::Lexical(LexicalError::Generic(
                    span.start, span.end,
                ))))
            }
            _ => (),
        };

        Some(Ok((span.start, Token::Normal(token), span.end)))
    }

    fn handle_display_token(
        &mut self,
        span: Range<usize>,
        token: DisplayToken<'input>,
    ) -> Option<Result<SpannedToken<'input>, ParseError>> {
        match token {
            DisplayToken::Error => {
                return Some(Err(ParseError::Lexical(LexicalError::Generic(
                    span.start, span.end,
                ))))
            }
            _ => (),
        };

        Some(Ok((span.start, Token::Display(token), span.end)))
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<SpannedToken<'input>, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lexer.as_mut().unwrap() {
            ModalLexer::Normal(lexer) => {
                let token = lexer.next()?.unwrap_or(NormalToken::Error);
                let span = lexer.span();
                self.handle_normal_token(span, token)
            }
            ModalLexer::Display(lexer) => {
                let token = lexer.next()?.unwrap_or(DisplayToken::Error);
                let span = lexer.span();
                self.handle_display_token(span, token)
            }
        }
    }
}

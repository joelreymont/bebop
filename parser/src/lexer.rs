use logos::Logos;
use serde::Serialize;
use std::num::ParseIntError;
use std::ops::Range;

use super::error::{LexerError, ParserError};

pub fn parse_number(
    base: u32,
    slice: &str,
) -> Result<usize, ParseIntError> {
    usize::from_str_radix(slice, base)
}

#[derive(Serialize, Debug, PartialEq, Copy, Clone, Logos)]
pub enum NormalToken<'input> {
    // Forbid lone carriage returns
    #[regex("\r[^\n]")]
    Error,
    #[regex("[ \t]+", logos::skip)]
    Ignored,
    #[regex("(\r\n)+|(\n+)")]
    NewLine,
    #[regex("[0-9]+", |lex| parse_number(10, lex.slice()).ok())]
    DecInt(usize),
    #[regex("0[bB][01]+", |lex| parse_number(2, &lex.slice()[2..]).ok())]
    BinInt(usize),
    #[regex("0[xX][a-fA-F0-9]+", |lex| parse_number(16, &lex.slice()[2..]).ok())]
    HexInt(usize),
    #[regex("[a-zA-Z_\\.]([a-zA-Z_\\.]|[0-9])*")]
    Ident(&'input str),
    #[token("if", ignore(case))]
    If,
    #[token("alignment", ignore(case))]
    Alignment,
    #[token("attach", ignore(case))]
    Attach,
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
    #[token("pcodeop", ignore(case))]
    PCodeOp,
    #[token("return", ignore(case))]
    Return,
    #[token("register", ignore(case))]
    Register,
    #[token("signed", ignore(case))]
    Signed,
    #[token("space", ignore(case))]
    Space,
    #[token("token", ignore(case))]
    Token,
    #[token("type", ignore(case))]
    Type,
    #[token("unimpl", ignore(case))]
    Unimpl,
    #[token("variables", ignore(case))]
    Variables,
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

#[derive(Serialize, Logos, Debug, PartialEq, Copy, Clone)]
pub enum DisplayToken<'input> {
    // Forbid lone carriage returns
    #[regex("\r[^\n]")]
    Error,
    #[regex("[a-zA-Z_\\.]([a-zA-Z_\\.]|[0-9])*")]
    Ident(&'input str),
    #[regex("[^ \ta-zA-Z_\\.\\^][^ \t\\^]*")]
    Text(&'input str),
    #[regex("[iI][sS]", priority = 100)]
    Is,
    #[token("^")]
    Caret,
    #[regex("[ \t]+")]
    Whitespace,
    #[regex("((\r\n)+|[\t\n]+)", logos::skip, priority = 10)]
    Ignored,
}

#[derive(Serialize, Debug, PartialEq, Copy, Clone)]
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

impl<'input> Iterator for ModalLexer<'input> {
    type Item = Result<Token<'input>, ()>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            ModalLexer::Normal(lexer) => {
                Some(lexer.next()?.map(Token::Normal))
            }
            ModalLexer::Display(lexer) => {
                Some(lexer.next()?.map(Token::Display))
            }
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum State {
    Normal,
    ForcedNormal,
    NewLine,
    MaybeDisplay,
    Display,
}

pub struct Lexer<'input> {
    pub lexer: Option<ModalLexer<'input>>,
    pub state: State,
}

impl<'input> Lexer<'input> {
    pub fn new(s: &'input str) -> Self {
        Lexer {
            lexer: Some(ModalLexer::Normal(NormalToken::lexer(s))),
            state: State::NewLine,
        }
    }

    pub fn switch_to_normal(&mut self) {
        match self.lexer.take() {
            Some(ModalLexer::Display(lexer)) => {
                self.lexer = Some(ModalLexer::Normal(lexer.morph()));
                self.state = State::Normal;
            }
            _ => panic!("lexer::switch_to_normal"),
        }
    }

    pub fn switch_to_display(&mut self) {
        match self.lexer.take() {
            Some(ModalLexer::Normal(lexer)) => {
                self.lexer = Some(ModalLexer::Display(lexer.morph()));
                self.state = State::Display;
            }
            _ => panic!("lexer::switch_to_display"),
        }
    }

    fn handle_normal_token(
        &mut self,
        span: Range<usize>,
        token: NormalToken<'input>,
    ) -> Option<Result<SpannedToken<'input>, ParserError>> {
        match token {
            NormalToken::Comment | NormalToken::NewLine => {
                return self.next();
            }
            NormalToken::Error => {
                return Some(Err(ParserError::Lexer(
                    LexerError::Generic(span),
                )));
            }
            _ => (),
        };

        Some(Ok((span.start, Token::Normal(token), span.end)))
    }

    fn handle_display_token(
        &mut self,
        span: Range<usize>,
        token: DisplayToken<'input>,
    ) -> Option<Result<SpannedToken<'input>, ParserError>> {
        if let DisplayToken::Error = token {
            return Some(Err(ParserError::Lexer(LexerError::Generic(
                span,
            ))));
        }
        Some(Ok((span.start, Token::Display(token), span.end)))
    }

    fn update_state(&mut self, token: Token<'input>) {
        use {DisplayToken as DT, NormalToken as NT, Token as T};
        match (self.state, token) {
            (
                State::Normal | State::NewLine | State::MaybeDisplay,
                T::Normal(NT::LBracket) | T::Normal(NT::LBrace),
            ) => self.state = State::ForcedNormal,
            (
                State::ForcedNormal,
                T::Normal(NT::RBracket) | T::Normal(NT::RBrace),
            ) => self.state = State::Normal,
            (State::Normal, T::Normal(NT::NewLine)) => {
                self.state = State::NewLine
            }
            (State::NewLine, T::Normal(NT::Ident(_))) => {
                self.state = State::MaybeDisplay
            }
            (State::NewLine, T::Normal(NT::Colon))
            | (State::MaybeDisplay, T::Normal(NT::Colon)) => {
                self.state = State::Display;
                self.switch_to_display();
            }
            (State::Display, T::Display(DT::Is)) => {
                self.state = State::Normal;
                self.switch_to_normal();
            }
            (State::NewLine, T::Normal(NT::NewLine)) => (),
            (State::NewLine, _) | (State::MaybeDisplay, _) => {
                self.state = State::Normal
            }
            _ => (),
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<SpannedToken<'input>, ParserError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lexer.as_mut().unwrap() {
            ModalLexer::Normal(lexer) => {
                let token = lexer.next()?.unwrap_or(NormalToken::Error);
                let span = lexer.span();
                self.update_state(Token::Normal(token));
                self.handle_normal_token(span, token)
            }
            ModalLexer::Display(lexer) => {
                let token = lexer.next()?.unwrap_or(DisplayToken::Error);
                let span = lexer.span();
                self.update_state(Token::Display(token));
                self.handle_display_token(span, token)
            }
        }
    }
}

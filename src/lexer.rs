use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::{prelude::*, stream::Stream};
use std::{collections::HashMap, env, fmt, fs};

pub type Span = std::ops::Range<usize>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Bool(bool),
    Num(String),
    Str(String),
    Op(String),
    Ctrl(char),
    Ident(String),
    Kind(String),
    Check,
    If,
    Else,
    And,
    Or,
    Eq,
    Xor,
    Pipe,
    End,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Bool(x) => write!(f, "{}", x),
            Token::Num(n) => write!(f, "{}", n),
            Token::Str(s) => write!(f, "{}", s),
            Token::Kind(s) => write!(f, "{}", s),
            Token::Op(s) => write!(f, "{}", s),
            Token::Ctrl(c) => write!(f, "{}", c),
            Token::Ident(s) => write!(f, "{}", s),
            Token::Check => write!(f, "check"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::And => write!(f, "and"),
            Token::Or => write!(f, "or"),
            Token::Eq => write!(f, "eq"),
            Token::Xor => write!(f, "xor"),
            Token::Pipe => write!(f, "|"),
            Token::End => write!(f, "end"),
        }
    }
}

pub fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    let num = text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
        .collect::<String>()
        .map(Token::Num);

    let str_ = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::Str);

    let op = one_of("+-*/=".chars())
        .repeated()
        .at_least(1)
        .collect::<String>()
        .map(Token::Op);

    let ctrl = one_of("()[]{}:".chars()).map(|c| Token::Ctrl(c));

    let pipe = just('|').to(Token::Pipe);

    let ident = text::ident()
        .collect::<String>()
        .map(|ident| match ident.as_str() {
            "and" => Token::And,
            "check" => Token::Check,
            "else" => Token::Else,
            "end" => Token::End,
            "eq" => Token::Eq,
            "false" => Token::Bool(false),
            "if" => Token::If,
            "or" => Token::Or,
            "true" => Token::Bool(true),
            "xor" => Token::Xor,
            _ => Token::Ident(ident),
        });

    let kind = kind()
        .collect::<String>()
        .map(|kind| Token::Kind(kind) );

    let token = num
        .or(str_)
        .or(op)
        .or(ctrl)
        .or(pipe)
        .or(kind)
        .or(ident)
        .recover_with(skip_then_retry_until([]));

    token
        .map_with_span(|tok, span| (tok, span))
        .padded()
        .repeated()
}

pub fn kind() -> impl Parser<char, Vec<char>, Error = Simple<char>> + Copy + Clone {
    filter(|c: &char| c.is_uppercase())
        .map(Some)
        .chain(filter(|c: &char| c.is_ascii_alphanumeric() || *c == '_').repeated())
}

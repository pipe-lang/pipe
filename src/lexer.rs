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

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Num(String),
    Str(String),
    List(Vec<Value>),
    Func(String),
}

// impl Value {
//     fn num(self) -> f64 {
//         if let Value::Num(x) = self {
//             x
//         } else {
//             panic!("Value is not a number!");
//         }
//     }
// }

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Null => write!(f, "null"),
            Self::Bool(x) => write!(f, "{}", x),
            Self::Num(x) => write!(f, "{}", x),
            Self::Str(x) => write!(f, "{}", x),
            Self::List(xs) => write!(
                f,
                "[{}]",
                xs.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Func(name) => write!(f, "<function: {}>", name),
        }
    }
}

#[derive(Clone, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
    And,
    Or,
    Xor,
}

type Params = Vec<(String, String)>;

#[derive(Debug)]
pub enum Expr {
    Error,
    Value(Value),
    List(Vec<Self>),
    Local(String),
    Assignation(Vec<String>, Box<Self>),
    Then(Box<Self>, Box<Self>),
    Binary(Box<Self>, BinaryOp, Box<Self>),
    Pipe(Box<Self>, Box<Self>),
    Call(Box<Self>, Vec<Self>),
    If(Box<Self>, Vec<Self>, Vec<Self>),
    Function(String, Params, Box<Expr>),
}

pub fn expr_parser() -> impl Parser<Token, Expr, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let raw_expression = recursive(|raw_expression| {
            let val = filter_map(|span, tok| match tok {
                Token::Bool(x) => Ok(Expr::Value(Value::Bool(x))),
                Token::Num(n) => Ok(Expr::Value(Value::Num(n))),
                Token::Str(s) => Ok(Expr::Value(Value::Str(s))),
                _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
            })
            .labelled("value");

            let ident = filter_map(|span, tok| match tok {
                Token::Ident(ident) => Ok(ident.clone()),
                _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
            })
            .labelled("identifier");

            let items = expr.clone().repeated();

            let assignees = ident.repeated();
            let assignation = assignees
                .then_ignore(just(Token::Op("=".to_string())))
                .then(raw_expression)
                .map(|(variables, value)| Expr::Assignation(variables, Box::new(value)));

            let list = items
                .clone()
                .delimited_by(Token::Ctrl('['), Token::Ctrl(']'))
                .map(Expr::List);

            let atom = expr
                .clone()
                .delimited_by(Token::Ctrl('('), Token::Ctrl(')'))
                .or(val.clone())
                .or(assignation)
                .or(ident.map(Expr::Local))
                .or(list)
                .or(expr
                    .clone()
                    .delimited_by(Token::Ctrl('('), Token::Ctrl(')')))
                .recover_with(nested_delimiters(
                    Token::Ctrl('('),
                    Token::Ctrl(')'),
                    [
                        (Token::Ctrl('{'), Token::Ctrl('}')),
                        (Token::Ctrl('['), Token::Ctrl(']')),
                    ],
                    || Expr::Error,
                ));

            let call = atom
                .then(
                    items
                        .delimited_by(Token::Ctrl('('), Token::Ctrl(')'))
                        .repeated(),
                )
                .foldl(|f, args| Expr::Call(Box::new(f), args));

            let op = just(Token::Op("*".to_string()))
                .to(BinaryOp::Mul)
                .or(just(Token::Op("/".to_string())).to(BinaryOp::Div));
            let product = call
                .clone()
                .then(op.then(call).repeated())
                .foldl(|a, (op, b)| Expr::Binary(Box::new(a), op, Box::new(b)));

            let op = just(Token::Op("+".to_string()))
                .to(BinaryOp::Add)
                .or(just(Token::Op("-".to_string())).to(BinaryOp::Sub));
            let sum = product
                .clone()
                .then(op.then(product).repeated())
                .foldl(|a, (op, b)| Expr::Binary(Box::new(a), op, Box::new(b)));

            let op = just(Token::And)
                .to(BinaryOp::And)
                .or(just(Token::Or).to(BinaryOp::Or))
                .or(just(Token::Xor).to(BinaryOp::Xor))
                .or(just(Token::Eq).to(BinaryOp::Eq))
                .or(just(Token::Op("!=".to_string())).to(BinaryOp::NotEq));

            let compare = sum
                .clone()
                .then(op.then(sum).repeated())
                .foldl(|a, (op, b)| Expr::Binary(Box::new(a), op, Box::new(b)));

            let pipe = compare
                .clone()
                .then(just(Token::Pipe).ignore_then(compare).repeated())
                .foldl(|left, right| Expr::Pipe(Box::new(left), Box::new(right)));

            pipe
        });

        let block = expr.clone().repeated();

        let if_ = just(Token::If)
            .ignore_then(raw_expression.clone())
            .then(block.clone())
            .then(just(Token::Else).ignore_then(block.clone()).or_not())
            .then_ignore(just(Token::End))
            .map(|((conditional, a), b)| {
                Expr::If(
                    Box::new(conditional),
                    a,
                    match b {
                        Some(b) => b,
                        None => vec![],
                    },
                )
            });

        let ident = filter_map(|span, tok| match tok {
            Token::Ident(ident) => Ok(ident.clone()),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
        });

        let kind = filter_map(|span, tok| match tok {
            Token::Kind(ident) => Ok(ident.clone()),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
        }).labelled("Type");

        let params = ident.clone().labelled("param name")
            .then(just(Token::Ctrl(':')).ignore_then(kind))
            .repeated()
            .labelled("parameters");

        let function = ident.clone()
            .labelled("function name")
            .then(params.delimited_by(Token::Ctrl('('), Token::Ctrl(')')))
            .then(expr)
            .then_ignore(just(Token::End))
            .map(|((name, params), instructions)| Expr::Function(name, params, Box::new(instructions)))
            .labelled("function");

        let block_expr = function.or(if_).or(raw_expression.clone()).labelled("block");

        let block_chain = block_expr
            .clone()
            .then(block_expr.clone().repeated())
            .foldl(|a, b| Expr::Then(Box::new(a), Box::new(b)));

        block_chain
    })
}

pub fn kind() -> impl Parser<char, Vec<char>, Error = Simple<char>> + Copy + Clone {
    filter(|c: &char| c.is_uppercase())
        .map(Some)
        .chain(filter(|c: &char| c.is_ascii_alphanumeric() || *c == '_').repeated())
}

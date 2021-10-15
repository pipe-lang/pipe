use chumsky::{prelude::*};

use crate::lexer::Token;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Num(String),
    Str(String),
    Array(Vec<Value>),
    Func(String),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Null => write!(f, "null"),
            Self::Bool(x) => write!(f, "{}", x),
            Self::Num(x) => write!(f, "{}", x),
            Self::Str(x) => write!(f, "{}", x),
            Self::Array(xs) => write!(
                f,
                "[{}]",
                xs.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Self::Func(name) => write!(f, "<function: {}>", name),
        }
    }
}
#[derive(Clone, Debug)]
pub enum BinaryOp {
    Add,
    And,
    Div,
    Eq,
    Mul,
    NotEq,
    Or,
    Sub,
    Xor,
}

type Params = Vec<(String, String)>;

#[derive(Debug)]
pub enum Expr {
    Array(Vec<Self>),
    Assignation(Vec<String>, Box<Self>),
    Binary(Box<Self>, BinaryOp, Box<Self>),
    Call(Box<Self>, Vec<Self>),
    Error,
    Function(String, Params, Box<Expr>),
    If(Box<Self>, Vec<Self>, Vec<Self>),
    Pipe(Box<Self>, Box<Self>),
    Struct(String, Params),
    Then(Box<Self>, Box<Self>), // NOTE: How to turn this into a Block(Vec<Seff>) instead?
    Value(Value),
    Variable(String),
}

pub fn expression() -> impl Parser<Token, Expr, Error = Simple<Token>> + Clone {
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
                .then(raw_expression.clone())
                .map(|(variables, value)| Expr::Assignation(variables, Box::new(value)));

            let array = raw_expression
                .clone()
                .repeated()
                .delimited_by(Token::Ctrl('['), Token::Ctrl(']'))
                .map(Expr::Array);

            let atom = expr
                .clone()
                .delimited_by(Token::Ctrl('('), Token::Ctrl(')'))
                .or(val.clone())
                .or(assignation)
                .or(ident.map(Expr::Variable))
                .or(array)
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
            .then(params.clone().delimited_by(Token::Ctrl('('), Token::Ctrl(')')))
            .then(expr)
            .then_ignore(just(Token::End))
            .map(|((name, params), instructions)| Expr::Function(name, params, Box::new(instructions)))
            .labelled("function");

        let struct_ = kind.clone().labelled("struct name")
            .then(params.clone().labelled("struct params"))
            .then_ignore(just(Token::End))
            .labelled("struct")
            .map(|(name, params)| Expr::Struct(name, params));

        let block_expr = function.or(struct_).or(if_).or(raw_expression.clone()).labelled("block");

        let block_chain = block_expr
            .clone()
            .then(block_expr.clone().repeated())
            .foldl(|a, b| Expr::Then(Box::new(a), Box::new(b)));

        block_chain
    })
}

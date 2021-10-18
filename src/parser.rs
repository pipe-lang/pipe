use chumsky::prelude::*;

use crate::lexer::Token;

type Params = Vec<(String, String)>;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Array(Vec<Self>),
    Assignation(Vec<String>, Box<Self>),
    Binary(Box<Self>, BinaryOp, Box<Self>),
    Bool(bool),
    Call(String, Vec<Self>),
    Error,
    Function(String, Params, Box<Expr>),
    If(Box<Self>, Vec<Self>, Vec<Self>),
    Num(String),
    Pipe(Box<Self>, Box<Self>),
    Str(String),
    Struct(String, Vec<StructAttr>),
    StructDef(String, Params),
    Block(Vec<Self>),
    Variable(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum StructAttr {
    Named(String, Expr),
    Value(Expr),
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    And,
    Sub,
    Div,
    Mul,
    Eq,
    NotEq,
    Or,
    Xor,
}

pub fn expression() -> impl Parser<Token, Expr, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let ident = filter_map(|span, tok| match tok {
            Token::Ident(ident) => Ok(ident.clone()),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
        })
        .labelled("identifier");

        let kind = filter_map(|span, tok| match tok {
            Token::Kind(ident) => Ok(ident.clone()),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
        })
        .labelled("Type");

        let raw_expression = recursive(|raw_expression| {
            let val = filter_map(|span, tok| match tok {
                Token::Bool(x) => Ok(Expr::Bool(x)),
                Token::Num(n) => Ok(Expr::Num(n)),
                Token::Str(s) => Ok(Expr::Str(s)),
                _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
            })
            .labelled("value");

            let assignees = ident.repeated();
            let assignation = assignees
                .then_ignore(just(Token::Op("=".to_string())))
                .then(raw_expression.clone())
                .map(|(variables, value)| Expr::Assignation(variables, Box::new(value)));

            let array = raw_expression
                .clone()
                .repeated()
                .delimited_by(Token::Ctrl('['), Token::Ctrl(']'))
                .map(Expr::Array)
                .labelled("array");

            let attr_named = ident
                .clone()
                .labelled("attribute name")
                .then(just(Token::Ctrl(':')).ignore_then(raw_expression.clone()))
                .map(|(name, value)| StructAttr::Named(name, value));

            let attr_value = raw_expression.clone().map(|exp| StructAttr::Value(exp));

            let attributes = attr_named.or(attr_value).repeated();

            let struct_value = kind
                .clone()
                .then(attributes.delimited_by(Token::Ctrl('{'), Token::Ctrl('}')))
                .map(|(name, attributes)| Expr::Struct(name, attributes));

            let call = ident
                .clone()
                .then(
                    raw_expression
                        .clone()
                        .repeated()
                        .delimited_by(Token::Ctrl('('), Token::Ctrl(')')),
                )
                .map(|(name, args)| Expr::Call(name, args));

            let atom = expr
                .clone()
                .delimited_by(Token::Ctrl('('), Token::Ctrl(')'))
                .or(val.clone())
                .or(call)
                .or(struct_value)
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

            let op = just(Token::Op("*".to_string()))
                .to(BinaryOp::Mul)
                .or(just(Token::Op("/".to_string())).to(BinaryOp::Div));
            let product = atom
                .clone()
                .then(op.then(atom).repeated())
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

            let call_after_pipe = ident
                .clone()
                .then(compare.clone().repeated())
                .map(|(name, args)| Expr::Call(name, args));

            let pipe = compare
                .clone()
                .then(
                    just(Token::Pipe)
                        .ignore_then(call_after_pipe.or(compare))
                        .repeated(),
                )
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

        let params = ident
            .clone()
            .labelled("param name")
            .then(just(Token::Ctrl(':')).ignore_then(kind))
            .repeated()
            .labelled("parameters");

        let function = ident
            .clone()
            .labelled("function name")
            .then(
                params
                    .clone()
                    .delimited_by(Token::Ctrl('('), Token::Ctrl(')')),
            )
            .then(expr)
            .then_ignore(just(Token::End))
            .map(|((name, params), instructions)| {
                Expr::Function(name, params, Box::new(instructions))
            })
            .labelled("function");

        let struct_ = kind
            .clone()
            .labelled("struct name")
            .then(params.clone().labelled("struct params"))
            .then_ignore(just(Token::End))
            .labelled("struct definition")
            .map(|(name, params)| Expr::StructDef(name, params));

        let block_expr = function
            .or(struct_)
            .or(if_)
            .or(raw_expression.clone())
            .labelled("block");

        let block_chain = block_expr
            .clone()
            .repeated()
            .at_least(1)
            .collect::<Vec<Expr>>()
            .map(Expr::Block);

        block_chain
    })
}

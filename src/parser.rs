use crate::lexer::{Span, Token};
use chumsky::prelude::*;

type Params = Vec<(String, String)>;
pub type Spanned<T> = (T, Span);

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Array(Vec<Spanned<Self>>),
    Assignation(Vec<String>, Box<Spanned<Self>>),
    Binary(Box<Spanned<Self>>, BinaryOp, Box<Spanned<Self>>),
    Bool(bool),
    Call(String, Vec<Spanned<Self>>),
    Error,
    Function(String, Params, Box<Spanned<Self>>),
    If(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Num(String),
    Pipe(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Str(String),
    Struct(String, Vec<StructAttr>),
    StructDef(String, Params),
    Block(Vec<Spanned<Self>>),
    Variable(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum StructAttr {
    Named(String, Spanned<Expr>),
    Value(Spanned<Expr>),
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

pub fn expression() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let ident = filter_map(|span: Span, tok| match tok {
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

            let atom = val.clone()
                .or(call)
                .or(struct_value)
                .or(assignation)
                .or(ident.map(Expr::Variable))
                .or(array)
                .map_with_span(|expr, span| (expr, span))
                .or(expr
                    .clone()
                    .delimited_by(Token::Ctrl('('), Token::Ctrl(')')))
                .recover_with(nested_delimiters(
                    Token::Ctrl('('),
                    Token::Ctrl(')'),
                    [(Token::Ctrl('['), Token::Ctrl(']'))],
                    |span| (Expr::Error, span),
                ))
                .recover_with(nested_delimiters(
                    Token::Ctrl('['),
                    Token::Ctrl(']'),
                    [(Token::Ctrl('('), Token::Ctrl(')'))],
                    |span| (Expr::Error, span),
                ));

            let op = just(Token::Op("*".to_string()))
                .to(BinaryOp::Mul)
                .or(just(Token::Op("/".to_string())).to(BinaryOp::Div));
            let product = atom
                .clone()
                .then(op.then(atom).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                });

            let op = just(Token::Op("+".to_string()))
                .to(BinaryOp::Add)
                .or(just(Token::Op("-".to_string())).to(BinaryOp::Sub));

            let sum = product
                .clone()
                .then(op.then(product).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                });

            let op = just(Token::And)
                .to(BinaryOp::And)
                .or(just(Token::Or).to(BinaryOp::Or))
                .or(just(Token::Xor).to(BinaryOp::Xor))
                .or(just(Token::Eq).to(BinaryOp::Eq))
                .or(just(Token::Op("!=".to_string())).to(BinaryOp::NotEq));

            let compare = sum
                .clone()
                .then(op.then(sum).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                });

            let call_after_pipe = ident
                .clone()
                .then(compare.clone().repeated())
                .map_with_span(|(name, args), span| (Expr::Call(name, args), span));

            let pipe = compare
                .clone()
                .then(
                    just(Token::Pipe)
                        .ignore_then(call_after_pipe.or(compare))
                        .repeated(),
                )
                .foldl(|left, right| {
                    let span = left.1.start..right.1.end;
                    (Expr::Pipe(Box::new(left), Box::new(right)), span)
                });

            pipe
        });

        let block = raw_expression
            .clone()
            .repeated()
            .collect::<Vec<Spanned<Expr>>>()
            .map_with_span(|block, span| (Expr::Block(block), span));

        let if_ = just(Token::If)
            .ignore_then(raw_expression.clone())
            .then(block.clone())
            .then(just(Token::Else).ignore_then(block.clone()).or_not())
            .then_ignore(just(Token::End))
            .map_with_span(|((conditional, consequent), alternative), span| {
                (
                    Expr::If(
                        Box::new(conditional),
                        Box::new(consequent),
                        match alternative {
                            Some(alternative) => Box::new(alternative),
                            None => Box::new((Expr::Block(Vec::new()), (0..0))),
                        },
                    ),
                    span,
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
            .map_with_span(|((name, params), instructions), span| {
                (Expr::Function(name, params, Box::new(instructions)), span)
            })
            .labelled("function");

        let struct_ = kind
            .clone()
            .labelled("struct name")
            .then(params.clone().labelled("struct params"))
            .then_ignore(just(Token::End))
            .labelled("struct definition")
            .map_with_span(|(name, params), span| (Expr::StructDef(name, params), span));

        let block_expr = function
            .or(struct_)
            .or(if_)
            .or(raw_expression.clone())
            .labelled("block");

        let block_chain = block_expr
            .clone()
            .repeated()
            .at_least(1)
            .map_with_span(|block, span| (Expr::Block(block), span));

        block_chain
    })
}

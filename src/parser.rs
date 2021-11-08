use crate::lexer::{Span, Token};
use chumsky::prelude::*;

type Params = Vec<(String, Option<String>)>;
pub type Spanned<T> = (T, Span);

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Array(Vec<Spanned<Self>>),
    Assignation(Vec<String>, Box<Spanned<Self>>),
    Binary(Box<Spanned<Self>>, BinaryOp, Box<Spanned<Self>>),
    Bool(bool),
    Call(String, Vec<Spanned<Self>>),
    Error,
    Function(String, Params, Box<Spanned<Self>>, Option<Box<Spanned<Self>>>),
    If(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    Is(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Num(String),
    Pipe(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Str(String),
    Struct(String, Vec<StructAttr>),
    StructDef(String, Params),
    Block(Vec<Spanned<Self>>),
    Check(Vec<Spanned<Self>>),
    Module(Vec<Spanned<Self>>),
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

pub fn module() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {
    let identifier = filter_map(|span: Span, tok| match tok {
        Token::Ident(identifier) => Ok(identifier.clone()),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    }).labelled("identifier");

    let kind = filter_map(|span, tok| match tok {
        Token::Kind(identifier) => Ok(identifier.clone()),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    }).labelled("Type");

    let raw_expression = recursive(|raw_expression| {
        let val = filter_map(|span, tok| match tok {
            Token::Bool(x) => Ok(Expr::Bool(x)),
            Token::Num(n) => Ok(Expr::Num(n)),
            Token::Str(s) => Ok(Expr::Str(s)),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
        }).labelled("value");

        let array = raw_expression.clone().repeated()
            .delimited_by(Token::Ctrl('['), Token::Ctrl(']'))
            .map(Expr::Array)
            .labelled("array");

        let attr_named = identifier.clone()
            .labelled("attribute name")
            .then(just(Token::Ctrl(':')).ignore_then(raw_expression.clone()))
            .map(|(name, value)| StructAttr::Named(name, value));

        let attr_value = raw_expression.clone().map(|exp| StructAttr::Value(exp));

        let attributes = attr_named.or(attr_value).labelled("attribute").repeated();

        let struct_value = kind.clone()
            .then(attributes.delimited_by(Token::Ctrl('{'), Token::Ctrl('}')))
            .map(|(name, attributes)| Expr::Struct(name, attributes));

        let call = identifier.clone()
            .then(
                raw_expression.clone().repeated().delimited_by(Token::Ctrl('('), Token::Ctrl(')'))
            )
            .map(|(name, args)| Expr::Call(name, args))
            .labelled("function call");

        let variable = identifier.map(Expr::Variable);

        let atom = val.clone()
            .or(call)
            .or(struct_value)
            .or(variable)
            .or(array)
            .map_with_span(|expr, span| (expr, span))
            .or(raw_expression.clone().delimited_by(Token::Ctrl('('), Token::Ctrl(')')))
            .recover_with(nested_delimiters(Token::Ctrl('('), Token::Ctrl(')'),
                [(Token::Ctrl('['), Token::Ctrl(']'))],
                |span| (Expr::Error, span),
            ))
            .recover_with(nested_delimiters(Token::Ctrl('['),Token::Ctrl(']'),
                [(Token::Ctrl('('), Token::Ctrl(')'))],
                |span| (Expr::Error, span),
            ));

        let op = just(Token::Op("*".to_string())).to(BinaryOp::Mul)
            .or(just(Token::Op("/".to_string())).to(BinaryOp::Div));

        let product = atom.clone()
            .then(op.then(atom).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (Expr::Binary(Box::new(a), op, Box::new(b)), span)
            });

        let op = just(Token::Op("+".to_string())).to(BinaryOp::Add)
            .or(just(Token::Op("-".to_string())).to(BinaryOp::Sub));

        let sum = product.clone()
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

        let compare = sum.clone()
            .then(op.then(sum).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (Expr::Binary(Box::new(a), op, Box::new(b)), span)
            });

        let call_after_pipe = identifier.clone()
            .then(compare.clone().repeated())
            .map_with_span(|(name, args), span| (Expr::Call(name, args), span));

        let pipe = compare.clone()
            .then(just(Token::Pipe).ignore_then(call_after_pipe.or(compare)).repeated())
            .foldl(|left, right| {
                let span = left.1.start..right.1.end;
                (Expr::Pipe(Box::new(left), Box::new(right)), span)
            });

        pipe
    });

    let expression = recursive(|expression| {
        let assignees = identifier.repeated();
        let assignation = assignees
            .then_ignore(just(Token::Op("=".to_string())))
            .then(expression.clone())
            .map_with_span(|(variables, value), span| (Expr::Assignation(variables, Box::new(value)), span))
            .labelled("assignation");

        let block = expression.clone().repeated()
            .collect::<Vec<Spanned<Expr>>>()
            .map_with_span(|block, span| (Expr::Block(block), span));

        let if_ = just(Token::If)
            .ignore_then(raw_expression.clone())
            .then(block.clone())
            .then(just(Token::Else).ignore_then(block.clone()).or_not())
            .then_ignore(just(Token::End))
            .map_with_span(|((conditional, consequent), alternative), span| {
                (Expr::If(Box::new(conditional), Box::new(consequent), match alternative {
                    Some(alternative) => Box::new(alternative),
                    None => Box::new((Expr::Block(Vec::new()), (0..0))),
                }), span)
        }).labelled("conditional");

        assignation.or(if_.clone()).or(raw_expression.clone())
    }).labelled("expression");

    let params = identifier.clone().labelled("param name")
        .then(just(Token::Ctrl(':')).ignore_then(kind).or_not())
        .repeated()
        .labelled("parameters");

    let block = expression.clone().repeated()
        .collect::<Vec<Spanned<Expr>>>()
        .map_with_span(|block, span| (Expr::Block(block), span));

    let is = raw_expression.clone().then(just(Token::Is).ignore_then(raw_expression.clone()))
        .map_with_span(|(left, right), span| (Expr::Is(Box::new(left), Box::new(right)), span));

    let check_block = is.or(expression.clone()).repeated()
        .collect::<Vec<Spanned<Expr>>>()
        .map_with_span(|block, span| (Expr::Check(block), span));

    let check = just(Token::Check).ignore_then(check_block.clone());

    let function = identifier.clone().labelled("function name")
        .then(params.clone().delimited_by(Token::Ctrl('('), Token::Ctrl(')')))
        .then(block.clone())
        .then(check.clone().or_not())
        .then_ignore(just(Token::End))
        .map_with_span(|(((name, params), instructions), check_instructions), span| {
            let check_instructions = match check_instructions {
                Some(spanned_exp) => Some(Box::new(spanned_exp)),
                None => None
            };

            (Expr::Function(name, params, Box::new(instructions), check_instructions), span)
        })
        .labelled("function");

    let struct_ = kind.clone().labelled("struct name")
        .then(params.clone().labelled("struct params"))
        .then_ignore(just(Token::End))
        .labelled("struct definition")
        .map_with_span(|(name, params), span| (Expr::StructDef(name, params), span));

    let module_block = check.then_ignore(just(Token::End))
        .or(function)
        .or(struct_)
        .or(expression);

    let module = module_block.clone().repeated().at_least(1)
        .map_with_span(|block, span| (Expr::Module(block), span));

    module.then_ignore(end())
}

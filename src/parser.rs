use chumsky::prelude::*;

pub type Span = std::ops::Range<usize>;

type Block = Vec<Lang>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Lang {
    Array(Block),
    Bool(bool),
    Float(String),
    If(Box<Lang>, Block, Block),
    Number(String),
    Str(String),
    Variable(String),
    Binary(Box<Lang>, BinaryOperator, Box<Lang>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Mul,
    Div,
    And,
    Or,
    Equals,
    Pipe,
}

pub fn instruction() -> impl Parser<char, (Lang, Span), Error = Simple<char>> {
    let number = text::int(10)
        .collect::<String>()
        .map(Lang::Number)
        .labelled("number");

    let float = text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)))
        .collect::<String>()
        .map(Lang::Float)
        .labelled("float");

    let true_ = seq("true".chars()).to(Lang::Bool(true)).labelled("true");
    let false_ = seq("false".chars()).to(Lang::Bool(false)).labelled("false");

    let boolean = true_.or(false_).labelled("boolean");

    let str_ = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Lang::Str)
        .labelled("string");

    let variable = text::ident().collect::<String>().map(Lang::Variable);

    let raw_value = boolean.or(variable).or(str_).or(float).or(number);

    let plus = seq("+".chars()).to(BinaryOperator::Plus);
    let minus = seq("-".chars()).to(BinaryOperator::Minus);
    let mul = seq("*".chars()).to(BinaryOperator::Mul);
    let div = seq("/".chars()).to(BinaryOperator::Div);
    let eq = seq("eq".chars()).to(BinaryOperator::Equals);
    let or = seq("or".chars()).to(BinaryOperator::Or);
    let and = seq("and".chars()).to(BinaryOperator::And);
    let pipe = seq("|".chars()).to(BinaryOperator::Pipe);

    let instruction = recursive(|instruction| {
        let array = instruction
            .clone()
            .repeated()
            .delimited_by('[', ']')
            .map(Lang::Array)
            .labelled("array");

        let atom = raw_value.or(array.clone());

        let pipe = atom
            .clone()
            .then(pipe.padded().then(atom).repeated())
            .foldl(|left, (operator, right)| {
                Lang::Binary(Box::new(left), operator, Box::new(right))
            });

        let product_operator = mul.or(div).padded();
        let product = pipe
            .clone()
            .then(product_operator.then(pipe.clone()).repeated())
            .foldl(|left, (operator, right)| {
                Lang::Binary(Box::new(left), operator, Box::new(right))
            });

        let sum_operator = plus.or(minus).padded();
        let sum = product
            .clone()
            .then(sum_operator.then(product.clone()).repeated())
            .foldl(|left, (operator, right)| {
                Lang::Binary(Box::new(left), operator, Box::new(right))
            });

        let bool_operator = eq.or(or).or(and).padded();
        let boolean_op = sum
            .clone()
            .then(bool_operator.then(sum.clone()).repeated())
            .foldl(|left, (operator, right)| {
                Lang::Binary(Box::new(left), operator, Box::new(right))
            });

        boolean_op.padded()
    })
    .labelled("instruction");

    let block = instruction.clone().repeated();

    let if_ = seq("if".chars())
        .ignore_then(instruction.clone())
        .then(block.clone())
        .then(seq("else".chars()).ignore_then(block.clone()).or_not())
        .then_ignore(seq("end".chars()))
        .map(|((condition, consequent), alternative)| {
            Lang::If(
                Box::new(condition),
                consequent,
                match alternative {
                    Some(alternative) => alternative,
                    None => vec![],
                },
            )
        })
        .labelled("conditional");

    let declaration = if_;

    declaration
        .or(instruction)
        .map_with_span(|tok, span| (tok, span))
        .padded()
}

pub fn lexer() -> impl Parser<char, Vec<(Lang, Span)>, Error = Simple<char>> {
    let block = instruction().repeated();

    block.then_ignore(end())
}

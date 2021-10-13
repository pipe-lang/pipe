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
    Binary(Box<Lang>, Operator, Box<Lang>),
    Pipe(Box<Lang>, Box<Lang>),
    Assignation(Vec<Lang>, Box<Lang>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Operator {
    Plus,
    Minus,
    Mul,
    Div,
    And,
    Or,
    Equals,
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

    let plus = seq("+".chars()).to(Operator::Plus);
    let minus = seq("-".chars()).to(Operator::Minus);
    let mul = seq("*".chars()).to(Operator::Mul);
    let div = seq("/".chars()).to(Operator::Div);
    let eq = seq("eq".chars()).to(Operator::Equals);
    let or = seq("or".chars()).to(Operator::Or);
    let and = seq("and".chars()).to(Operator::And);

    let instruction = recursive(|instruction| {
        let array = instruction
            .clone()
            .repeated()
            .delimited_by('[', ']')
            .map(Lang::Array)
            .labelled("array");

        let atom = raw_value.or(array.clone());

        let product_operator = mul.or(div).padded();
        let product = atom
            .clone()
            .then(product_operator.then(atom.clone()).repeated())
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

        let pipe = boolean_op
            .clone()
            .then(seq("|".chars()).padded().ignore_then(boolean_op).repeated())
            .foldl(|left, right| Lang::Pipe(Box::new(left), Box::new(right)));

        pipe.padded()
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

    let assignees = variable.padded().repeated();
    let assignation = assignees
        .then_ignore(seq("=".chars()).padded())
        .then(instruction.clone())
        .map(|(variables, value)| Lang::Assignation(variables, Box::new(value)));

    let declaration = if_.or(assignation);

    declaration
        .or(instruction.clone())
        .map_with_span(|tok, span| (tok, span))
        .padded()
}

pub fn lexer() -> impl Parser<char, Vec<(Lang, Span)>, Error = Simple<char>> {
    let block = instruction().repeated();

    block.then_ignore(end())
}

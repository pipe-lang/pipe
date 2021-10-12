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

    let instruction = recursive(|instruction| {
        let array = instruction
            .clone()
            .repeated()
            .delimited_by('[', ']')
            .map(Lang::Array)
            .labelled("array");

        array.or(boolean).or(str_).or(float).or(number).padded()
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

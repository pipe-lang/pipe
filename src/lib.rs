use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::{prelude::*, stream::Stream};
use std::path::Path;
use std::fs;

pub mod lexer;
pub mod parser;
pub mod call;
pub mod wasm;

pub fn parse(module_name: &str, code: &str) -> Result<String, Vec<Simple<char>>> {
    let (tokens, errs) = lexer::lexer().parse_recovery(code);

    let parse_errs = if let Some(tokens) = tokens {
        let len = code.chars().count();
        let (ast_result, parse_errs) = parser::module(module_name)
            .parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

        if parse_errs.len() == 0 {
            if let Some(ast) = ast_result {
                let (ast, _span) = call::find(ast);

                let wasm_code = wasm::convert(&ast);

                return Ok(wasm_code)
            }
        }

        parse_errs
    } else {
        Vec::new()
    };

    errs.clone().into_iter()
        .map(|e| e.map(|c| c.to_string()))
        .chain(parse_errs.into_iter().map(|e| e.map(|tok| tok.to_string())))
        .for_each(|e| {
            let report = Report::build(ReportKind::Error, (), e.span().start)
                .with_code(3)
                .with_message(format!(
                    "{}, expected {}",
                    if e.found().is_some() {
                        "Unexpected token in input"
                    } else {
                        "Unexpected end of input"
                    },
                    if e.expected().len() == 0 {
                        "end of input".to_string()
                    } else {
                        e.expected()
                            .map(|x| x.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    }
                ))
                .with_label(
                    Label::new(e.span().start..e.span().end)
                        .with_message(format!(
                            "Unexpected {}",
                            e.found()
                                .map(|c| format!("token {}", c.fg(Color::Red)))
                                .unwrap_or_else(|| "end of input".to_string())
                        ))
                        .with_color(Color::Red),
                );

            let report = match e.reason() {
                chumsky::error::SimpleReason::Unclosed { span, delimiter } => report.with_label(
                    Label::new(span.clone())
                        .with_message(format!("Unclosed delimiter {}",delimiter.fg(Color::Yellow)))
                        .with_color(Color::Yellow),
                ),
                chumsky::error::SimpleReason::Unexpected => report,
                chumsky::error::SimpleReason::Custom(msg) => report
                    .with_label(Label::new(e.span())
                    .with_message(format!("{}", msg.fg(Color::Yellow)))
                    .with_color(Color::Yellow)),
            };

            report.finish().print(Source::from(&code)).unwrap();
        });

    Err(errs)
}

pub fn run(file_name: &str) {
    let src = fs::read_to_string(&file_name).expect("Failed to read file");
    let module = Path::new(&file_name).file_stem().unwrap().to_str().unwrap();
    if let Ok(wasm) = parse(module, &src) {
        wasm::run(&wasm);
    }
}

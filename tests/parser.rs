use pipe::*;
use parser::*;

#[test]
fn parse_number() {
    let code = "42";

    let expt = (Expr::Block(vec![(Expr::Num("42".to_string()) , 0..2)]), 0..2);

    assert_eq!(parse(code), Ok(Some(expt)))
}

#[test]
fn parse_string() {
    let code = "\"hola\"";

    let expt = (Expr::Block(vec![(Expr::Str("hola".to_string()) , 0..6)]), 0..6);

    assert_eq!(parse(code), Ok(Some(expt)))
}

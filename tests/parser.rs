use indoc::indoc;
use parser::*;
use pipe::*;

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

#[test]
fn method() {
    let code = indoc! {"
      add(lhs rhs)
        lhs + rhs
      end
    "};

    let expt = (Expr::Block(
            vec![
                (
                    Expr::Function("add".to_string(), vec![("lhs".to_string(), None), ("rhs".to_string(), None)],
                    Box::new(
                        (Expr::Block(vec![
                            (Expr::Binary(
                                Box::new((Expr::Variable("lhs".to_string()), 15..18)),
                                BinaryOp::Add,
                                Box::new((Expr::Variable("rhs".to_string()), 21..24))
                            ), 15..24)
                        ]), 15..24))
                    )
                    ,0..28
                )
            ])
        , 0..28);

    assert_eq!(parse(code), Ok(Some(expt)))
}

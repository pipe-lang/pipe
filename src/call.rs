use crate::parser::*;

type Scope = Vec<String>;

pub fn parse(ast: Spanned<Expr>, scope: &mut Scope) -> Spanned<Expr> {
    match ast {
        (Expr::Array(array), span) => (Expr::Array(array.iter().map(|elem| parse(elem.clone(), scope)).collect()), span),
        (Expr::Assignation(variables, instruction), span) => {
            let updated_instruction = parse(*instruction, scope);

            (Expr::Assignation(variables.clone(), Box::new(updated_instruction)), span)
        }
        (Expr::Binary(left, op, right), span) => {
            let left = parse(*left, scope);
            let right = parse(*right, scope);

            (Expr::Binary(Box::new(left), op.clone(), Box::new(right)), span)
        }
        (Expr::Call(name, instructions), span) => (Expr::Call(
            name.clone(),
            instructions.iter().map(|inst| parse(inst.clone(), scope)).collect(),
        ), span),
        (Expr::Function(name, params, body, check), span) => {
            scope.push(name.clone());

            let body = parse(*body, scope);
            let check = match check {
                Some(spanned_expr) => Some(Box::new(parse(*spanned_expr, scope))),
                None => None
            };

            (Expr::Function(name.clone(), params.clone(), Box::new(body), check), span)
        }
        (Expr::If(instruction, then, else_), span) => {
            let instruction = parse(*instruction, scope);
            let then = parse(*then, scope);
            let else_ = parse(*else_, scope);

            (Expr::If(Box::new(instruction), Box::new(then), Box::new(else_)), span)
        }
        (Expr::Pipe(left, right), span) => {
            let left = parse(*left, scope);
            let right = parse(*right, scope);

            (Expr::Pipe(Box::new(left), Box::new(right)), span)
        }
        (Expr::Block(instructions), span) => {
            let instructions = instructions.iter().map(|inst| parse(inst.clone(), scope)).collect();

            (Expr::Block(instructions), span)
        }
        (Expr::Check(instructions), span) => {
            let instructions = instructions.iter().map(|inst| parse(inst.clone(), scope)).collect();

            (Expr::Check(instructions), span)
        }
        (Expr::Module(instructions), span) => {
            let instructions = instructions.iter().map(|inst| parse(inst.clone(), scope)).collect();

            (Expr::Module(instructions), span)
        }
        (Expr::Variable(name), span) => {
            if scope.into_iter().any(|function| *function == name ) {
                (Expr::Call(name, vec![]), span)
            } else {
                (Expr::Variable(name), span)
            }
        }
        other => other.clone(),
    }
}

pub fn find(ast: Spanned<Expr>) -> Spanned<Expr> {
    let mut scope = Scope::new();

    parse(ast, &mut scope)
}

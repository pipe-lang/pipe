use crate::parser::*;
use std::collections::HashMap;

#[derive(Clone, PartialEq, Copy)]
pub enum ValueType {
    Function,
    Variable,
}

type Scope = Vec<HashMap<String, ValueType>>;

fn insert_definition(scope: &mut Scope, name: String, kind: ValueType) {
    let current_scope = scope.last_mut().unwrap();

    current_scope.insert(name, kind);
}

fn add_level(scope: &mut Scope) {
    scope.push(HashMap::new())
}

fn remove_level(scope: &mut Scope) {
    scope.pop();
}

fn find_definition(scope: &Scope, name: &str) -> Option<ValueType> {
    for level in scope.iter() {
        match level.get(name) {
            Some(definition) => return Some(*definition),
            None => (),
        }
    }

    None
}

pub fn find(expression: &Expr, scope: &mut Scope) -> Expr {
    match expression {
        Expr::Array(array) => Expr::Array(array.iter().map(|elem| find(elem, scope)).collect()),
        Expr::Assignation(variables, instruction) => {
            let updated_instruction = find(instruction, scope);

            for variable in variables {
                insert_definition(scope, variable.clone(), ValueType::Variable);
            }

            Expr::Assignation(variables.clone(), Box::new(updated_instruction))
        }
        Expr::Binary(left, op, right) => {
            let left = find(left, scope);
            let right = find(right, scope);

            Expr::Binary(Box::new(left), op.clone(), Box::new(right))
        }
        Expr::Call(name, instructions) => Expr::Call(
            name.clone(),
            instructions.iter().map(|inst| find(inst, scope)).collect(),
        ),
        Expr::Function(name, params, body) => {
            insert_definition(scope, name.clone(), ValueType::Function);
            let body = find(body, scope);

            Expr::Function(name.clone(), params.clone(), Box::new(body))
        }
        Expr::If(instruction, then, else_) => {
            let instruction = find(instruction, scope);
            let then = find(then, scope);
            let else_ = find(else_, scope);

            Expr::If(Box::new(instruction), Box::new(then), Box::new(else_))
        }
        Expr::Pipe(left, right) => {
            let left = find(left, scope);
            let right = find(right, scope);

            Expr::Pipe(Box::new(left), Box::new(right))
        }
        Expr::Block(instructions) => {
            add_level(scope);
            let instructions = instructions.iter().map(|inst| find(inst, scope)).collect();
            remove_level(scope);
            Expr::Block(instructions)
        }
        Expr::Variable(name) => {
            let definition = find_definition(scope, name);

            match definition {
                Some(definition) => match definition {
                    ValueType::Function => Expr::Call(name.clone(), vec![]),
                    ValueType::Variable => Expr::Variable(name.clone()),
                },
                None => Expr::Str(name.clone()),
            }
        }
        other => other.clone(),
    }
}

pub fn start(ast: &Expr) -> Expr {
    find(ast, &mut Scope::new())
}

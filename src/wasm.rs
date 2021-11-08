use anyhow::Result;
use indexmap::IndexMap;
use indoc::indoc;
use wasmtime::*;
use wasmtime_wasi::sync::WasiCtxBuilder;

use crate::parser::*;

pub struct Scope {
    pointers: IndexMap<String, (usize, usize)>,
    memory: String
}

impl Scope {
    pub fn new() -> Scope {
        Scope { pointers: IndexMap::new(), memory: "".to_string() }
    }
}

pub fn do_convert(expression: &Expr, scope: &mut Scope) -> String {
    match expression {
        Expr::Bool(thruty) => {
            let value = if *thruty { 1 } else { 0 };

            format!("(i32.const {})", value)
        }
        Expr::Variable(name) => format!("(local.get ${})", name),
        Expr::Num(num) => format!("(local.get ${})", num),
        Expr::Block(instructions) => instructions.into_iter()
            .map(|(expression, _span)| do_convert(expression, scope))
            .collect::<Vec<String>>()
            .join(" "),
        Expr::Str(string) => {
            if let Some(position) = scope.pointers.get(string) {
                format!("(i32.const {}) (i32.const {})", position.0, position.1)
            } else {
                let position = (scope.memory.len(), string.len());

                scope.pointers.insert(string.to_owned(), position);
                scope.memory += &string;

                format!("(i32.const {}) (i32.const {})", position.0, position.1)
            }
        }
        Expr::Module(name, expressions) => {
            let imports_str = "".to_string();
            let types_str   = "".to_string();
            let exports_str = "".to_string();

            let mut functions          = Vec::new();
            let mut instructions       = Vec::new();
            let mut struct_definitions = Vec::new();

            for (expression, _span) in expressions {
                match expression {
                    Expr::Function(_name, _params, _body, _check) => {
                        functions.push(expression);
                    }
                    Expr::StructDef(_name, _params) => struct_definitions.push(expression),
                    _ => instructions.push(expression)
                }
            }

            let main_body = instructions.into_iter().map(|inst| format!("{} drop", do_convert(inst, scope))).collect::<Vec<String>>().join(" ");
            let main = format!("(func (export \"_start\") {})", main_body);

            let functions_str = functions.into_iter().map(|function| do_convert(function, scope)).collect::<Vec<String>>().join(" ");

            let data_str = if scope.memory.len() > 0 {
                format!("(data (i32.const 0) \"{}\")", scope.memory)
            } else {
                "".to_string()
            };

            let body = vec![imports_str, data_str, functions_str, main, types_str, exports_str]
                .into_iter()
                .filter(|x| x != "")
                .collect::<Vec<String>>()
                .join("\n  ");

            format!(indoc! {r#"
            (module ${}
              (import "wasi_unstable" "fd_write" (func $fd_write (param i32 i32 i32 i32) (result i32)))
              (memory 1)
              (export "memory" (memory 0))

              (func $print (param i32 i32) (result i32)
                (i32.store (i32.const 20) (local.get 0))
                (i32.store (i32.const 24) (local.get 1))

                (call $fd_write
                  (i32.const 1)  ;; file_descriptor - 1 for stdout
                  (i32.const 20) ;; *iovs - The pointer to the iov array
                  (i32.const 1)  ;; iovs_len - We're printing 1 string stored in an iov - so one.
                  (i32.const 0)  ;; nwritten - A place in memory to store the number of bytes written
                )
              )

              {}
            )"#}, name, body)
        }
        Expr::Call(name, params) => {
            let params_str = params.into_iter()
                .map(|(instruction, _span)| do_convert(instruction, scope))
                .collect::<Vec<String>>()
                .join(" ");

            format!("(call ${} {})", name, params_str)
        },
        Expr::Function(name, params, body, check_body) => {
            let params_str = "".to_string();
            let results    = "".to_string();
            let locals    = "".to_string();

            let (block, span) = &**body;

            let instructions = do_convert(block, scope);

            let body = vec![params_str, results, locals, instructions]
                .into_iter()
                .filter(|x| x != "")
                .collect::<Vec<String>>()
                .join(" ");

            format!("(func ${} {})", name, body)
        }
        _ => "".to_string()
    }
}

pub fn convert(expression: &Expr) -> String {
    do_convert(expression, &mut Scope::new())
}

// From https://docs.wasmtime.dev/examples-rust-wasi.html#wasirs
pub fn run(code: &str) -> Result<()> {
    let engine = Engine::default();
    let mut linker = Linker::new(&engine);
    wasmtime_wasi::add_to_linker(&mut linker, |s| s)?;

    let wasi = WasiCtxBuilder::new().inherit_stdio().inherit_args()?.build();
    let mut store = Store::new(&engine, wasi);

    let module = Module::new(&engine, code)?;
    linker.module(&mut store, "", &module)?;
    linker.get_default(&mut store, "")?.typed::<(), (), _>(&store)?.call(&mut store, ())?;

    Ok(())
}

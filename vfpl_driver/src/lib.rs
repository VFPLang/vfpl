use std::rc::Rc;
use vfpl_global::{GlobalCtx, Session};

pub fn start() {
    let program_name = std::env::args().nth(1);

    match program_name {
        Some(program_name) => {
            let session = Session::new();
            let global_ctx = Rc::new(GlobalCtx::new(session));

            let stderr = std::io::stderr();
            let mut stderr = stderr.lock();

            let content = std::fs::read_to_string(program_name).unwrap_or_else(|err| {
                eprintln!("Error while reading the file: {}", err);
                std::process::exit(1);
            });

            let tokens = vfpl_lexer::lex(&content, global_ctx.clone()).unwrap_or_else(|err| {
                vfpl_error::display_error(&content, err, &mut stderr, true, global_ctx.clone())
                    .expect("Printing to stderr failed");
                std::process::exit(1);
            });

            let ast =
                vfpl_parser::parse(tokens.into_iter(), global_ctx.clone()).unwrap_or_else(|err| {
                    vfpl_error::display_error(&content, err, &mut stderr, true, global_ctx.clone())
                        .expect("Printing to stderr failed");
                    std::process::exit(1);
                });

            vfpl_ast_interpreter::run(&ast, global_ctx.clone()).unwrap_or_else(|err| {
                vfpl_error::display_error(&content, err, &mut stderr, true, global_ctx.clone())
                    .expect("Printing to stderr failed");
                std::process::exit(1);
            });
        }
        None => eprintln!("Provide a program name as the program argument."),
    }
}

use crate::lexer::Lexer;

mod error;
mod lexer;
mod parse;

/// this is mainly to remove the dead code warnings on code that is not actually dead
pub fn run(code: &str) {
    let tokens = Lexer::new(code).compute_tokens();

    match tokens {
        Ok(tokens) => {
            let ast = parse::parse(tokens.into_iter());

            match ast {
                Ok(ast) => println!("{:#?}", ast),
                Err(error) => eprintln!("{}", error),
            }
        }
        Err(error) => eprintln!("{:?}", error),
    }
}

fn main() {
    let program_name = std::env::args().nth(1);

    match program_name {
        Some(program_name) => {
            let content = std::fs::read_to_string(program_name).unwrap_or_else(|err| {
                eprintln!("Error while reading the file: {}", err);
                std::process::exit(1);
            });

            let tokens = vfpl::lex(&content).unwrap_or_else(|err| {
                vfpl::display_error(&content, err);
                std::process::exit(1);
            });

            let ast = vfpl::parse(tokens.into_iter()).unwrap_or_else(|err| {
                vfpl::display_error(&content, err);
                std::process::exit(1);
            });

            vfpl::run(ast).unwrap_or_else(|err| {
                vfpl::display_error(&content, err);
                std::process::exit(1);
            });
        }
        None => eprintln!("Provide a program name as the program argument."),
    }
}

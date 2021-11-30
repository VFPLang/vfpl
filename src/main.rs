fn main() {
    let program_name = std::env::args().nth(1);

    match program_name {
        Some(program_name) => {
            let content = std::fs::read_to_string(program_name).unwrap_or_else(|err| {
                eprintln!("Error while reading the file: {}", err);
                std::process::exit(1);
            });

            let tokens = vfpl::lex(&content).unwrap_or_else(|err| {
                eprintln!("Lex error: {:?}", err);
                std::process::exit(1);
            });

            let ast = vfpl::parse(tokens.into_iter()).unwrap_or_else(|err| {
                eprintln!("Parse error: {}", err);
                std::process::exit(1);
            });

            println!("{:#?}\nSuccessfully parsed program!", ast);
        }
        None => eprintln!("Provide a program name as the program argument."),
    }
}

fn main() {
    if let Some("--verbose") = std::env::args().nth(1).as_deref() {
        println!("Dear World. Hello. Kind regards, the Interpreter.");
    } else {
        println!("Hello, world!");
    }
}

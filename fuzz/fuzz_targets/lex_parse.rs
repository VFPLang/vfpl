#![no_main]
use libfuzzer_sys::fuzz_target;

use vfpl_global::Session;

fuzz_target!(|input: &str| {
    let session = std::rc::Rc::new(std::cell::RefCell::new(Session::new()));

    if let Ok(tokens) = vfpl::lex(input, session.clone()) {
        let _ = vfpl::parse(tokens.into_iter(), session);
    }
});

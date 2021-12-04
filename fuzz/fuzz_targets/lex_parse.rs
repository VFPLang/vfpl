#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(input) = std::str::from_utf8(data) {
        if let Ok(tokens) = vfpl::lex(&input) {
            let _ = vfpl::parse(tokens.into_iter());
        }
    }
});

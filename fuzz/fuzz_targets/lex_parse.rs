#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    // fuzzed code goes here

    if let Ok(input) = std::str::from_utf8(data) {
        vfpl::run(input);
    }
});

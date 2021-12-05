use fastrand::Rng;

pub fn random_ident(rng: &Rng) -> &'static str {
    const IDENTS_SIZE: usize = 10;
    const IDENTS: [&str; IDENTS_SIZE] = [
        "double_value",
        "five",
        "append",
        "memcpy",
        "to_string",
        "val",
        "integer",
        "x",
        "y",
        "vfpl",
    ];

    let index = rng.usize(0..IDENTS_SIZE);
    IDENTS[index]
}

pub fn random_number(rng: &Rng) -> i64 {
    rng.i64(1..10)
}

use fastrand::Rng;

/// The global session object that stores everything about the VFPL invocation
/// Every part of the interpreter has a reference to it
#[derive(Debug, Default)]
pub struct Session {
    rng: Rng,
}

impl Session {
    pub fn new() -> Self {
        Self { rng: Rng::new() }
    }

    pub fn rng(&self) -> &Rng {
        &self.rng
    }

    /// Only use this for testing
    pub fn test_session() -> Self {
        Self {
            // "VFPL" in ASCII/UTF8
            rng: Rng::with_seed(0x56_46_50_4C),
        }
    }
}

mod global_ctx;
mod session;

pub use global_ctx::GlobalCtx;
pub use session::Session;

/// Only we need to depend on `lasso`, use re-exports for things downstream crates might need
pub use lasso::Spur;

/// The global context used for testing, to make debug impls work better
///
/// Use this context to intern your testing strings
pub use global_ctx::testing::GLOBAL_TEST_CTX;

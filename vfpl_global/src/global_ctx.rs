use crate::Session;
use lasso::{Rodeo, Spur};
use std::fmt::{Debug, Formatter};

pub struct GlobalCtx {
    intern: Rodeo,
    session: Session,
}

impl GlobalCtx {
    pub fn new(session: Session) -> Self {
        Self {
            intern: Rodeo::new(),
            session,
        }
    }

    /// Get the [`Session`]
    pub fn sess(&self) -> &Session {
        &self.session
    }

    /// Intern a string
    pub fn intern_string<T: AsRef<str>>(&mut self, str: T) -> Spur {
        self.intern.get_or_intern(str)
    }

    /// Resolves an interned string
    pub fn resolve_string(&self, spur: &Spur) -> &str {
        self.intern.resolve(spur)
    }

    pub fn test_ctx() -> std::rc::Rc<Self> {
        Self {
            intern: Rodeo::new(),
            session: Session::test_session(),
        }
        .into()
    }
}

/// Impl Debug so that types containing this can derive Debug
impl Debug for GlobalCtx {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("[global context]")
    }
}

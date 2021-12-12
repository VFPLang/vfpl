use crate::Session;
use lasso::{Rodeo, Spur};
use std::cell::RefCell;
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
        match self.intern.try_resolve(spur) {
            Some(str) => str,
            None => "[identifier]",
        }
    }
    /// Resolves an interned string
    pub fn try_resolve_string(&self, spur: &Spur) -> Option<&str> {
        self.intern.try_resolve(spur)
    }

    pub fn test_ctx() -> std::rc::Rc<std::cell::RefCell<Self>> {
        RefCell::new(Self {
            intern: Rodeo::new(),
            session: Session::test_session(),
        })
        .into()
    }
}

/// Impl Debug so that types containing this can derive Debug
impl Debug for GlobalCtx {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("[global context]")
    }
}

pub mod testing {
    //! Provide a global [`GlobalCtx`](super::GlobalCtx) for testing
    //! This is *not* to be used by the normal interpreter

    use crate::{GlobalCtx, Session};
    use once_cell::sync::Lazy;
    use std::sync::{Arc, Mutex};

    pub static GLOBAL_TEST_CTX: Lazy<Arc<Mutex<GlobalCtx>>> =
        Lazy::new(|| Arc::new(Mutex::new(GlobalCtx::new(Session::test_session()))));
}

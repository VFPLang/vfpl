use crate::Session;
use lasso::{Rodeo, Spur};
use std::cell::RefCell;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

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

#[derive(Clone)]
pub struct SpurCtx {
    spur: Spur,
    global_ctx: Rc<RefCell<GlobalCtx>>,
}

impl SpurCtx {
    pub fn new(spur: Spur, global_ctx: Rc<RefCell<GlobalCtx>>) -> Self {
        Self { spur, global_ctx }
    }

    pub fn spur(&self) -> Spur {
        self.spur
    }
}

impl PartialEq for SpurCtx {
    fn eq(&self, other: &Self) -> bool {
        self.spur == other.spur
    }
}

impl Debug for SpurCtx {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let global_ctx = self.global_ctx.borrow();

        Debug::fmt(global_ctx.resolve_string(&self.spur), f)
    }
}

impl Display for SpurCtx {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let global_ctx = self.global_ctx.borrow();

        Display::fmt(global_ctx.resolve_string(&self.spur), f)
    }
}

impl Eq for SpurCtx {}

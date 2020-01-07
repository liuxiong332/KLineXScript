use super::error::{PineError, PineErrorKind};
use super::input::{Input, StrRange};
use std::cell::{Cell, RefCell};

#[derive(Debug, PartialEq, Clone)]
pub struct PineInputError {
    pub code: PineErrorKind,
    pub range: StrRange,
}

impl PineInputError {
    pub fn new(code: PineErrorKind, range: StrRange) -> PineInputError {
        PineInputError { code, range }
    }
}

pub struct AstState {
    errors: RefCell<Vec<PineInputError>>,
    indent: Cell<usize>,
}

impl AstState {
    pub fn new() -> AstState {
        AstState {
            errors: RefCell::new(vec![]),
            indent: Cell::new(0),
        }
    }

    pub fn enter_scope(&self) {
        self.indent.replace(self.indent.get() + 1);
    }

    pub fn exit_scope(&self) {
        debug_assert!(self.indent.get() > 0);
        self.indent.replace(self.indent.get() - 1);
    }

    pub fn get_indent(&self) -> usize {
        self.indent.get()
    }

    pub fn merge_pine_error(&self, mut err: PineError<Input>) {
        match err.errors.pop() {
            None => (),
            Some((input, kind)) => match kind {
                PineErrorKind::Nom(_) | PineErrorKind::Char(_) | PineErrorKind::Context(_) => self
                    .catch(PineInputError::new(
                        PineErrorKind::NonRecongnizeStmt,
                        StrRange::new(input.start, input.end),
                    )),
                _ => self.catch(PineInputError::new(
                    kind,
                    StrRange::new(input.start, input.end),
                )),
            },
        }
    }

    pub fn catch(&self, err: PineInputError) {
        self.errors.borrow_mut().push(err);
    }

    pub fn is_ok(&self) -> bool {
        self.errors.borrow().is_empty()
    }

    pub fn into_inner(&self) -> Vec<PineInputError> {
        self.errors.replace(vec![])
    }
}

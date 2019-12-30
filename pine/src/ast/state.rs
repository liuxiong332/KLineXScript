use super::error::{PineError, PineErrorKind};
use super::input::{Input, StrRange};
// use std::marker::PhantomData;
use std::cell::RefCell;

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
    // phantom: PhantomData<&'a str>,
}

impl AstState {
    pub fn new() -> AstState {
        AstState {
            errors: RefCell::new(vec![]),
            // phantom: PhantomData,
        }
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

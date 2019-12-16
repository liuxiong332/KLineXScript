use super::error::PineErrorKind;
use super::input::StrRange;
// use std::marker::PhantomData;
use std::cell::RefCell;

#[derive(Debug, PartialEq, Clone)]
pub struct PineInputError {
    code: PineErrorKind,
    range: StrRange,
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

    pub fn catch(&self, err: PineInputError) {
        self.errors.borrow_mut().push(err);
    }
}

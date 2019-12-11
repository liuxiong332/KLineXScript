use super::error::PineErrorKind;

#[derive(Debug, PartialEq)]
pub struct PineInputError<'a> {
    input: Input<'a>,
    code: PineErrorKind,
}

pub struct AstState<'a> {
    pub errors: Vec<PineInputError<'a>>,
}

impl<'a> AstState<'a> {
    pub fn new() -> AstState<'a> {
        AstState { errors: vec![] }
    }
}

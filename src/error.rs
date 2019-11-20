use nom::error::{ErrorKind, ParseError};
use nom::IResult;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum PineErrorKind {
    Nom(ErrorKind),
    Char(char),
    Context(&'static str),
    ReservedVarName,
    InvalidIdentifier(&'static str), // The identifier is not invalid
    InvalidDecimal,
    InvalidStrLiteral(&'static str),
    InvalidColorLiteral,
    InvalidFuncCallArgs(&'static str),
    IncorrectIndent,
}

#[derive(Debug, PartialEq)]
pub struct PineError<I> {
    /// list of errors accumulated by `PineError`, containing the affected
    /// part of input data, and some context
    pub errors: Vec<(I, PineErrorKind)>,
}

pub type PineResult<'a, O = &'a str> = IResult<&'a str, O, PineError<&'a str>>;

impl<I> PineError<I> {
    pub fn from_pine_kind(input: I, kind: PineErrorKind) -> Self {
        PineError {
            errors: vec![(input, kind)],
        }
    }
}

impl<I> ParseError<I> for PineError<I> {
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        PineError {
            errors: vec![(input, PineErrorKind::Nom(kind))],
        }
    }

    fn append(input: I, kind: ErrorKind, mut other: Self) -> Self {
        other.errors.push((input, PineErrorKind::Nom(kind)));
        other
    }

    fn from_char(input: I, c: char) -> Self {
        PineError {
            errors: vec![(input, PineErrorKind::Char(c))],
        }
    }

    fn add_context(input: I, ctx: &'static str, mut other: Self) -> Self {
        other.errors.push((input, PineErrorKind::Context(ctx)));
        other
    }
}

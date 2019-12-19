use super::input::Input;
use nom::error::{ErrorKind, ParseError};
use nom::Err;
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
    CannotInferType,
    NotEndOfInput,                                    // expect end of input, but not
    PrefixNoNamesAfterDot, // Prefix expressions don't contain names after dot
    LVTupleNoNames,        // left value tuple don't contain names
    BlockNoStmts,          // Block don't contain statements.
    VarNotDeclare,         // The variable not declare before used
    InvalidTypeCast { origin: String, cast: String }, // This type cast is not valid
    VarNotCallable,
    FuncCallSignatureNotMatch,
    ForbiddenDictArgsForUserFunc, // cannot call user defined function with dict arguments.
    VarNotSeriesInRef,            // The variable in reference operate is not series.
    RefIndexNotInt,               // The reference index is not int
    RefObjTypeNotObj,             // The reference type is not object
    RefKeyNotExist,               // The specific reference key not exists in the object
    CondNotBool,                  // condition is not bool
    CondExpTypesNotSame,          // The expression types of condition is not the same
    ExpNoReturn,                  //The block in expression not return anything
    ExpReturnNa,                  // The block in expression return na
    TypeMismatch,                 // The return type is not match
    ForRangeIndexNotInt,          // The index of for-range expression is not int
    UnaryTypeNotNum,              // The destination type is not num for unary operator.
    BinaryTypeNotNum,             // The destination type is not num for binary operator.
    BoolExpTypeNotBool,           // The type of bool expression is not bool
    VarHasDeclare,                // The variable in assignment has declared before.
    BreakNotInForStmt,            // Use break in non for-range statement.
    ContinueNotInForStmt,         // Use break in non for-range statement.
}

#[derive(Debug, PartialEq)]
pub struct PineError<I> {
    /// list of errors accumulated by `PineError`, containing the affected
    /// part of input data, and some context
    pub errors: Vec<(I, PineErrorKind)>,
}

pub type PineResult<'a, O = Input<'a>> = IResult<Input<'a>, O, PineError<Input<'a>>>;

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

pub fn pine_err(input: &str, kind: PineErrorKind) -> Err<PineError<&str>> {
    Err::Error(PineError::from_pine_kind(input, kind))
}

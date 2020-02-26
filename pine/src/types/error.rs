#[derive(Debug, PartialEq, Clone)]
pub enum RuntimeErr {
    NotValidParam,
    NotSupportOperator,
    NotImplement(String),
    InvalidNADeclarer,
    MissingParameters(String),     // The parameter is required.
    InvalidParameters(String),     // The parameters are invalid.
    UnrecongnizedSession,          // The session string is invalid.
    FuncCallParamNotValid(String), // The function call parameters is not valid.

    VarNotFound, // The variable not found in context

    UnknownRuntimeErr,
    // TypeMismatch(String), // The derised type is not the same as the real type.
    // TupleMismatch, // count of Left and Right side of assignment is not the same
    Continue,
    Break,

    ForRangeIndexIsNA, // The index of for-range is na
}

use crate::ast::error::PineErrorKind;
use crate::ast::input::StrRange;
use crate::ast::state::PineInputError;
use crate::helper::str_replace;
use crate::runtime::context::PineRuntimeError;
use crate::types::error::RuntimeErr;
use std::collections::HashMap;
use std::string::ToString;

static ERROR_MAP: &[(&'static str, &'static str)] = &[
    ("UnknownErr", "Unknown syntax error."),
    (
        "ReservedVarName",
        "You can't use reserved name(if, else, for, etc.) as variables.",
    ),
    (
        "InvalidIdentifier",
        "Variables must start with alphabetic or _, a combination of alphabetic or digits or _.",
    ),
    (
        "InvalidDecimal",
        "The number is invalid.",
    ),
    ("InvalidCtrlInStrLiteral", "Invalid control character."),
    ("InvalidStrLiteral", "The string literal is invalid."),
    ("InvalidColorLiteral", "The color literal is invalid."),
    ("InvalidFuncCallArgs", "Positional parameter must appear before the dictionary parameter."),
    ("IncorrectIndent", "You must indent with 4 spaces or 1 tab."),
    ("CannotInferType", "Type of variables cannot be identified from the context."),
    ("NotEndOfInput", "It should be the end of input."),
    ("PrefixNoNamesAfterDot", "The property is invalid."),
    ("LVTupleNoNames", "Left tuple value can't be empty."),
    ("TupleNotMatch", "The tuple of the left side does not match that of the right side."),
    ("BlockNoStmts", "The statement is required for a code block."),
    ("VarNotDeclare", "Before they are used, all variables have to be declared."),
    ("InvalidTypeCast", "You can't convert {} into {}."),
    ("VarNotCallable", "This variable is not callable."),
    ("FuncCallSignatureNotMatch", "The function call parameters do not match the function signature."),
    ("ForbiddenDictArgsForUserFunc", "The dictionary parameters can't be used in a user defined function."),
    ("VarNotSeriesInRef", "The data type in reference expression must be series type."),
    ("RefIndexNotInt", "The index of reference expression must be integer."),
    ("RefObjTypeNotObj", "The data type of the called expression is not object type."),
    ("RefKeyNotExist", "The object key in property getter expression doesn't exist."),
    ("CondNotBool", "The condition expression can't be converted into bool value."),
    ("CondExpTypesNotSame", "The types returned from different branches are not compatible."),
    ("ExpNoReturn", "The code block in expression that return nothing is invalid."),
    ("ExpReturnNa", "The code block in expression that return na is invalid."),
    ("TypeMismatch", "The types returned from different branches are not compatible."),
    ("ForRangeIndexNotInt", "The index in for-range expression must be integer."),
    ("UnaryTypeNotNum", "The destination type for unary expression must be numeric."),
    ("BinaryTypeNotNum", "The destination types for binary expression must be numeric or string."),
    ("BoolExpTypeNotBool", "The destination types used in bool expression must be convertible to bool."),
    ("VarHasDeclare", "You can't declare the same variable twice."),
    ("BreakNotInForStmt", "The break statement can only be used in a for-range statement."),
    ("ContinueNotInForStmt", "The continue statement can only be used in a for-range statement."),
    ("NonRecongnizeStmt", "This statement is invalid."),

    ("NotValidParam", "The parameters are invalid."),
    ("NotSupportOperator", "The operation is not available now."),
    ("NotImplement", "The operation is not implemented."),
    // ("InvalidTypeCast", "The type cast is not valid."),
    ("InvalidNADeclarer", "The variable can't be declared with na."),
    ("UnrecongnizedSession", "Unrecognized session string."),
    ("InvalidParameters", "The parameters are invalid. {}"),
    ("MissingParameters", "Missing parameters. {}"),
    ("FuncCallParamNotValid", "The parameters called in the function is invalid, {}."),
    ("VarNotFound", "The variable doesn't exist in this context."),
    ("UnknownRuntimeErr", "Unknown runtime error."),
    ("Continue", "Continue statement."),
    ("Break", "Break statement."),
    ("ForRangeIndexIsNA", "The index used in for-range statement can't be na.")
];

pub struct ErrorFormater {
    error_map: HashMap<&'static str, &'static str>,
}

impl ErrorFormater {
    pub fn new() -> ErrorFormater {
        let map: HashMap<_, _> = ERROR_MAP.iter().cloned().collect();
        ErrorFormater { error_map: map }
    }

    pub fn format_error(&self, error_code: PineErrorKind) -> String {
        match error_code {
            PineErrorKind::Nom(_)
            | PineErrorKind::Char(_)
            | PineErrorKind::Context(_)
            | PineErrorKind::UnknownErr => String::from(self.error_map["UnknownErr"]),
            PineErrorKind::ReservedVarName => String::from(self.error_map["ReservedVarName"]),
            PineErrorKind::InvalidIdentifier => String::from(self.error_map["InvalidIdentifier"]),
            PineErrorKind::InvalidDecimal => String::from(self.error_map["InvalidDecimal"]),
            PineErrorKind::InvalidCtrlInStrLiteral => {
                String::from(self.error_map["InvalidCtrlInStrLiteral"])
            }
            PineErrorKind::InvalidStrLiteral => String::from(self.error_map["InvalidStrLiteral"]),
            PineErrorKind::InvalidColorLiteral => {
                String::from(self.error_map["InvalidColorLiteral"])
            }
            PineErrorKind::InvalidFuncCallArgs => {
                String::from(self.error_map["InvalidFuncCallArgs"])
            }
            PineErrorKind::IncorrectIndent => String::from(self.error_map["IncorrectIndent"]),
            PineErrorKind::CannotInferType => String::from(self.error_map["CannotInferType"]),
            PineErrorKind::NotEndOfInput => String::from(self.error_map["NotEndOfInput"]),
            PineErrorKind::PrefixNoNamesAfterDot => {
                String::from(self.error_map["PrefixNoNamesAfterDot"])
            }
            PineErrorKind::LVTupleNoNames => String::from(self.error_map["LVTupleNoNames"]),
            PineErrorKind::TupleNotMatch => String::from(self.error_map["LVTupleNoNames"]),
            PineErrorKind::BlockNoStmts => String::from(self.error_map["BlockNoStmts"]),
            PineErrorKind::VarNotDeclare => String::from(self.error_map["VarNotDeclare"]),
            PineErrorKind::InvalidTypeCast { origin, cast } => str_replace(
                self.error_map["InvalidTypeCast"],
                vec![origin.to_string(), cast.to_string()],
            ),
            PineErrorKind::VarNotCallable => String::from(self.error_map["VarNotCallable"]),
            PineErrorKind::FuncCallSignatureNotMatch => {
                String::from(self.error_map["FuncCallSignatureNotMatch"])
            }
            PineErrorKind::ForbiddenDictArgsForUserFunc => {
                String::from(self.error_map["ForbiddenDictArgsForUserFunc"])
            }
            PineErrorKind::VarNotSeriesInRef => String::from(self.error_map["VarNotSeriesInRef"]),
            PineErrorKind::RefIndexNotInt => String::from(self.error_map["RefIndexNotInt"]),
            PineErrorKind::RefObjTypeNotObj => String::from(self.error_map["RefObjTypeNotObj"]),
            PineErrorKind::RefKeyNotExist => String::from(self.error_map["RefKeyNotExist"]),
            PineErrorKind::CondNotBool => String::from(self.error_map["CondNotBool"]),
            PineErrorKind::CondExpTypesNotSame => {
                String::from(self.error_map["CondExpTypesNotSame"])
            }
            PineErrorKind::ExpNoReturn => String::from(self.error_map["ExpNoReturn"]),
            PineErrorKind::ExpReturnNa => String::from(self.error_map["ExpReturnNa"]),
            PineErrorKind::TypeMismatch => String::from(self.error_map["TypeMismatch"]),
            PineErrorKind::ForRangeIndexNotInt => {
                String::from(self.error_map["ForRangeIndexNotInt"])
            }
            PineErrorKind::UnaryTypeNotNum => String::from(self.error_map["UnaryTypeNotNum"]),
            PineErrorKind::BinaryTypeNotNum => String::from(self.error_map["BinaryTypeNotNum"]),
            PineErrorKind::BoolExpTypeNotBool => String::from(self.error_map["BoolExpTypeNotBool"]),
            PineErrorKind::VarHasDeclare => String::from(self.error_map["VarHasDeclare"]),
            PineErrorKind::BreakNotInForStmt => String::from(self.error_map["BreakNotInForStmt"]),
            PineErrorKind::ContinueNotInForStmt => {
                String::from(self.error_map["ContinueNotInForStmt"])
            }
            PineErrorKind::NonRecongnizeStmt => String::from(self.error_map["NonRecongnizeStmt"]),
        }
    }

    pub fn format_runtime_error(&self, error_code: RuntimeErr) -> String {
        match error_code {
            RuntimeErr::NotValidParam => String::from(self.error_map["NotValidParam"]),
            RuntimeErr::NotSupportOperator => String::from(self.error_map["NotSupportOperator"]),
            RuntimeErr::NotImplement(_) => String::from(self.error_map["NotImplement"]),
            // RuntimeErr::InvalidTypeCast => String::from(self.error_map["InvalidTypeCast"]),
            RuntimeErr::InvalidNADeclarer => String::from(self.error_map["InvalidNADeclarer"]),
            RuntimeErr::UnrecongnizedSession => {
                String::from(self.error_map["UnrecongnizedSession"])
            }
            RuntimeErr::InvalidParameters(s) => {
                str_replace(self.error_map["InvalidParameters"], vec![s])
            }
            RuntimeErr::MissingParameters(s) => {
                str_replace(self.error_map["MissingParameters"], vec![s])
            }
            RuntimeErr::FuncCallParamNotValid(s) => {
                str_replace(self.error_map["FuncCallParamNotValid"], vec![s])
            }
            RuntimeErr::VarNotFound => String::from(self.error_map["VarNotFound"]),
            RuntimeErr::UnknownRuntimeErr => String::from(self.error_map["UnknownRuntimeErr"]),
            RuntimeErr::Continue => String::from(self.error_map["Continue"]),
            RuntimeErr::Break => String::from(self.error_map["Break"]),
            RuntimeErr::ForRangeIndexIsNA => String::from(self.error_map["ForRangeIndexIsNA"]),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct PineFormatError {
    pub message: String,
    pub range: StrRange,
}

impl PineFormatError {
    pub fn from_input_error(formatter: &ErrorFormater, input_err: PineInputError) -> Self {
        PineFormatError {
            range: input_err.range,
            message: formatter.format_error(input_err.code),
        }
    }

    pub fn from_runtime_error(formatter: &ErrorFormater, runtime_err: PineRuntimeError) -> Self {
        PineFormatError {
            range: runtime_err.range,
            message: formatter.format_runtime_error(runtime_err.code),
        }
    }
}

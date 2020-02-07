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
        "You cann't use reserved name for identifier.",
    ),
    (
        "InvalidIdentifier",
        "The identifier must start with alphabetic or _ and only can contain alphabetic or digits or _.",
    ),
    (
        "InvalidDecimal",
        "The decimal is not valid.",
    ),
    ("InvalidCtrlInStrLiteral", "The character after the control character is not accepted."),
    ("InvalidStrLiteral", "The string literal is not valid."),
    ("InvalidColorLiteral", "The color literal is not valid."),
    ("InvalidFuncCallArgs", "Position argument must appear before the dictionary argument."),
    ("IncorrectIndent", "You must indent with 4 spaces or 1 tab."),
    ("CannotInferType", "Can inter the variable type from the context."),
    ("NotEndOfInput", "Expect end of input, but get more characters."),
    ("PrefixNoNamesAfterDot", "The property getter expression is not valid."),
    ("LVTupleNoNames", "Left tuple value cann't be empty."),
    ("BlockNoStmts", "The block without any statements is not allowed."),
    ("VarNotDeclare", "The variable isn't declared before it's used."),
    ("InvalidTypeCast", "The {} cann't cast to {}."),
    ("VarNotCallable", "The variable is not callable."),
    ("FuncCallSignatureNotMatch", "The function call arguments aren't match the function signature."),
    ("ForbiddenDictArgsForUserFunc", "The dictionary arguments cann't be used in user funciton."),
    ("VarNotSeriesInRef", "The data type in reference expression is not series."),
    ("RefIndexNotInt", "The index of reference expression is not integer"),
    ("RefObjTypeNotObj", "The destination type in object property getter expression is not object"),
    ("RefKeyNotExist", "The key in object property getter don't exist in the object"),
    ("CondNotBool", "The condition expression cann't be converted to bool value."),
    ("CondExpTypesNotSame", "The return types from different branches are not compatible."),
    ("ExpNoReturn", "The block in expression that return nothing is not allowed."),
    ("ExpReturnNa", "The block in expression that return NA is not allowed."),
    ("TypeMismatch", "The return types from different branches aren't compatible."),
    ("ForRangeIndexNotInt", "The index in for-range expression isn't integer."),
    ("UnaryTypeNotNum", "The destination type for unary expression isn't numeric."),
    ("BinaryTypeNotNum", "The destination types for binary expression must be numeric or string."),
    ("BoolExpTypeNotBool", "The destination types for bool expression cann't be converted to bool."),
    ("VarHasDeclare", "The variable cann't be declared twice."),
    ("BreakNotInForStmt", "The break statement can only appear in for range statement."),
    ("ContinueNotInForStmt", "The continue statement can only appear in for range statement."),
    ("NonRecongnizeStmt", "This statement isn't valid pine statements."),

    ("NotValidParam", "The parameters are not valid."),
    ("NotSupportOperator", "The operation is not supported now."),
    ("NotImplement", "The operation has not implemented now."),
    ("InvalidTypeCast", "The type cast is not valid."),
    ("InvalidNADeclarer", "The variable cann't be declared with na."),
    ("FuncCallParamNotValid", "The function call's parameters is not valid, {}."),
    ("VarNotFound", "The variable doesn't exist in this cotnext."),
    ("UnknownRuntimeErr", "Unknown runtime error."),
    ("Continue", "Continue statement."),
    ("Break", "Break statement."),
    ("ForRangeIndexIsNA", "The index for for range statement cann't be na")
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
            PineErrorKind::BlockNoStmts => String::from(self.error_map["BlockNoStmts"]),
            PineErrorKind::VarNotDeclare => String::from(self.error_map["VarNotDeclare"]),
            PineErrorKind::InvalidTypeCast { origin, cast } => str_replace(
                self.error_map["CannotInferType"],
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
            RuntimeErr::InvalidTypeCast => String::from(self.error_map["InvalidTypeCast"]),
            RuntimeErr::InvalidNADeclarer => String::from(self.error_map["InvalidNADeclarer"]),
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

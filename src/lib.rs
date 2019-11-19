// #[macro_use]
extern crate nom;

#[macro_use]
extern crate lazy_static;

// mod comment_expr;
mod color;
mod comment;
mod error;
mod func_call;
mod name;
mod num;
mod op;
mod stat_expr;
mod stat_expr_types;
mod string;
mod trans;
mod utils;

use error::{PineError, PineErrorKind};
use stat_expr::statement;
use stat_expr_types::Statement;

pub fn parse_all(input: &str) -> Result<Statement, PineError<&str>> {
    match statement(input) {
        Ok((input, parsed)) => {
            if input.len() != 0 {
                Err(PineError::from_pine_kind(
                    input,
                    PineErrorKind::Context("Parse error"),
                ))
            } else {
                Ok(parsed)
            }
        }
        _ => Err(PineError::from_pine_kind(
            input,
            PineErrorKind::Context("Parse error"),
        )),
    }
}

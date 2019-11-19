// #[macro_use]
extern crate nom;
use nom::Err;

#[macro_use]
extern crate lazy_static;

// mod comment_expr;
pub mod color;
pub mod comment;
pub mod error;
pub mod func_call;
pub mod name;
pub mod num;
pub mod op;
pub mod stat_expr;
pub mod stat_expr_types;
pub mod string;
pub mod trans;
pub mod utils;

use error::{PineError, PineErrorKind};
use stat_expr::block;
use stat_expr_types::Block;

pub fn parse_all(input: &str) -> Result<Block, PineError<&str>> {
    match block(input) {
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
        Err(Err::Error(pine_error)) => Err(pine_error),
        _ => Err(PineError::from_pine_kind(
            input,
            PineErrorKind::Context("Parse error"),
        )),
    }
}

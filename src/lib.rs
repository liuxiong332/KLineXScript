// #[macro_use]
extern crate nom;
use nom::Err;

#[macro_use]
extern crate lazy_static;

pub mod ast;
pub mod runtime;
pub mod types;

use ast::error::{PineError, PineErrorKind};
use ast::stat_expr::block;
use ast::stat_expr_types::Block;

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

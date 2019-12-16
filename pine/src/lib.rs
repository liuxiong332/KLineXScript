// #[macro_use]
extern crate nom;
use nom::Err;

#[macro_use]
extern crate lazy_static;

pub mod ast;
pub mod libs;
pub mod runtime;
pub mod types;

use ast::error::{PineError, PineErrorKind};
use ast::input::{Input, Position};
use ast::stat_expr::block;
use ast::stat_expr_types::Block;
use ast::state::AstState;

pub fn parse_all(in_str: &str) -> Result<Block, PineError<Input>> {
    let input = Input::new(in_str, Position::new(0, 0), Position::max());
    match block(input.clone(), &AstState::new()) {
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

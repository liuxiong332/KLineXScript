pub mod context;
pub mod data_src;
pub mod exp;
pub mod function;
pub mod op;
pub mod statement;

use crate::ast::stat_expr_types::Block;
use crate::types::{ConvertErr, PineType};
use context::{Context, ContextType, Ctx, Runner};
use std::collections::HashMap;

pub fn run<'a>(
    blk: &'a Block<'a>,
    vars: HashMap<&'a str, Box<dyn PineType<'a> + 'a>>,
) -> Result<(), ConvertErr> {
    let mut context = Context::new(None, ContextType::Normal);
    for (k, v) in vars.into_iter() {
        context.create_var(k, v);
    }
    blk.run(&mut context)?;
    Ok(())
}

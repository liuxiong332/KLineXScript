pub mod context;
pub mod ctxid_parser;
pub mod data_src;
pub mod exp;
pub mod function;
pub mod op;
pub mod statement;

use crate::ast::stat_expr_types::Block;
use crate::types::{PineRef, RuntimeErr};
use context::{Context, ContextType, Ctx, Runner, VarOperate};
use std::collections::HashMap;
use std::rc::Rc;

pub fn run<'a>(blk: &'a Block<'a>, vars: HashMap<&'a str, PineRef<'a>>) -> Result<(), RuntimeErr> {
    let context = Rc::new(Context::new(None, ContextType::Normal));
    for (k, v) in vars.into_iter() {
        context.create_var(k, v);
    }
    blk.run(&context)?;
    Ok(())
}

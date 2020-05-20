pub mod any_series;
pub mod context;
pub mod data_src;
pub mod error_format;
pub mod exp;
pub mod function;
pub mod instance_caller;
pub mod op;
pub mod output;
pub mod runtime_convert;
pub mod statement;

pub use any_series::*;
pub use context::*;
pub use data_src::*;
pub use error_format::*;
pub use output::*;
// use crate::ast::stat_expr_types::Block;
// use crate::types::PineRef;
// use context::{Context, ContextType, PineRuntimeError, Runner, VarOperate};
// use std::collections::HashMap;

// pub fn run<'a>(
//     blk: &'a Block<'a>,
//     vars: HashMap<&'a str, PineRef<'a>>,
// ) -> Result<(), PineRuntimeError> {
//     let mut context = Context::new(None, ContextType::Normal);
//     for (k, v) in vars.into_iter() {
//         context.create_var(k, v);
//     }
//     blk.run(&mut context)?;
//     Ok(())
// }

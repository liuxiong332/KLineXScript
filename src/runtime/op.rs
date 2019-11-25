use super::context::{Context, Runner};
use super::exp::Exp;
use crate::ast::op::UnaryOp;
use crate::types::{
    downcast, ConvertErr, DataType as FirstType, Float, Int, Negative, PineType, SecondType,
};

pub fn unary_op_run<'a>(
    op: UnaryOp,
    exp: Box<Exp<'a>>,
    context: &mut Context<'a>,
) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
    match op {
        UnaryOp::Plus => exp.run(context),
        UnaryOp::Minus => {
            let val = exp.run(context)?;
            match val.get_type() {
                (FirstType::Int, SecondType::Simple) => {
                    Ok(Box::new(downcast::<Int>(val).unwrap().negative()) as Box<dyn PineType>)
                }
                (FirstType::Float, SecondType::Simple) => {
                    Ok(Box::new(downcast::<Float>(val).unwrap().negative()) as Box<dyn PineType>)
                }
                _ => Err(ConvertErr::NotSupportOperator),
            }
        }
        // UnaryOp::BoolNot => {
        //     let val = exp.run(context)?;
        //     let Bool::implicity_from(val)?;
        // }
        _ => Err(ConvertErr::NotSupportOperator),
    }
}

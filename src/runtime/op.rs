use super::context::{Context, Runner};
use super::exp::Exp;
use crate::ast::op::{BinaryOp, UnaryOp};
use crate::types::{
    downcast, Arithmetic, Bool, ConvertErr, DataType as FirstType, Float, Int, Negative, PineFrom,
    PineType, SecondType,
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
        UnaryOp::BoolNot => {
            let val = exp.run(context)?;
            let bool_val = Bool::implicity_from(val)?;
            match *bool_val {
                true => Ok(Box::new(false)),
                false => Ok(Box::new(true)),
            }
        }
    }
}

fn biarith<'a, D: Arithmetic + 'a>(op: BinaryOp, d1: D, d2: D) -> D {
    match op {
        BinaryOp::Plus => d1.add(d2),
        BinaryOp::Minus => d1.minus(d2),
        BinaryOp::Mul => d1.mul(d2),
        BinaryOp::Div => d1.div(d2),
        BinaryOp::Mod => d1.rem(d2),
        _ => unreachable!(),
    }
}

pub fn binary_op_run<'a>(
    op: BinaryOp,
    exp1: Box<Exp<'a>>,
    exp2: Box<Exp<'a>>,
    context: &mut Context<'a>,
) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
    match op {
        BinaryOp::Plus | BinaryOp::Minus | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
            let val1 = exp1.run(context)?;
            let val2 = exp2.run(context)?;
            match (op, val1.get_type(), val2.get_type()) {
                (op, (FirstType::Float, SecondType::Simple), _)
                | (op, _, (FirstType::Float, SecondType::Simple)) => {
                    let f1 = *Float::implicity_from(val1)?;
                    let f2 = *Float::implicity_from(val2)?;
                    Ok(Box::new(biarith(op, f1, f2)))
                }
                (op, (FirstType::Int, SecondType::Simple), _)
                | (op, _, (FirstType::Int, SecondType::Simple)) => {
                    let d1 = *Int::implicity_from(val1)?;
                    let d2 = *Int::implicity_from(val2)?;
                    Ok(Box::new(biarith(op, d1, d2)))
                }
                (BinaryOp::Plus, (FirstType::String, SecondType::Simple), _)
                | (_, _, (FirstType::String, SecondType::Simple)) => Ok(Box::new(
                    *(String::implicity_from(val1)?) + &*(String::implicity_from(val2)?),
                )),
                _ => Err(ConvertErr::NotSupportOperator),
            }
        }
        _ => Err(ConvertErr::NotSupportOperator),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::num::Numeral;
    use std::collections::HashMap;

    #[test]
    fn unary_op_test() {
        let mut context = Context::new(HashMap::new());
        assert_eq!(
            downcast::<Int>(
                unary_op_run(
                    UnaryOp::Minus,
                    Box::new(Exp::Num(Numeral::Int(1))),
                    &mut context,
                )
                .unwrap()
            ),
            Ok(Box::new(Some(-1)))
        );

        assert_eq!(
            downcast::<Bool>(
                unary_op_run(UnaryOp::BoolNot, Box::new(Exp::Bool(true)), &mut context,).unwrap()
            ),
            Ok(Box::new(false))
        );
    }
}

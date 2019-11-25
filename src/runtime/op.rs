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
fn bi_operate<'a, D: Arithmetic + PartialOrd + PartialEq + PineType<'a> + 'a>(
    op: BinaryOp,
    d1: Box<D>,
    d2: Box<D>,
) -> Box<dyn PineType<'a> + 'a> {
    match op {
        BinaryOp::Plus => Box::new(d1.add(*d2)),
        BinaryOp::Minus => Box::new(d1.minus(*d2)),
        BinaryOp::Mul => Box::new(d1.mul(*d2)),
        BinaryOp::Div => Box::new(d1.div(*d2)),
        BinaryOp::Mod => Box::new(d1.rem(*d2)),
        BinaryOp::Eq => Box::new(*d1 == *d2),
        BinaryOp::Neq => Box::new(*d1 != *d2),
        BinaryOp::Lt => Box::new(*d1 < *d2),
        BinaryOp::Leq => Box::new(*d1 <= *d2),
        BinaryOp::Gt => Box::new(*d1 > *d2),
        BinaryOp::Geq => Box::new(*d1 >= *d2),
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
        BinaryOp::BoolAnd => {
            let val1 = exp1.run(context)?;
            let bval1 = Bool::implicity_from(val1)?;
            if *bval1 == false {
                return Ok(Box::new(false));
            }
            let val2 = exp2.run(context)?;
            let bval2 = Bool::implicity_from(val2)?;
            Ok(bval2)
        }
        BinaryOp::BoolOr => {
            let val1 = exp1.run(context)?;
            let bval1 = Bool::implicity_from(val1)?;
            if *bval1 == true {
                return Ok(Box::new(true));
            }
            let val2 = exp2.run(context)?;
            let bval2 = Bool::implicity_from(val2)?;
            Ok(bval2)
        }
        _ => {
            let val1 = exp1.run(context)?;
            let val2 = exp2.run(context)?;
            match (op, val1.get_type(), val2.get_type()) {
                (op, (FirstType::Float, SecondType::Simple), _)
                | (op, _, (FirstType::Float, SecondType::Simple)) => {
                    let f1 = Float::implicity_from(val1)?;
                    let f2 = Float::implicity_from(val2)?;
                    Ok(bi_operate(op, f1, f2))
                }
                (op, (FirstType::Int, SecondType::Simple), _)
                | (op, _, (FirstType::Int, SecondType::Simple)) => {
                    let d1 = Int::implicity_from(val1)?;
                    let d2 = Int::implicity_from(val2)?;
                    Ok(bi_operate(op, d1, d2))
                }
                (BinaryOp::Plus, (FirstType::String, SecondType::Simple), _)
                | (_, _, (FirstType::String, SecondType::Simple)) => Ok(Box::new(
                    *(String::implicity_from(val1)?) + &*(String::implicity_from(val2)?),
                )),
                _ => Err(ConvertErr::NotSupportOperator),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::num::Numeral;
    use crate::types::PineStaticType;
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

    fn int_exp<'a>(int: i32) -> Exp<'a> {
        Exp::Num(Numeral::Int(int))
    }

    fn float_exp<'a>(f: f64) -> Exp<'a> {
        Exp::Num(Numeral::Float(f))
    }

    fn biop_runner<'a, D: PineStaticType>(
        op: BinaryOp,
        v1: Exp<'a>,
        v2: Exp<'a>,
    ) -> Result<Box<D>, ConvertErr> {
        let mut context = Context::new(HashMap::new());
        downcast::<D>(binary_op_run(op, Box::new(v1), Box::new(v2), &mut context).unwrap())
    }

    #[test]
    fn binary_op_test() {
        assert_eq!(
            biop_runner(BinaryOp::Plus, int_exp(1), int_exp(2)),
            Ok(Box::new(Some(3)))
        );
        assert_eq!(
            biop_runner(BinaryOp::Plus, float_exp(1f64), float_exp(2f64)),
            Ok(Box::new(Some(3f64)))
        );
        assert_eq!(
            biop_runner(BinaryOp::Plus, int_exp(1i32), float_exp(2f64)),
            Ok(Box::new(Some(3f64)))
        );
        assert_eq!(
            biop_runner(
                BinaryOp::Plus,
                Exp::Str(String::from("hello")),
                Exp::Str(String::from("world"))
            ),
            Ok(Box::new(String::from("helloworld")))
        );

        assert_eq!(
            biop_runner(BinaryOp::Minus, int_exp(2), int_exp(1)),
            Ok(Box::new(Some(1)))
        );
        assert_eq!(
            biop_runner(BinaryOp::Minus, float_exp(2f64), float_exp(1f64)),
            Ok(Box::new(Some(1f64)))
        );
        assert_eq!(
            biop_runner(BinaryOp::Mul, int_exp(2i32), int_exp(3i32)),
            Ok(Box::new(Some(6i32)))
        );
        assert_eq!(
            biop_runner(BinaryOp::Mul, float_exp(2f64), float_exp(3f64)),
            Ok(Box::new(Some(6f64)))
        );
        assert_eq!(
            biop_runner(BinaryOp::Div, int_exp(5i32), int_exp(2i32)),
            Ok(Box::new(Some(2i32)))
        );
        assert_eq!(
            biop_runner(BinaryOp::Div, float_exp(5f64), float_exp(2f64)),
            Ok(Box::new(Some(2.5f64)))
        );

        assert_eq!(
            biop_runner(BinaryOp::Mod, int_exp(12i32), int_exp(5i32)),
            Ok(Box::new(Some(2i32)))
        );
        assert_eq!(
            biop_runner(BinaryOp::Mod, float_exp(12f64), float_exp(5f64)),
            Ok(Box::new(Some(2f64)))
        );
    }

    #[test]
    fn logic_op_test() {
        assert_eq!(
            biop_runner(BinaryOp::Eq, int_exp(2), int_exp(1)),
            Ok(Box::new(false))
        );
        assert_eq!(
            biop_runner(BinaryOp::Neq, int_exp(2), int_exp(1)),
            Ok(Box::new(true))
        );
        assert_eq!(
            biop_runner(BinaryOp::Gt, int_exp(2), int_exp(1)),
            Ok(Box::new(true))
        );
        assert_eq!(
            biop_runner(BinaryOp::Geq, int_exp(2), int_exp(1)),
            Ok(Box::new(true))
        );
        assert_eq!(
            biop_runner(BinaryOp::Lt, int_exp(2), int_exp(1)),
            Ok(Box::new(false))
        );
        assert_eq!(
            biop_runner(BinaryOp::Leq, int_exp(2), int_exp(1)),
            Ok(Box::new(false))
        );
    }
}

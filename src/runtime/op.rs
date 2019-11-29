use super::context::{Ctx, RVRunner};
use super::exp::Exp;
use crate::ast::op::{BinaryOp, UnaryOp};
use crate::types::{
    downcast, Arithmetic, Bool, ConvertErr, DataType as FirstType, Float, Int, Negative, PineFrom,
    PineType, SecondType, Series,
};

pub fn unary_op_run<'a>(
    op: &UnaryOp,
    exp: &'a Box<Exp<'a>>,
    context: &mut (dyn Ctx<'a>),
) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
    match op {
        UnaryOp::Plus => exp.rv_run(context),
        UnaryOp::Minus => {
            let val = exp.rv_run(context)?;
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
            let val = exp.rv_run(context)?;
            let bool_val = Bool::implicity_from(val)?;
            match *bool_val {
                true => Ok(Box::new(false)),
                false => Ok(Box::new(true)),
            }
        }
    }
}
fn bi_operate<'a, D: Arithmetic + PartialOrd + PartialEq + PineType<'a> + 'a>(
    op: &BinaryOp,
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

pub fn binary_op_run<'a, 'b>(
    op: &BinaryOp,
    exp1: &'a Box<Exp<'a>>,
    exp2: &'a Box<Exp<'a>>,
    context: &mut (dyn 'b + Ctx<'a>),
) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
    match op {
        BinaryOp::BoolAnd => {
            let val1 = exp1.rv_run(context)?;
            let bval1 = Bool::implicity_from(val1)?;
            if *bval1 == false {
                return Ok(Box::new(false));
            }
            let val2 = exp2.rv_run(context)?;
            let bval2 = Bool::implicity_from(val2)?;
            Ok(bval2)
        }
        BinaryOp::BoolOr => {
            let val1 = exp1.rv_run(context)?;
            let bval1 = Bool::implicity_from(val1)?;
            if *bval1 == true {
                return Ok(Box::new(true));
            }
            let val2 = exp2.rv_run(context)?;
            let bval2 = Bool::implicity_from(val2)?;
            Ok(bval2)
        }
        _ => {
            let val1 = exp1.rv_run(context)?;
            let val2 = exp2.rv_run(context)?;
            match (op, val1.get_type(), val2.get_type()) {
                (op, (FirstType::Float, SecondType::Simple), _)
                | (op, _, (FirstType::Float, SecondType::Simple)) => {
                    let f1 = Float::implicity_from(val1)?;
                    let f2 = Float::implicity_from(val2)?;
                    Ok(bi_operate(op, f1, f2))
                }
                (op, (FirstType::Float, SecondType::Series), _)
                | (op, _, (FirstType::Float, SecondType::Series)) => {
                    let f1: Box<Series<Float>> = Series::implicity_from(val1)?;
                    let f2: Box<Series<Float>> = Series::implicity_from(val2)?;
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
    use crate::ast::name::VarName;
    use crate::ast::num::Numeral;
    use crate::runtime::context::{Context, ContextType};
    use crate::types::PineStaticType;

    #[test]
    fn unary_op_test() {
        let exp = Box::new(Exp::Num(Numeral::Int(1)));
        let exp2 = Box::new(Exp::Bool(true));

        let mut context = Context::new(None, ContextType::Normal);
        assert_eq!(
            downcast::<Int>(unary_op_run(&UnaryOp::Minus, &exp, &mut context,).unwrap()),
            Ok(Box::new(Some(-1)))
        );

        assert_eq!(
            downcast::<Bool>(unary_op_run(&UnaryOp::BoolNot, &exp2, &mut context,).unwrap()),
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
        let mut context = Context::new(None, ContextType::Normal);
        downcast::<D>(binary_op_run(&op, &Box::new(v1), &Box::new(v2), &mut context).unwrap())
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

    #[test]
    fn logic_test() {
        assert_eq!(
            biop_runner(BinaryOp::BoolAnd, Exp::Bool(false), Exp::Bool(true)),
            Ok(Box::new(false))
        );
        assert_eq!(
            biop_runner(BinaryOp::BoolAnd, Exp::Bool(false), Exp::Bool(false)),
            Ok(Box::new(false))
        );
        assert_eq!(
            biop_runner(BinaryOp::BoolOr, Exp::Bool(false), Exp::Bool(true)),
            Ok(Box::new(true))
        );
        assert_eq!(
            biop_runner(BinaryOp::BoolOr, Exp::Bool(false), Exp::Bool(false)),
            Ok(Box::new(false))
        );
    }

    fn biop_rv_runner<'a, D: PineStaticType>(
        op: BinaryOp,
        v1: Exp<'a>,
        v2: Exp<'a>,
    ) -> Result<Box<D>, ConvertErr> {
        let mut context = Context::new(None, ContextType::Normal);
        context.create_var("arg1", Box::new(Some(4)));
        context.create_var("arg2", Box::new(Some(2)));

        downcast::<D>(binary_op_run(&op, &Box::new(v1), &Box::new(v2), &mut context).unwrap())
    }

    fn var_exp<'a>(var: &'a str) -> Exp<'a> {
        Exp::VarName(VarName(var))
    }

    #[test]
    fn rv_op_test() {
        assert_eq!(
            biop_rv_runner(BinaryOp::Plus, var_exp("arg1"), var_exp("arg2")),
            Ok(Box::new(Some(6)))
        );
        assert_eq!(
            biop_rv_runner(BinaryOp::Minus, var_exp("arg1"), var_exp("arg2")),
            Ok(Box::new(Some(2)))
        );
        assert_eq!(
            biop_rv_runner(BinaryOp::Mul, var_exp("arg1"), var_exp("arg2")),
            Ok(Box::new(Some(8)))
        );
        assert_eq!(
            biop_rv_runner(BinaryOp::Div, var_exp("arg1"), var_exp("arg2")),
            Ok(Box::new(Some(2)))
        );
        assert_eq!(
            biop_rv_runner(BinaryOp::Mod, var_exp("arg1"), var_exp("arg2")),
            Ok(Box::new(Some(0)))
        );

        assert_eq!(
            biop_rv_runner(BinaryOp::Eq, var_exp("arg1"), var_exp("arg2")),
            Ok(Box::new(false))
        );
        assert_eq!(
            biop_rv_runner(BinaryOp::Neq, var_exp("arg1"), var_exp("arg2")),
            Ok(Box::new(true))
        );
        assert_eq!(
            biop_rv_runner(BinaryOp::Lt, var_exp("arg1"), var_exp("arg2")),
            Ok(Box::new(false))
        );
        assert_eq!(
            biop_rv_runner(BinaryOp::Leq, var_exp("arg1"), var_exp("arg2")),
            Ok(Box::new(false))
        );
        assert_eq!(
            biop_rv_runner(BinaryOp::Gt, var_exp("arg1"), var_exp("arg2")),
            Ok(Box::new(true))
        );
        assert_eq!(
            biop_rv_runner(BinaryOp::Geq, var_exp("arg1"), var_exp("arg2")),
            Ok(Box::new(true))
        );
        assert_eq!(
            biop_rv_runner(BinaryOp::BoolAnd, var_exp("arg1"), var_exp("arg2")),
            Ok(Box::new(true))
        );
        assert_eq!(
            biop_rv_runner(BinaryOp::BoolOr, var_exp("arg1"), var_exp("arg2")),
            Ok(Box::new(true))
        );
    }
}

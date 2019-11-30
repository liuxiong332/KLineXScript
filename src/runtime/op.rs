use super::context::{Ctx, RVRunner};
use super::exp::Exp;
use crate::ast::op::{BinaryOp, UnaryOp};
use crate::types::{
    downcast_pf, Arithmetic, Bool, DataType as FirstType, Float, Int, Negative, PineFrom, PineRef,
    PineType, RefData, RuntimeErr, SecondType, Series,
};
use std::fmt::Debug;

pub fn unary_op_run<'a>(
    op: &UnaryOp,
    exp: &'a Box<Exp<'a>>,
    context: &mut (dyn Ctx<'a>),
) -> Result<PineRef<'a>, RuntimeErr> {
    match op {
        UnaryOp::Plus => exp.rv_run(context),
        UnaryOp::Minus => {
            let val = exp.rv_run(context)?;
            match val.get_type() {
                (FirstType::Int, SecondType::Simple) => Ok(PineRef::new_box(
                    downcast_pf::<Int>(val).unwrap().negative(),
                )),
                (FirstType::Float, SecondType::Simple) => Ok(PineRef::new_box(
                    downcast_pf::<Float>(val).unwrap().negative(),
                )),
                _ => Err(RuntimeErr::NotSupportOperator),
            }
        }
        UnaryOp::BoolNot => {
            let val = exp.rv_run(context)?;
            let bool_val = Bool::implicity_from(val)?;
            match *bool_val {
                true => Ok(PineRef::new_box(false)),
                false => Ok(PineRef::new_box(true)),
            }
        }
    }
}

fn bi_operate<'a, D>(op: &BinaryOp, d1: RefData<D>, d2: RefData<D>) -> PineRef<'a>
where
    D: Arithmetic + PartialOrd + PartialEq + Debug + Clone + PineType<'a> + 'a,
{
    match op {
        BinaryOp::Plus => PineRef::new(d1.into_inner().add(d2.into_inner())),
        BinaryOp::Minus => PineRef::new(d1.into_inner().minus(d2.into_inner())),
        BinaryOp::Mul => PineRef::new(d1.into_inner().mul(d2.into_inner())),
        BinaryOp::Div => PineRef::new(d1.into_inner().div(d2.into_inner())),
        BinaryOp::Mod => PineRef::new(d1.into_inner().rem(d2.into_inner())),
        BinaryOp::Eq => PineRef::new(*d1 == *d2),
        BinaryOp::Neq => PineRef::new(*d1 != *d2),
        BinaryOp::Lt => PineRef::new(*d1 < *d2),
        BinaryOp::Leq => PineRef::new(*d1 <= *d2),
        BinaryOp::Gt => PineRef::new(*d1 > *d2),
        BinaryOp::Geq => PineRef::new(*d1 >= *d2),
        _ => unreachable!(),
    }
}

pub fn binary_op_run<'a, 'b>(
    op: &BinaryOp,
    exp1: &'a Box<Exp<'a>>,
    exp2: &'a Box<Exp<'a>>,
    context: &mut (dyn 'b + Ctx<'a>),
) -> Result<PineRef<'a>, RuntimeErr> {
    match op {
        BinaryOp::BoolAnd => {
            let val1 = exp1.rv_run(context)?;
            let bval1 = Bool::implicity_from(val1)?;
            if *bval1 == false {
                return Ok(PineRef::new_box(false));
            }
            let val2 = exp2.rv_run(context)?;
            let bval2 = Bool::implicity_from(val2)?;
            Ok(bval2.into_pf())
        }
        BinaryOp::BoolOr => {
            let val1 = exp1.rv_run(context)?;
            let bval1 = Bool::implicity_from(val1)?;
            if *bval1 == true {
                return Ok(PineRef::new_box(true));
            }
            let val2 = exp2.rv_run(context)?;
            let bval2 = Bool::implicity_from(val2)?;
            Ok(bval2.into_pf())
        }
        _ => {
            let val1 = exp1.rv_run(context)?;
            let val2 = exp2.rv_run(context)?;
            match (op, val1.get_type(), val2.get_type()) {
                (op, (FirstType::Float, SecondType::Series), _)
                | (op, _, (FirstType::Float, SecondType::Series)) => {
                    let f1: RefData<Series<Float>> = Series::implicity_from(val1)?;
                    let f2: RefData<Series<Float>> = Series::implicity_from(val2)?;
                    Ok(bi_operate(op, f1, f2))
                }
                (op, (FirstType::Int, SecondType::Series), _)
                | (op, _, (FirstType::Int, SecondType::Series)) => {
                    let d1: RefData<Series<Int>> = Series::implicity_from(val1)?;
                    let d2: RefData<Series<Int>> = Series::implicity_from(val2)?;
                    Ok(bi_operate(op, d1, d2))
                }
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
                | (_, _, (FirstType::String, SecondType::Simple)) => Ok(PineRef::new_rc(
                    String::implicity_from(val1)?.into_inner()
                        + &(String::implicity_from(val2)?.into_inner()),
                )),
                _ => Err(RuntimeErr::NotSupportOperator),
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
            downcast_pf::<Int>(unary_op_run(&UnaryOp::Minus, &exp, &mut context,).unwrap()),
            Ok(RefData::new_box(Some(-1)))
        );

        assert_eq!(
            downcast_pf::<Bool>(unary_op_run(&UnaryOp::BoolNot, &exp2, &mut context,).unwrap()),
            Ok(RefData::new_box(false))
        );
    }

    fn int_exp<'a>(int: i32) -> Exp<'a> {
        Exp::Num(Numeral::Int(int))
    }

    fn float_exp<'a>(f: f64) -> Exp<'a> {
        Exp::Num(Numeral::Float(f))
    }

    fn biop_runner<'a, D: PineStaticType + PartialEq + Debug>(
        op: BinaryOp,
        v1: Exp<'a>,
        v2: Exp<'a>,
    ) -> Result<RefData<D>, RuntimeErr> {
        let mut context = Context::new(None, ContextType::Normal);
        downcast_pf::<D>(binary_op_run(&op, &Box::new(v1), &Box::new(v2), &mut context).unwrap())
    }

    #[test]
    fn binary_op_test() {
        assert_eq!(
            biop_runner(BinaryOp::Plus, int_exp(1), int_exp(2)),
            Ok(RefData::new_box(Some(3)))
        );
        assert_eq!(
            biop_runner(BinaryOp::Plus, float_exp(1f64), float_exp(2f64)),
            Ok(RefData::new_box(Some(3f64)))
        );
        assert_eq!(
            biop_runner(BinaryOp::Plus, int_exp(1i32), float_exp(2f64)),
            Ok(RefData::new_box(Some(3f64)))
        );
        assert_eq!(
            biop_runner(
                BinaryOp::Plus,
                Exp::Str(String::from("hello")),
                Exp::Str(String::from("world"))
            ),
            Ok(RefData::new(String::from("helloworld")))
        );

        assert_eq!(
            biop_runner(BinaryOp::Minus, int_exp(2), int_exp(1)),
            Ok(RefData::new_box(Some(1)))
        );
        assert_eq!(
            biop_runner(BinaryOp::Minus, float_exp(2f64), float_exp(1f64)),
            Ok(RefData::new_box(Some(1f64)))
        );
        assert_eq!(
            biop_runner(BinaryOp::Mul, int_exp(2i32), int_exp(3i32)),
            Ok(RefData::new_box(Some(6i32)))
        );
        assert_eq!(
            biop_runner(BinaryOp::Mul, float_exp(2f64), float_exp(3f64)),
            Ok(RefData::new_box(Some(6f64)))
        );
        assert_eq!(
            biop_runner(BinaryOp::Div, int_exp(5i32), int_exp(2i32)),
            Ok(RefData::new_box(Some(2i32)))
        );
        assert_eq!(
            biop_runner(BinaryOp::Div, float_exp(5f64), float_exp(2f64)),
            Ok(RefData::new_box(Some(2.5f64)))
        );

        assert_eq!(
            biop_runner(BinaryOp::Mod, int_exp(12i32), int_exp(5i32)),
            Ok(RefData::new_box(Some(2i32)))
        );
        assert_eq!(
            biop_runner(BinaryOp::Mod, float_exp(12f64), float_exp(5f64)),
            Ok(RefData::new_box(Some(2f64)))
        );
    }

    #[test]
    fn logic_op_test() {
        assert_eq!(
            biop_runner(BinaryOp::Eq, int_exp(2), int_exp(1)),
            Ok(RefData::new_box(false))
        );
        assert_eq!(
            biop_runner(BinaryOp::Neq, int_exp(2), int_exp(1)),
            Ok(RefData::new_box(true))
        );
        assert_eq!(
            biop_runner(BinaryOp::Gt, int_exp(2), int_exp(1)),
            Ok(RefData::new_box(true))
        );
        assert_eq!(
            biop_runner(BinaryOp::Geq, int_exp(2), int_exp(1)),
            Ok(RefData::new_box(true))
        );
        assert_eq!(
            biop_runner(BinaryOp::Lt, int_exp(2), int_exp(1)),
            Ok(RefData::new_box(false))
        );
        assert_eq!(
            biop_runner(BinaryOp::Leq, int_exp(2), int_exp(1)),
            Ok(RefData::new_box(false))
        );
    }

    #[test]
    fn logic_test() {
        assert_eq!(
            biop_runner(BinaryOp::BoolAnd, Exp::Bool(false), Exp::Bool(true)),
            Ok(RefData::new_box(false))
        );
        assert_eq!(
            biop_runner(BinaryOp::BoolAnd, Exp::Bool(false), Exp::Bool(false)),
            Ok(RefData::new_box(false))
        );
        assert_eq!(
            biop_runner(BinaryOp::BoolOr, Exp::Bool(false), Exp::Bool(true)),
            Ok(RefData::new_box(true))
        );
        assert_eq!(
            biop_runner(BinaryOp::BoolOr, Exp::Bool(false), Exp::Bool(false)),
            Ok(RefData::new_box(false))
        );
    }

    fn biop_rv_runner<'a, D: PineStaticType + PartialEq + Debug>(
        op: BinaryOp,
        v1: Exp<'a>,
        v2: Exp<'a>,
    ) -> Result<RefData<D>, RuntimeErr> {
        let mut context = Context::new(None, ContextType::Normal);
        context.create_var("arg1", PineRef::new_box(Some(4)));
        context.create_var("arg2", PineRef::new_box(Some(2)));

        downcast_pf::<D>(binary_op_run(&op, &Box::new(v1), &Box::new(v2), &mut context).unwrap())
    }

    fn var_exp<'a>(var: &'a str) -> Exp<'a> {
        Exp::VarName(VarName(var))
    }

    #[test]
    fn rv_op_test() {
        assert_eq!(
            biop_rv_runner(BinaryOp::Plus, var_exp("arg1"), var_exp("arg2")),
            Ok(RefData::new_box(Some(6)))
        );
        assert_eq!(
            biop_rv_runner(BinaryOp::Minus, var_exp("arg1"), var_exp("arg2")),
            Ok(RefData::new_box(Some(2)))
        );
        assert_eq!(
            biop_rv_runner(BinaryOp::Mul, var_exp("arg1"), var_exp("arg2")),
            Ok(RefData::new_box(Some(8)))
        );
        assert_eq!(
            biop_rv_runner(BinaryOp::Div, var_exp("arg1"), var_exp("arg2")),
            Ok(RefData::new_box(Some(2)))
        );
        assert_eq!(
            biop_rv_runner(BinaryOp::Mod, var_exp("arg1"), var_exp("arg2")),
            Ok(RefData::new_box(Some(0)))
        );

        assert_eq!(
            biop_rv_runner(BinaryOp::Eq, var_exp("arg1"), var_exp("arg2")),
            Ok(RefData::new_box(false))
        );
        assert_eq!(
            biop_rv_runner(BinaryOp::Neq, var_exp("arg1"), var_exp("arg2")),
            Ok(RefData::new_box(true))
        );
        assert_eq!(
            biop_rv_runner(BinaryOp::Lt, var_exp("arg1"), var_exp("arg2")),
            Ok(RefData::new_box(false))
        );
        assert_eq!(
            biop_rv_runner(BinaryOp::Leq, var_exp("arg1"), var_exp("arg2")),
            Ok(RefData::new_box(false))
        );
        assert_eq!(
            biop_rv_runner(BinaryOp::Gt, var_exp("arg1"), var_exp("arg2")),
            Ok(RefData::new_box(true))
        );
        assert_eq!(
            biop_rv_runner(BinaryOp::Geq, var_exp("arg1"), var_exp("arg2")),
            Ok(RefData::new_box(true))
        );
        assert_eq!(
            biop_rv_runner(BinaryOp::BoolAnd, var_exp("arg1"), var_exp("arg2")),
            Ok(RefData::new_box(true))
        );
        assert_eq!(
            biop_rv_runner(BinaryOp::BoolOr, var_exp("arg1"), var_exp("arg2")),
            Ok(RefData::new_box(true))
        );
    }
}

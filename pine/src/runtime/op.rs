use super::context::{Ctx, PineRuntimeError, RVRunner};
use crate::ast::op::{BinaryOp, UnaryOp};
use crate::ast::stat_expr_types::{BinaryExp, UnaryExp};
use crate::types::{
    downcast_pf, Arithmetic, Bool, DataType as FirstType, Float, Int, Negative, PineFrom, PineRef,
    PineType, RefData, RuntimeErr, SecondType, Series,
};
use std::fmt::Debug;

pub fn unary_op_run<'a>(
    unary_exp: &'a UnaryExp<'a>,
    context: &mut (dyn Ctx<'a>),
) -> Result<PineRef<'a>, PineRuntimeError> {
    match unary_exp.op {
        UnaryOp::Plus => unary_exp.exp.rv_run(context),
        UnaryOp::Minus => {
            let val = unary_exp.exp.rv_run(context)?;
            // minus destination type can be int, float, series(int) or series(float)
            match val.get_type() {
                (FirstType::Int, SecondType::Simple) => Ok(PineRef::new_box(
                    downcast_pf::<Int>(val).unwrap().negative(),
                )),
                (FirstType::Int, SecondType::Series) => {
                    let s = downcast_pf::<Series<Int>>(val).unwrap();
                    Ok(PineRef::new(Series::from(s.get_current().negative())))
                }
                (FirstType::Float, SecondType::Simple) => Ok(PineRef::new_box(
                    downcast_pf::<Float>(val).unwrap().negative(),
                )),
                (FirstType::Float, SecondType::Series) => {
                    let s = downcast_pf::<Series<Float>>(val).unwrap();
                    Ok(PineRef::new(Series::from(s.get_current().negative())))
                }
                _ => unreachable!(),
            }
        }
        UnaryOp::BoolNot => {
            let val = unary_exp.exp.rv_run(context)?;
            let bool_val = Bool::implicity_from(val).unwrap();
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
    binary_exp: &'a BinaryExp,
    context: &mut (dyn 'b + Ctx<'a>),
) -> Result<PineRef<'a>, PineRuntimeError> {
    match binary_exp.op {
        BinaryOp::BoolAnd => {
            let val1 = binary_exp.exp1.rv_run(context)?;
            let bval1 = Bool::implicity_from(val1).unwrap();
            if *bval1 == false {
                return Ok(PineRef::new_box(false));
            }
            let val2 = binary_exp.exp2.rv_run(context)?;
            let bval2 = Bool::implicity_from(val2).unwrap();
            Ok(bval2.into_pf())
        }
        BinaryOp::BoolOr => {
            let val1 = binary_exp.exp1.rv_run(context)?;
            let bval1 = Bool::implicity_from(val1).unwrap();
            if *bval1 == true {
                return Ok(PineRef::new_box(true));
            }
            let val2 = binary_exp.exp2.rv_run(context)?;
            let bval2 = Bool::implicity_from(val2).unwrap();
            Ok(bval2.into_pf())
        }
        _ => {
            let val1 = binary_exp.exp1.rv_run(context)?;
            let val2 = binary_exp.exp2.rv_run(context)?;
            match (&binary_exp.op, val1.get_type(), val2.get_type()) {
                // series(string) + series(string)
                (BinaryOp::Plus, (FirstType::String, SecondType::Series), _)
                | (_, _, (FirstType::String, SecondType::Series)) => {
                    let s1: RefData<Series<String>> = Series::implicity_from(val1).unwrap();
                    let s2: RefData<Series<String>> = Series::implicity_from(val2).unwrap();
                    Ok(PineRef::new_rc(Series::from(
                        s1.get_current() + &s2.get_current(),
                    )))
                }
                // string + string
                (BinaryOp::Plus, (FirstType::String, SecondType::Simple), _)
                | (_, _, (FirstType::String, SecondType::Simple)) => Ok(PineRef::new_rc(
                    String::implicity_from(val1).unwrap().into_inner()
                        + &(String::implicity_from(val2).unwrap().into_inner()),
                )),
                // series(float) +/-/.. any
                (op, (FirstType::Float, SecondType::Series), _)
                | (op, _, (FirstType::Float, SecondType::Series)) => {
                    let f1: RefData<Series<Float>> = Series::implicity_from(val1).unwrap();
                    let f2: RefData<Series<Float>> = Series::implicity_from(val2).unwrap();
                    Ok(bi_operate(op, f1, f2))
                }
                // series(int) +/-/.. any
                (op, (FirstType::Int, SecondType::Series), _)
                | (op, _, (FirstType::Int, SecondType::Series)) => {
                    let d1: RefData<Series<Int>> = Series::implicity_from(val1).unwrap();
                    let d2: RefData<Series<Int>> = Series::implicity_from(val2).unwrap();
                    Ok(bi_operate(op, d1, d2))
                }
                // float +/-/.. any
                (op, (FirstType::Float, SecondType::Simple), _)
                | (op, _, (FirstType::Float, SecondType::Simple)) => {
                    let f1 = Float::implicity_from(val1).unwrap();
                    let f2 = Float::implicity_from(val2).unwrap();
                    Ok(bi_operate(op, f1, f2))
                }
                // int +/-/.. any
                (op, (FirstType::Int, SecondType::Simple), _)
                | (op, _, (FirstType::Int, SecondType::Simple)) => {
                    let d1 = Int::implicity_from(val1).unwrap();
                    let d2 = Int::implicity_from(val2).unwrap();
                    Ok(bi_operate(op, d1, d2))
                }
                _ => Err(PineRuntimeError::new(
                    RuntimeErr::NotSupportOperator,
                    binary_exp.range,
                )),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::input::StrRange;
    use crate::ast::name::VarName;
    use crate::ast::num::Numeral;
    use crate::ast::stat_expr_types::BoolNode;
    use crate::ast::stat_expr_types::Exp;
    use crate::ast::string::StringNode;
    use crate::runtime::context::{Context, ContextType, VarOperate};
    use crate::types::PineStaticType;

    #[test]
    fn unary_op_test() {
        let exp = Exp::Num(Numeral::from_i32(1));
        let exp2 = Exp::Bool(BoolNode::new(true, StrRange::new_empty()));

        let minus_exp = UnaryExp::new(UnaryOp::Minus, exp, StrRange::new_empty());
        let not_exp = UnaryExp::new(UnaryOp::BoolNot, exp2, StrRange::new_empty());

        let mut context = Context::new(None, ContextType::Normal);
        assert_eq!(
            downcast_pf::<Int>(unary_op_run(&minus_exp, &mut context,).unwrap()),
            Ok(RefData::new_box(Some(-1)))
        );

        assert_eq!(
            downcast_pf::<Bool>(unary_op_run(&not_exp, &mut context,).unwrap()),
            Ok(RefData::new_box(false))
        );
    }

    fn int_exp<'a>(int: i32) -> Exp<'a> {
        Exp::Num(Numeral::from_i32(int))
    }

    fn float_exp<'a>(f: f64) -> Exp<'a> {
        Exp::Num(Numeral::from_f64(f))
    }

    fn biop_runner<'a, D: PineStaticType + PartialEq + Debug>(
        op: BinaryOp,
        v1: Exp<'a>,
        v2: Exp<'a>,
    ) -> Result<RefData<D>, RuntimeErr> {
        let mut context = Context::new(None, ContextType::Normal);
        downcast_pf::<D>(
            binary_op_run(
                &BinaryExp::new(op, v1, v2, StrRange::new_empty()),
                &mut context,
            )
            .unwrap(),
        )
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
                Exp::Str(StringNode::new(
                    String::from("hello"),
                    StrRange::new_empty()
                )),
                Exp::Str(StringNode::new(
                    String::from("world"),
                    StrRange::new_empty()
                ))
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
        let gen_bool_exp = |val| Exp::Bool(BoolNode::new_no_range(val));
        assert_eq!(
            biop_runner(BinaryOp::BoolAnd, gen_bool_exp(false), gen_bool_exp(true)),
            Ok(RefData::new_box(false))
        );
        assert_eq!(
            biop_runner(BinaryOp::BoolAnd, gen_bool_exp(false), gen_bool_exp(false)),
            Ok(RefData::new_box(false))
        );
        assert_eq!(
            biop_runner(BinaryOp::BoolOr, gen_bool_exp(false), gen_bool_exp(true)),
            Ok(RefData::new_box(true))
        );
        assert_eq!(
            biop_runner(BinaryOp::BoolOr, gen_bool_exp(false), gen_bool_exp(false)),
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

        downcast_pf::<D>(
            binary_op_run(
                &BinaryExp::new(op, v1, v2, StrRange::new_empty()),
                &mut context,
            )
            .unwrap(),
        )
    }

    fn var_exp<'a>(var: &'a str) -> Exp<'a> {
        Exp::VarName(VarName::new_no_input(var))
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

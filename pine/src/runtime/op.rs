use super::context::{Ctx, PineRuntimeError, RVRunner};
use crate::ast::op::{BinaryOp, UnaryOp};
use crate::ast::stat_expr_types::{BinaryExp, UnaryExp};
use crate::ast::syntax_type::{SimpleSyntaxType, SyntaxType};
use crate::types::{
    downcast_pf, Arithmetic, Bool, Color, DataType as FirstType, Float, Int, Negative, PineFrom,
    PineRef, PineType, RefData, RuntimeErr, SecondType, Series, NA,
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
            let sec_type = val.get_type().1;
            let bool_val = Bool::implicity_from(val).unwrap();
            match (*bool_val, sec_type) {
                (true, SecondType::Simple) => Ok(PineRef::new_box(false)),
                (true, SecondType::Series) => Ok(PineRef::new_rc(Series::from(false))),
                (false, SecondType::Simple) => Ok(PineRef::new_box(true)),
                (false, SecondType::Series) => Ok(PineRef::new_rc(Series::from(true))),
                _ => unreachable!(),
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

fn bi_eq_operate<'a, D>(op: &BinaryOp, d1: &D, d2: &D) -> bool
where
    D: PartialEq + Debug + Clone + PineType<'a> + 'a,
{
    match op {
        BinaryOp::Eq => *d1 == *d2,
        BinaryOp::Neq => *d1 != *d2,
        _ => unreachable!(),
    }
}

fn eq_run<'a, 'b>(
    binary_exp: &'a BinaryExp<'a>,
    context: &mut (dyn 'b + Ctx<'a>),
) -> Result<PineRef<'a>, PineRuntimeError> {
    let val1 = binary_exp.exp1.rv_run(context)?;
    let val2 = binary_exp.exp2.rv_run(context)?;
    match binary_exp.ref_type {
        SyntaxType::Series(SimpleSyntaxType::Bool) => {
            let s1: RefData<Series<Bool>> = Series::implicity_from(val1).unwrap();
            let s2: RefData<Series<Bool>> = Series::implicity_from(val2).unwrap();
            let res = bi_eq_operate(&binary_exp.op, &s1.get_current(), &s2.get_current());
            Ok(PineRef::new_rc(Series::from(res)))
        }
        SyntaxType::Series(SimpleSyntaxType::Int) => {
            let s1: RefData<Series<Int>> = Series::implicity_from(val1).unwrap();
            let s2: RefData<Series<Int>> = Series::implicity_from(val2).unwrap();
            let res = bi_eq_operate(&binary_exp.op, &s1.get_current(), &s2.get_current());
            Ok(PineRef::new_rc(Series::from(res)))
        }
        SyntaxType::Series(SimpleSyntaxType::Float) => {
            let s1: RefData<Series<Float>> = Series::implicity_from(val1).unwrap();
            let s2: RefData<Series<Float>> = Series::implicity_from(val2).unwrap();
            let res = bi_eq_operate(&binary_exp.op, &s1.get_current(), &s2.get_current());
            Ok(PineRef::new_rc(Series::from(res)))
        }
        SyntaxType::Series(SimpleSyntaxType::Na) => {
            let s1: RefData<Series<NA>> = Series::implicity_from(val1).unwrap();
            let s2: RefData<Series<NA>> = Series::implicity_from(val2).unwrap();
            let res = bi_eq_operate(&binary_exp.op, &s1.get_current(), &s2.get_current());
            Ok(PineRef::new_rc(Series::from(res)))
        }
        SyntaxType::Series(SimpleSyntaxType::Color) => {
            let s1: RefData<Series<Color>> = Series::implicity_from(val1).unwrap();
            let s2: RefData<Series<Color>> = Series::implicity_from(val2).unwrap();
            let res = bi_eq_operate(&binary_exp.op, &s1.get_current(), &s2.get_current());
            Ok(PineRef::new_rc(Series::from(res)))
        }
        SyntaxType::Series(SimpleSyntaxType::String) => {
            let s1: RefData<Series<String>> = Series::implicity_from(val1).unwrap();
            let s2: RefData<Series<String>> = Series::implicity_from(val2).unwrap();
            let res = bi_eq_operate(&binary_exp.op, &s1.get_current(), &s2.get_current());
            Ok(PineRef::new_rc(Series::from(res)))
        }

        SyntaxType::Simple(SimpleSyntaxType::Bool) => {
            let s1: RefData<Bool> = Bool::implicity_from(val1).unwrap();
            let s2: RefData<Bool> = Bool::implicity_from(val2).unwrap();
            let res = bi_eq_operate(&binary_exp.op, &*s1, &*s2);
            Ok(PineRef::new_box(res))
        }
        SyntaxType::Simple(SimpleSyntaxType::Int) => {
            let s1: RefData<Int> = Int::implicity_from(val1).unwrap();
            let s2: RefData<Int> = Int::implicity_from(val2).unwrap();
            let res = bi_eq_operate(&binary_exp.op, &*s1, &*s2);
            Ok(PineRef::new_box(res))
        }
        SyntaxType::Simple(SimpleSyntaxType::Float) => {
            let s1: RefData<Float> = Float::implicity_from(val1).unwrap();
            let s2: RefData<Float> = Float::implicity_from(val2).unwrap();
            let res = bi_eq_operate(&binary_exp.op, &*s1, &*s2);
            Ok(PineRef::new_box(res))
        }
        SyntaxType::Simple(SimpleSyntaxType::Na) => {
            let s1: RefData<NA> = NA::implicity_from(val1).unwrap();
            let s2: RefData<NA> = NA::implicity_from(val2).unwrap();
            let res = bi_eq_operate(&binary_exp.op, &*s1, &*s2);
            Ok(PineRef::new_box(res))
        }
        SyntaxType::Simple(SimpleSyntaxType::Color) => {
            let s1: RefData<Color> = Color::implicity_from(val1).unwrap();
            let s2: RefData<Color> = Color::implicity_from(val2).unwrap();
            let res = bi_eq_operate(&binary_exp.op, &*s1, &*s2);
            Ok(PineRef::new_box(res))
        }
        SyntaxType::Simple(SimpleSyntaxType::String) => {
            let s1: RefData<String> = String::implicity_from(val1).unwrap();
            let s2: RefData<String> = String::implicity_from(val2).unwrap();
            let res = bi_eq_operate(&binary_exp.op, &*s1, &*s2);
            Ok(PineRef::new_box(res))
        }
        _ => unreachable!(),
    }
}

fn bool_into_val<'a>(val: bool, result_type: &SyntaxType<'a>) -> PineRef<'a> {
    match result_type {
        SyntaxType::Series(SimpleSyntaxType::Bool) => PineRef::new_rc(Series::from(val)),
        SyntaxType::Simple(SimpleSyntaxType::Bool) => PineRef::new_box(val),
        _ => unreachable!(),
    }
}

pub fn binary_op_run<'a, 'b>(
    binary_exp: &'a BinaryExp<'a>,
    context: &mut (dyn 'b + Ctx<'a>),
) -> Result<PineRef<'a>, PineRuntimeError> {
    match binary_exp.op {
        BinaryOp::BoolAnd => {
            //TODO: That can be generate many small temporary object that can be avoided.
            let val1 = binary_exp.exp1.rv_run(context)?;
            let bval1 = Bool::implicity_from(val1).unwrap();
            if *bval1 == false {
                return Ok(bool_into_val(false, &binary_exp.result_type));
            }
            let val2 = binary_exp.exp2.rv_run(context)?;
            let bval2 = Bool::implicity_from(val2).unwrap();
            Ok(bool_into_val(*bval2, &binary_exp.result_type))
        }
        BinaryOp::BoolOr => {
            let val1 = binary_exp.exp1.rv_run(context)?;
            let bval1 = Bool::implicity_from(val1).unwrap();
            if *bval1 == true {
                return Ok(bool_into_val(true, &binary_exp.result_type));
            }
            let val2 = binary_exp.exp2.rv_run(context)?;
            let bval2 = Bool::implicity_from(val2).unwrap();
            Ok(bool_into_val(*bval2, &binary_exp.result_type))
        }
        BinaryOp::Eq | BinaryOp::Neq => eq_run(&binary_exp, context),
        _ => {
            let val1 = binary_exp.exp1.rv_run(context)?;
            let val2 = binary_exp.exp2.rv_run(context)?;
            match (&binary_exp.op, &binary_exp.ref_type) {
                // series(string) + series(string)
                (BinaryOp::Plus, &SyntaxType::Series(SimpleSyntaxType::String)) => {
                    let s1: RefData<Series<String>> = Series::implicity_from(val1).unwrap();
                    let s2: RefData<Series<String>> = Series::implicity_from(val2).unwrap();
                    Ok(PineRef::new_rc(Series::from(
                        s1.get_current() + &s2.get_current(),
                    )))
                }
                // string + string
                (BinaryOp::Plus, &SyntaxType::Simple(SimpleSyntaxType::String)) => {
                    Ok(PineRef::new_rc(
                        String::implicity_from(val1).unwrap().into_inner()
                            + &(String::implicity_from(val2).unwrap().into_inner()),
                    ))
                }
                // series(float) +/-/.. any
                (op, &SyntaxType::Series(SimpleSyntaxType::Float)) => {
                    let f1: RefData<Series<Float>> = Series::implicity_from(val1).unwrap();
                    let f2: RefData<Series<Float>> = Series::implicity_from(val2).unwrap();
                    Ok(bi_operate(op, f1, f2))
                }
                // float +/-/.. any
                (op, &SyntaxType::Simple(SimpleSyntaxType::Float)) => {
                    let f1 = Float::implicity_from(val1).unwrap();
                    let f2 = Float::implicity_from(val2).unwrap();
                    Ok(bi_operate(op, f1, f2))
                }
                // series(int) +/-/.. any
                (op, &SyntaxType::Series(SimpleSyntaxType::Int)) => {
                    let d1: RefData<Series<Int>> = Series::implicity_from(val1).unwrap();
                    let d2: RefData<Series<Int>> = Series::implicity_from(val2).unwrap();
                    Ok(bi_operate(op, d1, d2))
                }
                // int +/-/.. any
                (op, &SyntaxType::Simple(SimpleSyntaxType::Int)) => {
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
    use crate::ast::num::Numeral;
    use crate::ast::stat_expr_types::{BoolNode, Exp, RVVarName, VarIndex};
    use crate::ast::string::StringNode;
    use crate::runtime::context::{Context, ContextType, VarOperate};
    use crate::syntax::SyntaxParser;
    use crate::types::PineStaticType;
    use std::collections::HashMap;

    #[test]
    fn unary_op_test() {
        let exp = Exp::Num(Numeral::from_i64(1));
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

    fn int_exp<'a>(int: i64) -> Exp<'a> {
        Exp::Num(Numeral::from_i64(int))
    }

    fn float_exp<'a>(f: f64) -> Exp<'a> {
        Exp::Num(Numeral::from_f64(f))
    }

    fn biop_runner<'a, D: PineStaticType + PartialEq + Debug + 'a>(
        op: BinaryOp,
        v1: Exp<'a>,
        v2: Exp<'a>,
        ref_type: SyntaxType<'a>,
    ) -> Result<RefData<D>, RuntimeErr> {
        let mut binary_exp = BinaryExp::new(op, v1, v2, StrRange::new_empty());
        binary_exp.ref_type = ref_type;
        let mut context = Context::new(None, ContextType::Normal);
        downcast_pf::<D>(binary_op_run(&binary_exp, &mut context).unwrap())
    }

    fn biop_runner_type<'a, D: PineStaticType + PartialEq + Debug + 'a>(
        op: BinaryOp,
        v1: Exp<'a>,
        v2: Exp<'a>,
    ) -> Result<RefData<D>, RuntimeErr> {
        let mut exp = BinaryExp::new(op, v1, v2, StrRange::new_empty());
        let var_types: Vec<_> = [
            ("series_int", SyntaxType::Series(SimpleSyntaxType::Int)),
            ("series_bool", SyntaxType::Series(SimpleSyntaxType::Bool)),
        ]
        .iter()
        .cloned()
        .collect();
        SyntaxParser::new_with_vars(&var_types)
            .parse_binary(&mut exp)
            .unwrap();

        let mut context = Context::new(None, ContextType::Normal);
        context.init_vars(vec![
            Some(PineRef::new(Series::from(Some(1)))),
            Some(PineRef::new(Series::from(true))),
        ]);
        downcast_pf::<D>(binary_op_run(&exp, &mut context).unwrap())
    }

    #[test]
    fn binary_op_test() {
        assert_eq!(
            biop_runner(BinaryOp::Plus, int_exp(1), int_exp(2), SyntaxType::int()),
            Ok(RefData::new_box(Some(3)))
        );
        assert_eq!(
            biop_runner(
                BinaryOp::Plus,
                float_exp(1f64),
                float_exp(2f64),
                SyntaxType::float()
            ),
            Ok(RefData::new_box(Some(3f64)))
        );
        assert_eq!(
            biop_runner(
                BinaryOp::Plus,
                int_exp(1i64),
                float_exp(2f64),
                SyntaxType::float()
            ),
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
                )),
                SyntaxType::string(),
            ),
            Ok(RefData::new(String::from("helloworld")))
        );

        assert_eq!(
            biop_runner(BinaryOp::Minus, int_exp(2), int_exp(1), SyntaxType::int()),
            Ok(RefData::new_box(Some(1)))
        );
        assert_eq!(
            biop_runner(
                BinaryOp::Minus,
                float_exp(2f64),
                float_exp(1f64),
                SyntaxType::float()
            ),
            Ok(RefData::new_box(Some(1f64)))
        );
        assert_eq!(
            biop_runner(
                BinaryOp::Mul,
                int_exp(2i64),
                int_exp(3i64),
                SyntaxType::int()
            ),
            Ok(RefData::new_box(Some(6i64)))
        );
        assert_eq!(
            biop_runner(
                BinaryOp::Mul,
                float_exp(2f64),
                float_exp(3f64),
                SyntaxType::float()
            ),
            Ok(RefData::new_box(Some(6f64)))
        );
        assert_eq!(
            biop_runner(
                BinaryOp::Div,
                int_exp(5i64),
                int_exp(2i64),
                SyntaxType::int()
            ),
            Ok(RefData::new_box(Some(2i64)))
        );
        assert_eq!(
            biop_runner(
                BinaryOp::Div,
                float_exp(5f64),
                float_exp(2f64),
                SyntaxType::float()
            ),
            Ok(RefData::new_box(Some(2.5f64)))
        );

        assert_eq!(
            biop_runner(
                BinaryOp::Mod,
                int_exp(12i64),
                int_exp(5i64),
                SyntaxType::int()
            ),
            Ok(RefData::new_box(Some(2i64)))
        );
        assert_eq!(
            biop_runner(
                BinaryOp::Mod,
                float_exp(12f64),
                float_exp(5f64),
                SyntaxType::float()
            ),
            Ok(RefData::new_box(Some(2f64)))
        );
    }

    #[test]
    fn logic_op_test() {
        assert_eq!(
            biop_runner_type(BinaryOp::Eq, int_exp(2), int_exp(1),),
            Ok(RefData::new_box(false))
        );
        assert_eq!(
            biop_runner_type(BinaryOp::Eq, int_exp(2), var_exp("series_int", 0),),
            Ok(RefData::new_rc(Series::from(false)))
        );
        assert_eq!(
            biop_runner_type(BinaryOp::Eq, float_exp(1f64), var_exp("series_int", 0),),
            Ok(RefData::new_rc(Series::from(true)))
        );
        assert_eq!(
            biop_runner_type(BinaryOp::Neq, int_exp(2), int_exp(1),),
            Ok(RefData::new_box(true))
        );
        assert_eq!(
            biop_runner(BinaryOp::Gt, int_exp(2), int_exp(1), SyntaxType::int()),
            Ok(RefData::new_box(true))
        );
        assert_eq!(
            biop_runner(BinaryOp::Geq, int_exp(2), int_exp(1), SyntaxType::int()),
            Ok(RefData::new_box(true))
        );
        assert_eq!(
            biop_runner(BinaryOp::Lt, int_exp(2), int_exp(1), SyntaxType::int()),
            Ok(RefData::new_box(false))
        );
        assert_eq!(
            biop_runner(BinaryOp::Leq, int_exp(2), int_exp(1), SyntaxType::int()),
            Ok(RefData::new_box(false))
        );
    }

    #[test]
    fn logic_test() {
        let gen_bool_exp = |val| Exp::Bool(BoolNode::new_no_range(val));
        assert_eq!(
            biop_runner_type(BinaryOp::BoolAnd, gen_bool_exp(false), gen_bool_exp(true),),
            Ok(RefData::new_box(false))
        );
        assert_eq!(
            biop_runner_type(BinaryOp::BoolAnd, gen_bool_exp(false), gen_bool_exp(false),),
            Ok(RefData::new_box(false))
        );
        assert_eq!(
            biop_runner_type(
                BinaryOp::BoolAnd,
                gen_bool_exp(false),
                var_exp("series_bool", 1)
            ),
            Ok(RefData::new_rc(Series::from(false)))
        );
        assert_eq!(
            biop_runner_type(BinaryOp::BoolOr, gen_bool_exp(false), gen_bool_exp(true),),
            Ok(RefData::new_box(true))
        );
        assert_eq!(
            biop_runner_type(BinaryOp::BoolOr, gen_bool_exp(false), gen_bool_exp(false),),
            Ok(RefData::new_box(false))
        );
    }

    fn biop_rv_runner<'a, D: PineStaticType + PartialEq + Debug + 'a>(
        op: BinaryOp,
        v1: Exp<'a>,
        v2: Exp<'a>,
        ref_type: SyntaxType<'a>,
    ) -> Result<RefData<D>, RuntimeErr> {
        let mut binary_exp = BinaryExp::new(op, v1, v2, StrRange::new_empty());
        binary_exp.ref_type = ref_type;
        let mut context = Context::new(None, ContextType::Normal);
        context.init_vars(vec![
            Some(PineRef::new_box(Some(4))),
            Some(PineRef::new_box(Some(2))),
        ]);
        downcast_pf::<D>(binary_op_run(&binary_exp, &mut context).unwrap())
    }

    fn biop_rv_runner_type<'a, D: PineStaticType + PartialEq + Debug + 'a>(
        op: BinaryOp,
        v1: Exp<'a>,
        v2: Exp<'a>,
    ) -> Result<RefData<D>, RuntimeErr> {
        let mut exp = BinaryExp::new(op, v1, v2, StrRange::new_empty());
        let var_types: Vec<_> = [
            ("arg1", SyntaxType::Simple(SimpleSyntaxType::Int)),
            ("arg2", SyntaxType::Simple(SimpleSyntaxType::Int)),
        ]
        .iter()
        .cloned()
        .collect();
        SyntaxParser::new_with_vars(&var_types)
            .parse_binary(&mut exp)
            .unwrap();

        let mut context = Context::new(None, ContextType::Normal);
        context.init_vars(vec![
            Some(PineRef::new_box(Some(4))),
            Some(PineRef::new_box(Some(2))),
        ]);
        downcast_pf::<D>(binary_op_run(&exp, &mut context).unwrap())
    }

    fn var_exp<'a>(var: &'a str, index: i32) -> Exp<'a> {
        Exp::VarName(RVVarName::new_with_index(var, VarIndex::new(index, 0)))
    }

    #[test]
    fn rv_op_test() {
        assert_eq!(
            biop_rv_runner(
                BinaryOp::Plus,
                var_exp("arg1", 0),
                var_exp("arg2", 1),
                SyntaxType::int()
            ),
            Ok(RefData::new_box(Some(6)))
        );
        assert_eq!(
            biop_rv_runner(
                BinaryOp::Minus,
                var_exp("arg1", 0),
                var_exp("arg2", 1),
                SyntaxType::int()
            ),
            Ok(RefData::new_box(Some(2)))
        );
        assert_eq!(
            biop_rv_runner(
                BinaryOp::Mul,
                var_exp("arg1", 0),
                var_exp("arg2", 1),
                SyntaxType::int()
            ),
            Ok(RefData::new_box(Some(8)))
        );
        assert_eq!(
            biop_rv_runner(
                BinaryOp::Div,
                var_exp("arg1", 0),
                var_exp("arg2", 1),
                SyntaxType::int()
            ),
            Ok(RefData::new_box(Some(2)))
        );
        assert_eq!(
            biop_rv_runner(
                BinaryOp::Mod,
                var_exp("arg1", 0),
                var_exp("arg2", 1),
                SyntaxType::int()
            ),
            Ok(RefData::new_box(Some(0)))
        );

        assert_eq!(
            biop_rv_runner_type(BinaryOp::Eq, var_exp("arg1", 0), var_exp("arg2", 1),),
            Ok(RefData::new_box(false))
        );
        assert_eq!(
            biop_rv_runner_type(BinaryOp::Neq, var_exp("arg1", 0), var_exp("arg2", 1),),
            Ok(RefData::new_box(true))
        );
        assert_eq!(
            biop_rv_runner(
                BinaryOp::Lt,
                var_exp("arg1", 0),
                var_exp("arg2", 1),
                SyntaxType::int()
            ),
            Ok(RefData::new_box(false))
        );
        assert_eq!(
            biop_rv_runner(
                BinaryOp::Leq,
                var_exp("arg1", 0),
                var_exp("arg2", 1),
                SyntaxType::int()
            ),
            Ok(RefData::new_box(false))
        );
        assert_eq!(
            biop_rv_runner(
                BinaryOp::Gt,
                var_exp("arg1", 0),
                var_exp("arg2", 1),
                SyntaxType::int()
            ),
            Ok(RefData::new_box(true))
        );
        assert_eq!(
            biop_rv_runner(
                BinaryOp::Geq,
                var_exp("arg1", 0),
                var_exp("arg2", 1),
                SyntaxType::int()
            ),
            Ok(RefData::new_box(true))
        );
        assert_eq!(
            biop_rv_runner_type(BinaryOp::BoolAnd, var_exp("arg1", 0), var_exp("arg2", 1),),
            Ok(RefData::new_box(true))
        );
        assert_eq!(
            biop_rv_runner_type(BinaryOp::BoolOr, var_exp("arg1", 0), var_exp("arg2", 1),),
            Ok(RefData::new_box(true))
        );
    }
}

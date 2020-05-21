use super::context::{
    Ctx, PineRuntimeError, RVRunner, Runner, RunnerForAssign, RunnerForFunc, RunnerForObj,
};
use super::instance_caller::*;
use super::op::{binary_op_run, unary_op_run};
use super::runtime_convert::convert;
use crate::ast::num::Numeral;
pub use crate::ast::stat_expr_types::{
    Condition, DataType, Exp, FunctionCall, PrefixExp, RVVarName, RefCall, Statement, TypeCast,
};
use crate::ast::syntax_type::{FunctionType, SimpleSyntaxType, SyntaxType};
use crate::types::{
    downcast_pf, downcast_pf_mut, Bool, CallObjEval, Callable, CallableEvaluate, CallableFactory,
    CallableObject, Color, DataType as FirstType, Evaluate, EvaluateFactory, Float, Int, Object,
    PineFrom, PineRef, PineStaticType, PineType, PineVar, RefData, RuntimeErr, SecondType, Series,
    SimpleCallableObject, Tuple, NA,
};
use std::fmt::Debug;

impl<'a> Runner<'a> for Exp<'a> {
    fn run(&'a self, _context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, PineRuntimeError> {
        match self {
            Exp::Na(_) => Ok(PineRef::new_box(NA)),
            Exp::Bool(b) => Ok(PineRef::new_box(b.value)),
            Exp::Num(Numeral::Float(f)) => Ok(PineRef::new_box(Some(f.value))),
            Exp::Num(Numeral::Int(n)) => Ok(PineRef::new_box(Some(n.value))),
            Exp::Str(ref s) => Ok(PineRef::new_rc(String::from(s.value.clone()))),
            Exp::Color(s) => Ok(PineRef::new_box(Color(s.value))),
            Exp::VarName(s) => Ok(PineRef::new_box(PineVar(s.name.value))),
            Exp::Tuple(ref tuple) => {
                let mut col: Vec<PineRef<'a>> = vec![];
                for exp in tuple.exps.iter() {
                    col.push(exp.run(_context)?)
                }
                Ok(PineRef::new_box(Tuple(col)))
            }
            Exp::TypeCast(ref type_cast) => type_cast.run(_context),
            Exp::FuncCall(ref func_call) => func_call.run(_context),
            Exp::RefCall(ref ref_call) => ref_call.run(_context),
            Exp::PrefixExp(ref prefix_exp) => prefix_exp.run(_context),
            Exp::Condition(ref cond) => cond.run(_context),
            Exp::Ite(ref ite) => ite.run(_context),
            Exp::ForRange(ref for_range) => for_range.run(_context),
            Exp::Assignment(ref assign) => assign.run(_context),
            Exp::VarAssignment(ref assign) => assign.run(_context),
            Exp::UnaryExp(ref node) => unary_op_run(&node, _context),
            Exp::BinaryExp(ref node) => binary_op_run(&node, _context),
        }
    }
}

impl<'a> RVRunner<'a> for Exp<'a> {
    fn rv_run(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, PineRuntimeError> {
        match self {
            Exp::VarName(name) => match context.move_var(name.var_index) {
                None => Err(PineRuntimeError::new(RuntimeErr::VarNotFound, self.range())),
                Some(s) => {
                    let ret = match s.get_type() {
                        (FirstType::Evaluate, SecondType::Simple) => {
                            let mut eval_val = downcast_pf::<Evaluate>(s.clone()).unwrap();
                            context.create_runnable(RefData::clone(&eval_val).into_rc());
                            // let eval_val = downcast_pf_mut::<Evaluate>(&mut s).unwrap();
                            eval_val.call(context)
                        }
                        (FirstType::CallableEvaluate, SecondType::Simple)
                        | (FirstType::CallableObjectEvaluate, SecondType::Simple)
                        | (FirstType::EvaluateFactory, SecondType::Simple) => {
                            call_eval_factory(context, name.eval_id, s.copy())
                        }
                        _ => Ok(s.copy()),
                    };
                    context.update_var(name.var_index, s);
                    match ret {
                        Ok(val) => Ok(val),
                        Err(e) => Err(PineRuntimeError::new(e, self.range())),
                    }
                }
            },
            Exp::Tuple(tuple) => {
                let mut col: Vec<PineRef<'a>> = vec![];
                for exp in tuple.exps.iter() {
                    col.push(exp.rv_run(context)?)
                }
                Ok(PineRef::new_box(Tuple(col)))
            }
            _ => self.run(context),
        }
    }
}

impl<'a> RunnerForAssign<'a> for Exp<'a> {
    fn run_for_assign(
        &'a self,
        context: &mut dyn Ctx<'a>,
    ) -> Result<PineRef<'a>, PineRuntimeError> {
        match self {
            Exp::Na(_)
            | Exp::Bool(_)
            | Exp::Num(_)
            | Exp::Str(_)
            | Exp::Color(_)
            | Exp::UnaryExp(_)
            | Exp::BinaryExp(_)
            | Exp::RefCall(_) => self.rv_run(context),
            Exp::VarName(_) => match self.rv_run(context) {
                // The line and label type should not to copy the origin object
                // other object should copy the origin object for assignment
                Ok(v) => match v.get_type() {
                    (FirstType::Line, _) | (FirstType::Label, _) => Ok(v),
                    _ => Ok(v.copy_inner()),
                },
                Err(e) => Err(e),
            },
            Exp::Tuple(tuple) => {
                let mut col: Vec<PineRef<'a>> = vec![];
                for exp in tuple.exps.iter() {
                    col.push(exp.run_for_assign(context)?);
                }
                Ok(PineRef::new_box(Tuple(col)))
            }
            Exp::FuncCall(ref func_call) => func_call.run_for_assign(context),
            Exp::Condition(ref cond) => cond.run_for_assign(context),

            // All other type objects are temporary object, need not to copy inner.
            _ => match self.rv_run(context) {
                Ok(v) => Ok(v.copy_inner()),
                Err(e) => Err(e),
            },
        }
    }
}

impl<'a> RunnerForFunc<'a> for Exp<'a> {
    fn run_for_func(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, PineRuntimeError> {
        match self {
            Exp::VarName(name) => match context.move_var(name.var_index) {
                None => Err(PineRuntimeError::new(RuntimeErr::VarNotFound, self.range())),
                Some(s) => {
                    let ret = match s.get_type() {
                        (FirstType::Evaluate, SecondType::Simple) => {
                            let mut eval_val = downcast_pf::<Evaluate>(s.clone()).unwrap();
                            context.create_runnable(RefData::clone(&eval_val).into_rc());
                            // let eval_val = downcast_pf_mut::<Evaluate>(&mut s).unwrap();
                            eval_val.call(context)
                        }
                        _ => Ok(s.copy()),
                    };
                    context.update_var(name.var_index, s);
                    match ret {
                        Ok(val) => Ok(val),
                        Err(e) => Err(PineRuntimeError::new(e, self.range())),
                    }
                }
            },
            _ => self.rv_run(context),
        }
    }
}

impl<'a> RunnerForObj<'a> for Exp<'a> {
    fn run_for_obj(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, PineRuntimeError> {
        match self {
            Exp::VarName(name) => match context.move_var(name.var_index) {
                None => Err(PineRuntimeError::new(RuntimeErr::VarNotFound, self.range())),
                Some(s) => {
                    let ret = match s.get_type() {
                        (FirstType::Evaluate, SecondType::Simple) => {
                            let mut eval_val = downcast_pf::<Evaluate>(s.clone()).unwrap();
                            context.create_runnable(RefData::clone(&eval_val).into_rc());
                            eval_val.call(context)
                        }
                        (FirstType::CallableEvaluate, SecondType::Simple) => {
                            call_eval_factory(context, name.eval_id, s.copy())
                        }
                        _ => Ok(s.copy()),
                    };
                    context.update_var(name.var_index, s);
                    match ret {
                        Ok(val) => Ok(val),
                        Err(e) => Err(PineRuntimeError::new(e, self.range())),
                    }
                }
            },
            _ => self.rv_run(context),
        }
    }
}

impl<'a> Runner<'a> for TypeCast<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, PineRuntimeError> {
        let result = self.exp.rv_run(context)?;
        match (&self.data_type, result.get_type().1) {
            (&DataType::Bool, SecondType::Simple) => {
                Ok(Bool::explicity_from(result).unwrap().into_pf())
            }
            (&DataType::Bool, SecondType::Series) => {
                let s: RefData<Series<Bool>> = Series::explicity_from(result).unwrap();
                Ok(s.into_pf())
            }
            (&DataType::Int, SecondType::Simple) => {
                Ok(Int::explicity_from(result).unwrap().into_pf())
            }
            (&DataType::Int, SecondType::Series) => {
                let s: RefData<Series<Int>> = Series::explicity_from(result).unwrap();
                Ok(s.into_pf())
            }
            (&DataType::Float, SecondType::Simple) => {
                Ok(Float::explicity_from(result).unwrap().into_pf())
            }
            (&DataType::Float, SecondType::Series) => {
                let s: RefData<Series<Float>> = Series::explicity_from(result).unwrap();
                Ok(s.into_pf())
            }
            (&DataType::Color, SecondType::Simple) => {
                Ok(Color::explicity_from(result).unwrap().into_pf())
            }
            (&DataType::Color, SecondType::Series) => {
                let s: RefData<Series<Color>> = Series::explicity_from(result).unwrap();
                Ok(s.into_pf())
            }
            (&DataType::String, SecondType::Simple) => {
                Ok(String::explicity_from(result).unwrap().into_pf())
            }
            (&DataType::String, SecondType::Series) => {
                let s: RefData<Series<String>> = Series::explicity_from(result).unwrap();
                Ok(s.into_pf())
            }
            (&DataType::Custom(_), SecondType::Simple) => {
                match type_cast_custom(context, self.cast_index, self.func_index, result) {
                    Ok(v) => Ok(v),
                    Err(e) => Err(PineRuntimeError::new(e, self.range)),
                }
            }
            _t => Err(PineRuntimeError::new(
                RuntimeErr::UnknownRuntimeErr,
                self.range,
            )),
        }
    }
}

impl<'a> Runner<'a> for PrefixExp<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, PineRuntimeError> {
        let var = self.left_exp.run_for_obj(context)?;
        // let var = context.move_var(self.var_index).unwrap();
        match var.get_type() {
            (FirstType::Object, SecondType::Simple) => {
                let object = downcast_pf::<Object>(var).unwrap();
                let subobj = object.get(context, self.right_name.value).unwrap();
                Ok(subobj)
            }
            (FirstType::CallableObject, SecondType::Simple) => {
                let object = downcast_pf::<CallableObject>(var).unwrap();
                let subobj = object.get(context, self.right_name.value).unwrap();
                Ok(subobj)
            }
            (FirstType::SimpleCallableObject, SecondType::Simple) => {
                let object = downcast_pf::<SimpleCallableObject>(var).unwrap();
                let subobj = object.get(context, self.right_name.value).unwrap();
                Ok(subobj)
            }
            (FirstType::CallableObjectEvaluate, SecondType::Simple) => {
                let object = downcast_pf::<CallObjEval>(var).unwrap();
                let subobj = object.get(context, self.right_name.value).unwrap();
                Ok(subobj)
            }
            _ => Err(PineRuntimeError::new(
                RuntimeErr::UnknownRuntimeErr,
                self.range,
            )),
        }
    }
}

impl<'a> Runner<'a> for Condition<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, PineRuntimeError> {
        let cond = self.cond.rv_run(context)?;
        let bool_val = Bool::implicity_from(cond).unwrap();
        match *bool_val {
            true => Ok(convert(self.exp1.rv_run(context)?, &self.result_type)),
            false => Ok(convert(self.exp2.rv_run(context)?, &self.result_type)),
        }
    }
}

impl<'a> RunnerForAssign<'a> for Condition<'a> {
    fn run_for_assign(
        &'a self,
        context: &mut dyn Ctx<'a>,
    ) -> Result<PineRef<'a>, PineRuntimeError> {
        let cond = self.cond.rv_run(context)?;
        let bool_val = Bool::implicity_from(cond).unwrap();
        match *bool_val {
            true => Ok(convert(
                self.exp1.run_for_assign(context)?,
                &self.result_type,
            )),
            false => Ok(convert(
                self.exp2.run_for_assign(context)?,
                &self.result_type,
            )),
        }
    }
}

fn get_slice<'a, D>(
    // context: &mut dyn Ctx<'a>,
    // name: &'a str,
    obj: PineRef<'a>,
    arg: PineRef<'a>,
) -> Result<PineRef<'a>, RuntimeErr>
where
    D: Default + PineType<'a> + PineStaticType + PartialEq + PineFrom<'a, D> + Debug + Clone + 'a,
{
    let s: RefData<Series<D>> = Series::implicity_from(obj)?;
    let arg_type = arg.get_type();
    let i = Int::implicity_from(arg)?;
    match *i {
        None => Err(RuntimeErr::UnknownRuntimeErr),
        Some(i) => {
            let res = PineRef::new_rc(s.index(i as usize)?);
            // context.update_var(name, s.into_pf());
            Ok(res)
        }
    }
}

impl<'a> Runner<'a> for RefCall<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, PineRuntimeError> {
        let var = self.name.rv_run(context)?;
        let arg = self.arg.rv_run(context)?;
        // if name.get_type() != (FirstType::PineVar, SecondType::Simple) {
        //     return Err(RuntimeErr::NotSupportOperator);
        // }
        // let varname = downcast_pf::<PineVar>(name).unwrap().0;
        // let var_opt = context.move_var(varname);
        // if var_opt.is_none() {
        //     return Err(RuntimeErr::NotSupportOperator);
        // }

        // let var = var_opt.unwrap();
        let result = match var.get_type() {
            (FirstType::Int, _) => get_slice::<Int>(var, arg),
            (FirstType::Float, _) => get_slice::<Float>(var, arg),
            (FirstType::Bool, _) => get_slice::<Bool>(var, arg),
            (FirstType::Color, _) => get_slice::<Color>(var, arg),
            (FirstType::String, _) => get_slice::<String>(var, arg),
            _ => Err(RuntimeErr::NotSupportOperator),
        };
        match result {
            Ok(val) => Ok(val),
            Err(code) => Err(PineRuntimeError::new(code, self.range)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::color::ColorNode;
    use crate::ast::input::StrRange;
    use crate::ast::name::VarName;
    use crate::ast::num::Numeral;
    use crate::ast::stat_expr_types::{BoolNode, NaNode, RVVarName, TupleNode, VarIndex};
    use crate::ast::string::StringNode;
    use crate::runtime::context::{Context, ContextType};
    use crate::types::{Callable, PineClass};
    use std::fmt::Debug;

    #[test]
    fn prefix_exp_test() {
        struct A;
        impl<'a> PineClass<'a> for A {
            fn custom_type(&self) -> &str {
                "Custom A"
            }

            fn get(
                &self,
                context: &mut dyn Ctx<'a>,
                name: &str,
            ) -> Result<PineRef<'a>, RuntimeErr> {
                match name {
                    "int" => Ok(PineRef::new_box(Some(1i64))),
                    "object" => Ok(PineRef::new_rc(Object::new(Box::new(A)))),
                    _ => Err(RuntimeErr::NotSupportOperator),
                }
            }

            fn copy(&self) -> Box<dyn PineClass<'a> + 'a> {
                Box::new(A)
            }
        }

        let exp = PrefixExp::new_no_input(
            Exp::PrefixExp(Box::new(PrefixExp::new_no_input(
                Exp::VarName(RVVarName::new_no_range("obja")),
                VarName::new_no_input("object"),
            ))),
            VarName::new_no_input("int"),
        );
        // exp.var_index = VarIndex::new(0, 0);

        let mut context = Context::new(None, ContextType::Normal);
        context.init_vars(vec![Some(PineRef::new_rc(Object::new(Box::new(A))))]);

        assert_eq!(
            downcast_pf::<Int>(exp.run(&mut context).unwrap()),
            Ok(RefData::new_box(Some(1)))
        );

        // For CallableObject
        let mut context = Context::new(None, ContextType::Normal);
        context.init_vars(vec![Some(PineRef::new_rc(CallableObject::new(
            Box::new(A),
            || Callable::new(None, None),
        )))]);
        assert_eq!(
            downcast_pf::<Int>(exp.run(&mut context).unwrap()),
            Ok(RefData::new_box(Some(1)))
        );
    }

    #[test]
    fn condition_test() {
        use crate::syntax::SyntaxParser;

        let mut cond_exp = Condition::new_no_input(
            Exp::Bool(BoolNode::new(true, StrRange::new_empty())),
            Exp::Num(Numeral::from_i64(1)),
            Exp::Num(Numeral::from_i64(2)),
        );
        SyntaxParser::new().parse_condition(&mut cond_exp).unwrap();

        let mut context = Context::new(None, ContextType::Normal);
        assert_eq!(
            downcast_pf::<Series<Int>>(cond_exp.run(&mut context).unwrap()),
            Ok(RefData::new_rc(Series::from(Some(1))))
        );
    }

    #[test]
    fn condition2_test() {
        use crate::ast::syntax_type::{SimpleSyntaxType, SyntaxType};
        use crate::syntax::SyntaxParser;

        let mut cond_exp = Condition::new_no_input(
            Exp::VarName(RVVarName::new_no_range("cond")),
            Exp::VarName(RVVarName::new_no_range("exp1")),
            Exp::VarName(RVVarName::new_no_range("exp2")),
        );
        SyntaxParser::new_with_vars(
            &[
                ("cond", SyntaxType::Simple(SimpleSyntaxType::Bool)),
                ("exp1", SyntaxType::Simple(SimpleSyntaxType::Int)),
                ("exp2", SyntaxType::Simple(SimpleSyntaxType::Int)),
            ]
            .iter()
            .cloned()
            .collect(),
        )
        .parse_condition(&mut cond_exp)
        .unwrap();
        // println!("cond exp {:?}", cond_exp);
        let mut context = Context::new(None, ContextType::Normal);
        context.init_vars(vec![
            Some(PineRef::new_box(true)),
            Some(PineRef::new_box(Some(1))),
            Some(PineRef::new_box(Some(2))),
        ]);

        assert_eq!(
            downcast_pf::<Series<Int>>(cond_exp.run(&mut context).unwrap()),
            Ok(RefData::new_rc(Series::from(Some(1))))
        );
    }

    #[test]
    fn ref_call_test() {
        let mut hello_name = RVVarName::new_no_range("hello");
        hello_name.var_index = VarIndex::new(0, 0);
        let exp = RefCall::new_no_input(Exp::VarName(hello_name), Exp::Num(Numeral::from_i64(1)));

        let mut context = Context::new(None, ContextType::Normal);
        let mut series: Series<Int> = Series::from(Some(1));
        series.commit();
        series.update(Some(2));
        context.init_vars(vec![Some(PineRef::new_rc(series))]);

        assert_eq!(
            downcast_pf::<Series<Int>>(exp.run(&mut context).unwrap()),
            Ok(RefData::new_rc(Series::from(Some(1))))
        );
    }

    #[test]
    fn var_rv_test() {
        let exp = Exp::VarName(RVVarName::new_with_index("hello", VarIndex::new(0, 0)));
        let mut context = Context::new(None, ContextType::Normal);
        context.init_vars(vec![Some(PineRef::new_box(Some(1)))]);

        assert_eq!(
            downcast_pf::<Int>(exp.rv_run(&mut context).unwrap()),
            Ok(RefData::new_box(Some(1)))
        );
    }

    fn simple_exp<'a, D>(exp: Exp<'a>, v: D)
    where
        D: PineStaticType + PartialEq + Debug + PineType<'a> + 'a,
    {
        let mut context = Context::new(None, ContextType::Normal);
        assert_eq!(
            downcast_pf::<D>(exp.run(&mut context).unwrap()),
            Ok(RefData::new(v))
        );
    }

    #[test]
    fn simple_exp_test() {
        simple_exp::<NA>(Exp::Na(NaNode::new(StrRange::new_empty())), NA);
        simple_exp::<Bool>(
            Exp::Bool(BoolNode::new(false, StrRange::new_empty())),
            false,
        );
        simple_exp(Exp::Num(Numeral::from_f64(0f64)), Some(0f64));
        simple_exp(Exp::Num(Numeral::from_i64(1)), Some(1));
        simple_exp(
            Exp::Str(StringNode::new(
                String::from("hello"),
                StrRange::new_empty(),
            )),
            String::from("hello"),
        );
        simple_exp(Exp::Color(ColorNode::from_str("#12")), Color("#12"));
        simple_exp(
            Exp::VarName(RVVarName::new_no_range("name")),
            PineVar("name"),
        );
        simple_exp(
            Exp::TypeCast(Box::new(TypeCast::new_no_input(
                DataType::Int,
                Exp::Num(Numeral::from_f64(1.2)),
            ))),
            Some(1),
        );
    }

    fn simple_rv_exp<'a, D>(exp: Exp<'a>, v: D)
    where
        D: PineStaticType + PartialEq + Debug + PineType<'a> + 'a,
    {
        let mut context = Context::new(None, ContextType::Normal);
        context.init_vars(vec![
            Some(PineRef::new_box(Some(1))),
            Some(PineRef::new(Series::from(Some(1)))),
        ]);
        assert_eq!(
            downcast_pf::<D>(exp.rv_run(&mut context).unwrap()),
            Ok(RefData::new(v))
        );
    }

    #[test]
    fn simple_rv_exp_test() {
        simple_rv_exp::<NA>(Exp::Na(NaNode::new(StrRange::new_empty())), NA);
        simple_rv_exp::<Bool>(
            Exp::Bool(BoolNode::new(false, StrRange::new_empty())),
            false,
        );
        simple_rv_exp(Exp::Num(Numeral::from_f64(0f64)), Some(0f64));
        simple_rv_exp(Exp::Num(Numeral::from_i64(1)), Some(1));
        simple_rv_exp(
            Exp::Str(StringNode::new(
                String::from("hello"),
                StrRange::new_empty(),
            )),
            String::from("hello"),
        );
        simple_rv_exp(Exp::Color(ColorNode::from_str("#12")), Color("#12"));

        let mut name = RVVarName::new_no_range("name");
        name.var_index = VarIndex::new(0, 0);

        simple_rv_exp(Exp::VarName(name.clone()), Some(1));
        simple_rv_exp(
            Exp::TypeCast(Box::new(TypeCast::new_no_input(
                DataType::Float,
                Exp::VarName(name.clone()),
            ))),
            Some(1f64),
        );
    }

    #[test]
    fn type_cast_test() {
        simple_exp(
            Exp::TypeCast(Box::new(TypeCast::new_no_input(
                DataType::Int,
                Exp::Num(Numeral::from_f64(1.2)),
            ))),
            Some(1),
        );
        let series_var = RVVarName::new_with_index("series", VarIndex::new(1, 0));
        simple_rv_exp(
            Exp::TypeCast(Box::new(TypeCast::new_no_input(
                DataType::Int,
                Exp::VarName(series_var.clone()),
            ))),
            Series::from(Some(1)),
        );

        simple_exp(
            Exp::TypeCast(Box::new(TypeCast::new_no_input(
                DataType::Float,
                Exp::Num(Numeral::from_f64(1.2)),
            ))),
            Some(1.2),
        );
        simple_rv_exp(
            Exp::TypeCast(Box::new(TypeCast::new_no_input(
                DataType::Float,
                Exp::VarName(series_var.clone()),
            ))),
            Series::from(Some(1.0)),
        );

        simple_exp(
            Exp::TypeCast(Box::new(TypeCast::new_no_input(
                DataType::Bool,
                Exp::Num(Numeral::from_f64(1.2)),
            ))),
            true,
        );
        simple_rv_exp(
            Exp::TypeCast(Box::new(TypeCast::new_no_input(
                DataType::Bool,
                Exp::VarName(series_var.clone()),
            ))),
            Series::from(true),
        );
    }

    #[test]
    fn tuple_exp_test() {
        let exp = Exp::Tuple(Box::new(TupleNode::new(
            vec![Exp::VarName(RVVarName::new_with_index(
                "name",
                VarIndex::new(0, 0),
            ))],
            StrRange::new_empty(),
        )));
        let mut context = Context::new(None, ContextType::Normal);

        let tuple_res = downcast_pf::<Tuple>(exp.run(&mut context).unwrap()).unwrap();
        let vec_res: Vec<RefData<PineVar>> = tuple_res
            .into_inner()
            .0
            .into_iter()
            .map(|s| downcast_pf::<PineVar>(s).unwrap())
            .collect();
        assert_eq!(vec_res, vec![RefData::new_box(PineVar("name"))]);
    }

    #[test]
    fn rv_tuple_exp_test() {
        let exp = Exp::Tuple(Box::new(TupleNode::new(
            vec![Exp::VarName(RVVarName::new_with_index(
                "name",
                VarIndex::new(0, 0),
            ))],
            StrRange::new_empty(),
        )));
        let mut context = Context::new(None, ContextType::Normal);
        context.init_vars(vec![Some(PineRef::new_box(Some(1)))]);

        let tuple_res = downcast_pf::<Tuple>(exp.rv_run(&mut context).unwrap()).unwrap();
        let vec_res: Vec<RefData<Int>> = tuple_res
            .into_inner()
            .0
            .into_iter()
            .map(|s| downcast_pf::<Int>(s).unwrap())
            .collect();
        assert_eq!(vec_res, vec![RefData::new_box(Some(1))]);
    }
}

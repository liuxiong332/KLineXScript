use super::context::{Ctx, RVRunner, Runner, VarOperate};
use super::op::{binary_op_run, unary_op_run};
use crate::ast::name::VarName;
use crate::ast::num::Numeral;
pub use crate::ast::stat_expr_types::{
    Condition, DataType, Exp, FunctionCall, PrefixExp, RefCall, Statement, TypeCast,
};
use crate::types::{
    downcast_pf, Bool, Color, DataType as FirstType, Float, Int, Object, PineFrom, PineRef,
    PineStaticType, PineType, PineVar, RefData, RuntimeErr, SecondType, Series, Tuple, NA,
};
use std::fmt::Debug;

impl<'a> Runner<'a> for Exp<'a> {
    fn run(&'a self, _context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, RuntimeErr> {
        match *self {
            Exp::Na => Ok(PineRef::new_box(NA)),
            Exp::Bool(b) => Ok(PineRef::new_box(b)),
            Exp::Num(Numeral::Float(f)) => Ok(PineRef::new_box(Some(f))),
            Exp::Num(Numeral::Int(n)) => Ok(PineRef::new_box(Some(n))),
            Exp::Str(ref s) => Ok(PineRef::new_rc(String::from(s))),
            Exp::Color(s) => Ok(PineRef::new_box(Color(s))),
            Exp::VarName(VarName(s)) => Ok(PineRef::new_box(PineVar(s))),
            Exp::Tuple(ref tuple) => {
                let mut col: Vec<PineRef<'a>> = vec![];
                for exp in tuple.iter() {
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
            Exp::UnaryExp(ref op, ref exp) => unary_op_run(op, exp, _context),
            Exp::BinaryExp(ref op, ref exp1, ref exp2) => binary_op_run(op, exp1, exp2, _context),
        }
    }
}

impl<'a> RVRunner<'a> for Exp<'a> {
    fn rv_run(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, RuntimeErr> {
        match *self {
            Exp::VarName(VarName(name)) => match context.move_var(name) {
                None => Err(RuntimeErr::VarNotFound),
                Some(s) => {
                    let ret = s.copy();
                    context.update_var(name, s);
                    Ok(ret)
                }
            },
            Exp::Tuple(ref tuple) => {
                let mut col: Vec<PineRef<'a>> = vec![];
                for exp in tuple.iter() {
                    col.push(exp.rv_run(context)?)
                }
                Ok(PineRef::new_box(Tuple(col)))
            }
            _ => self.run(context),
        }
    }
}

impl<'a> Runner<'a> for TypeCast<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, RuntimeErr> {
        let result = self.exp.rv_run(context)?;
        match (&self.data_type, result.get_type().1) {
            (&DataType::Bool, SecondType::Simple) => Ok(Bool::explicity_from(result)?.into_pf()),
            (&DataType::Bool, SecondType::Series) => {
                let s: RefData<Series<Bool>> = Series::explicity_from(result)?;
                Ok(s.into_pf())
            }
            (&DataType::Int, SecondType::Simple) => Ok(Int::explicity_from(result)?.into_pf()),
            (&DataType::Int, SecondType::Series) => {
                let s: RefData<Series<Int>> = Series::explicity_from(result)?;
                Ok(s.into_pf())
            }
            (&DataType::Float, SecondType::Simple) => Ok(Float::explicity_from(result)?.into_pf()),
            (&DataType::Float, SecondType::Series) => {
                let s: RefData<Series<Float>> = Series::explicity_from(result)?;
                Ok(s.into_pf())
            }
            (&DataType::Color, SecondType::Simple) => Ok(Color::explicity_from(result)?.into_pf()),
            (&DataType::Color, SecondType::Series) => {
                let s: RefData<Series<Color>> = Series::explicity_from(result)?;
                Ok(s.into_pf())
            }
            (&DataType::String, SecondType::Simple) => {
                Ok(String::explicity_from(result)?.into_pf())
            }
            (&DataType::String, SecondType::Series) => {
                let s: RefData<Series<String>> = Series::explicity_from(result)?;
                Ok(s.into_pf())
            }
            _t => Err(RuntimeErr::NotCompatible(format!(
                "Cannot convert {:?} to {:?}",
                result.get_type(),
                _t
            ))),
        }
    }
}

impl<'a> Runner<'a> for PrefixExp<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, RuntimeErr> {
        let varname = self.var_chain[0].0;
        let var = context.move_var(varname);
        if var.is_none() {
            return Err(RuntimeErr::NotSupportOperator);
        }
        let var_unwrap = var.unwrap();
        if var_unwrap.get_type() != (FirstType::Object, SecondType::Simple) {
            return Err(RuntimeErr::InvalidVarType(format!(
                "Expect Object type, but get {:?}",
                var_unwrap.get_type().0
            )));
        }
        let object = downcast_pf::<Object>(var_unwrap)?;
        let mut subobj = object.get(self.var_chain[1].0)?;
        for name in self.var_chain[2..].iter() {
            match subobj.get_type() {
                (FirstType::Object, SecondType::Simple) => {
                    let obj = downcast_pf::<Object>(subobj).unwrap();
                    subobj = obj.get(name.0)?;
                }
                _ => return Err(RuntimeErr::NotSupportOperator),
            }
        }
        context.update_var(varname, object.into_pf());
        Ok(subobj)
    }
}

impl<'a> Runner<'a> for Condition<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, RuntimeErr> {
        let cond = self.cond.rv_run(context)?;
        let bool_val = Bool::implicity_from(cond)?;
        match *bool_val {
            true => self.exp1.rv_run(context),
            false => self.exp2.rv_run(context),
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
        None => Err(RuntimeErr::InvalidVarType(format!(
            "Expect simple int, but get {:?}",
            arg_type
        ))),
        Some(i) => {
            let res = PineRef::new_rc(s.index(i as usize)?);
            // context.update_var(name, s.into_pf());
            Ok(res)
        }
    }
}

impl<'a> Runner<'a> for RefCall<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, RuntimeErr> {
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
        match var.get_type() {
            (FirstType::Int, _) => get_slice::<Int>(var, arg),
            (FirstType::Float, _) => get_slice::<Float>(var, arg),
            (FirstType::Bool, _) => get_slice::<Bool>(var, arg),
            (FirstType::Color, _) => get_slice::<Color>(var, arg),
            (FirstType::String, _) => get_slice::<String>(var, arg),
            _ => Err(RuntimeErr::NotSupportOperator),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::context::{Context, ContextType};
    use crate::types::PineClass;
    use std::fmt::Debug;

    #[test]
    fn prefix_exp_test() {
        struct A;
        impl<'a> PineClass<'a> for A {
            fn custom_type(&self) -> &str {
                "Custom A"
            }

            fn get(&self, name: &str) -> Result<PineRef<'a>, RuntimeErr> {
                match name {
                    "int" => Ok(PineRef::new_box(Some(1i32))),
                    "object" => Ok(PineRef::new_rc(Object::new(Box::new(A)))),
                    _ => Err(RuntimeErr::NotSupportOperator),
                }
            }

            fn copy(&self) -> PineRef<'a> {
                PineRef::new_rc(Object::new(Box::new(A)))
            }
        }

        let exp = PrefixExp {
            var_chain: vec![VarName("obja"), VarName("object"), VarName("int")],
        };

        let mut context = Context::new(None, ContextType::Normal);
        context.create_var("obja", PineRef::new_rc(Object::new(Box::new(A))));

        assert_eq!(
            downcast_pf::<Int>(exp.run(&mut context).unwrap()),
            Ok(RefData::new_box(Some(1)))
        );
        // Context::new()
    }

    #[test]
    fn condition_test() {
        let cond_exp = Condition {
            cond: Exp::Bool(true),
            exp1: Exp::Num(Numeral::Int(1)),
            exp2: Exp::Num(Numeral::Int(2)),
        };
        let mut context = Context::new(None, ContextType::Normal);
        assert_eq!(
            downcast_pf::<Int>(cond_exp.run(&mut context).unwrap()),
            Ok(RefData::new_box(Some(1)))
        );
    }

    #[test]
    fn condition2_test() {
        let cond_exp = Condition {
            cond: Exp::VarName(VarName("cond")),
            exp1: Exp::VarName(VarName("exp1")),
            exp2: Exp::VarName(VarName("exp2")),
        };
        let mut context = Context::new(None, ContextType::Normal);
        context.create_var("cond", PineRef::new_box(true));
        context.create_var("exp1", PineRef::new_box(Some(1)));
        context.create_var("exp2", PineRef::new_box(Some(2)));

        assert_eq!(
            downcast_pf::<Int>(cond_exp.run(&mut context).unwrap()),
            Ok(RefData::new_box(Some(1)))
        );
    }

    #[test]
    fn ref_call_test() {
        let exp = RefCall {
            name: Exp::VarName(VarName("hello")),
            arg: Exp::Num(Numeral::Int(1)),
        };

        let mut context = Context::new(None, ContextType::Normal);
        let mut series: Series<Int> = Series::from(Some(1));
        series.commit();
        series.update(Some(2));
        context.create_var("hello", PineRef::new_rc(series));

        assert_eq!(
            downcast_pf::<Series<Int>>(exp.run(&mut context).unwrap()),
            Ok(RefData::new_rc(Series::from(Some(1))))
        );
    }

    #[test]
    fn var_rv_test() {
        let exp = Exp::VarName(VarName("hello"));
        let mut context = Context::new(None, ContextType::Normal);
        context.create_var("hello", PineRef::new_box(Some(1)));

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
        simple_exp::<NA>(Exp::Na, NA);
        simple_exp::<Bool>(Exp::Bool(false), false);
        simple_exp(Exp::Num(Numeral::Float(0f64)), Some(0f64));
        simple_exp(Exp::Num(Numeral::Int(1)), Some(1));
        simple_exp(Exp::Str(String::from("hello")), String::from("hello"));
        simple_exp(Exp::Color("#12"), Color("#12"));
        simple_exp(Exp::VarName(VarName("name")), PineVar("name"));
        simple_exp(
            Exp::TypeCast(Box::new(TypeCast {
                data_type: DataType::Int,
                exp: Exp::Num(Numeral::Float(1.2)),
            })),
            Some(1),
        );
    }

    fn simple_rv_exp<'a, D>(exp: Exp<'a>, v: D)
    where
        D: PineStaticType + PartialEq + Debug + PineType<'a> + 'a,
    {
        let mut context = Context::new(None, ContextType::Normal);
        context.create_var("name", PineRef::new_box(Some(1)));
        context.create_var("series", PineRef::new(Series::from(Some(1))));
        assert_eq!(
            downcast_pf::<D>(exp.rv_run(&mut context).unwrap()),
            Ok(RefData::new(v))
        );
    }

    #[test]
    fn simple_rv_exp_test() {
        simple_rv_exp::<NA>(Exp::Na, NA);
        simple_rv_exp::<Bool>(Exp::Bool(false), false);
        simple_rv_exp(Exp::Num(Numeral::Float(0f64)), Some(0f64));
        simple_rv_exp(Exp::Num(Numeral::Int(1)), Some(1));
        simple_rv_exp(Exp::Str(String::from("hello")), String::from("hello"));
        simple_rv_exp(Exp::Color("#12"), Color("#12"));
        simple_rv_exp(Exp::VarName(VarName("name")), Some(1));
        simple_rv_exp(
            Exp::TypeCast(Box::new(TypeCast {
                data_type: DataType::Float,
                exp: Exp::VarName(VarName("name")),
            })),
            Some(1f64),
        );
    }

    #[test]
    fn type_cast_test() {
        simple_exp(
            Exp::TypeCast(Box::new(TypeCast {
                data_type: DataType::Int,
                exp: Exp::Num(Numeral::Float(1.2)),
            })),
            Some(1),
        );
        simple_rv_exp(
            Exp::TypeCast(Box::new(TypeCast {
                data_type: DataType::Int,
                exp: Exp::VarName(VarName("series")),
            })),
            Series::from(Some(1)),
        );

        simple_exp(
            Exp::TypeCast(Box::new(TypeCast {
                data_type: DataType::Float,
                exp: Exp::Num(Numeral::Float(1.2)),
            })),
            Some(1.2),
        );
        simple_rv_exp(
            Exp::TypeCast(Box::new(TypeCast {
                data_type: DataType::Float,
                exp: Exp::VarName(VarName("series")),
            })),
            Series::from(Some(1.0)),
        );

        simple_exp(
            Exp::TypeCast(Box::new(TypeCast {
                data_type: DataType::Bool,
                exp: Exp::Num(Numeral::Float(1.2)),
            })),
            true,
        );
        simple_rv_exp(
            Exp::TypeCast(Box::new(TypeCast {
                data_type: DataType::Bool,
                exp: Exp::VarName(VarName("series")),
            })),
            Series::from(true),
        );
    }

    #[test]
    fn tuple_exp_test() {
        let exp = Exp::Tuple(Box::new(vec![Exp::VarName(VarName("name"))]));
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
        let exp = Exp::Tuple(Box::new(vec![Exp::VarName(VarName("name"))]));
        let mut context = Context::new(None, ContextType::Normal);
        context.create_var("name", PineRef::new_box(Some(1)));

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

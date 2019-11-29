use super::context::{Ctx, RVRunner, Runner};
use super::op::{binary_op_run, unary_op_run};
use crate::ast::name::VarName;
use crate::ast::num::Numeral;
pub use crate::ast::stat_expr_types::{
    Condition, DataType, Exp, FunctionCall, PrefixExp, RefCall, Statement, TypeCast,
};
use crate::types::{
    downcast, Bool, Color, DataType as FirstType, Float, Int, Object, PineFrom, PineStaticType,
    PineType, PineVar, RuntimeErr, SecondType, Series, Tuple, NA,
};
use std::fmt::Debug;

impl<'a> Runner<'a> for Exp<'a> {
    fn run(&'a self, _context: &mut dyn Ctx<'a>) -> Result<Box<dyn PineType<'a> + 'a>, RuntimeErr> {
        match *self {
            Exp::Na => Ok(Box::new(NA)),
            Exp::Bool(b) => Ok(Box::new(b)),
            Exp::Num(Numeral::Float(f)) => Ok(Box::new(Some(f))),
            Exp::Num(Numeral::Int(n)) => Ok(Box::new(Some(n))),
            Exp::Str(ref s) => Ok(Box::new(String::from(s))),
            Exp::Color(s) => Ok(Box::new(Color(s))),
            Exp::VarName(VarName(s)) => Ok(Box::new(PineVar(s))),
            Exp::Tuple(ref tuple) => {
                let mut col: Vec<Box<dyn PineType<'a> + 'a>> = vec![];
                for exp in tuple.iter() {
                    col.push(exp.run(_context)?)
                }
                Ok(Box::new(Tuple(col)))
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
    fn rv_run(
        &'a self,
        context: &mut dyn Ctx<'a>,
    ) -> Result<Box<dyn PineType<'a> + 'a>, RuntimeErr> {
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
                let mut col: Vec<Box<dyn PineType<'a> + 'a>> = vec![];
                for exp in tuple.iter() {
                    col.push(exp.rv_run(context)?)
                }
                Ok(Box::new(Tuple(col)))
            }
            _ => self.run(context),
        }
    }
}

impl<'a> Runner<'a> for TypeCast<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<Box<dyn PineType<'a> + 'a>, RuntimeErr> {
        let result = self.exp.rv_run(context)?;
        match self.data_type {
            DataType::Bool => Ok(Bool::explicity_from(result)?),
            DataType::Int => Ok(Int::explicity_from(result)?),
            DataType::Float => Ok(Float::explicity_from(result)?),
            DataType::Color => Ok(Color::explicity_from(result)?),
            DataType::String => Ok(String::explicity_from(result)?),
            _ => Err(RuntimeErr::NotCompatible),
        }
    }
}

impl<'a> Runner<'a> for PrefixExp<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<Box<dyn PineType<'a> + 'a>, RuntimeErr> {
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
        let object = downcast::<Object>(var_unwrap)?;
        let mut subobj = object.get(self.var_chain[1].0)?;
        for name in self.var_chain[2..].iter() {
            match subobj.get_type() {
                (FirstType::Object, SecondType::Simple) => {
                    let obj = downcast::<Object>(subobj).unwrap();
                    subobj = obj.get(name.0)?;
                }
                _ => return Err(RuntimeErr::NotSupportOperator),
            }
        }
        context.update_var(varname, object);
        Ok(subobj)
    }
}

impl<'a> Runner<'a> for Condition<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<Box<dyn PineType<'a> + 'a>, RuntimeErr> {
        let cond = self.cond.rv_run(context)?;
        let bool_val = Bool::implicity_from(cond)?;
        match *downcast::<Bool>(bool_val).unwrap() {
            true => self.exp1.rv_run(context),
            false => self.exp2.rv_run(context),
        }
    }
}

fn get_slice<'a, D: Default + PineType<'a> + PineStaticType + Debug + 'a + Clone>(
    context: &mut dyn Ctx<'a>,
    name: &'a str,
    obj: Box<dyn PineType<'a> + 'a>,
    arg: Box<dyn PineType<'a> + 'a>,
) -> Result<Box<dyn PineType<'a> + 'a>, RuntimeErr> {
    let s: Box<Series<D>> = Series::implicity_from(obj)?;
    let arg_type = arg.get_type();
    let i = Int::implicity_from(arg)?;
    match *i {
        None => Err(RuntimeErr::InvalidVarType(format!(
            "Expect simple int, but get {:?} {:?}",
            arg_type.1, arg_type.0
        ))),
        Some(i) => {
            let res = Box::new(s.index(i as usize)?);
            context.update_var(name, s);
            Ok(res)
        }
    }
}

impl<'a> Runner<'a> for RefCall<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<Box<dyn PineType<'a> + 'a>, RuntimeErr> {
        let name = self.name.run(context)?;
        let arg = self.arg.run(context)?;
        if name.get_type() != (FirstType::PineVar, SecondType::Simple) {
            return Err(RuntimeErr::NotSupportOperator);
        }
        let varname = downcast::<PineVar>(name).unwrap().0;
        let var_opt = context.move_var(varname);
        if var_opt.is_none() {
            return Err(RuntimeErr::NotSupportOperator);
        }

        let var = var_opt.unwrap();
        match var.get_type() {
            (FirstType::Int, _) => get_slice::<Int>(context, varname, var, arg),
            (FirstType::Float, _) => get_slice::<Float>(context, varname, var, arg),
            (FirstType::Bool, _) => get_slice::<Bool>(context, varname, var, arg),
            (FirstType::Color, _) => get_slice::<Color>(context, varname, var, arg),
            (FirstType::String, _) => get_slice::<String>(context, varname, var, arg),
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

            fn get(&self, name: &str) -> Result<Box<dyn PineType<'a> + 'a>, RuntimeErr> {
                match name {
                    "int" => Ok(Box::new(Some(1i32))),
                    "object" => Ok(Box::new(Object::new(Box::new(A)))),
                    _ => Err(RuntimeErr::NotSupportOperator),
                }
            }

            fn copy(&self) -> Box<dyn PineType<'a> + 'a> {
                Box::new(Object::new(Box::new(A)))
            }
        }

        let exp = PrefixExp {
            var_chain: vec![VarName("obja"), VarName("object"), VarName("int")],
        };

        let mut context = Context::new(None, ContextType::Normal);
        context.create_var("obja", Box::new(Object::new(Box::new(A))));

        assert_eq!(
            downcast::<Int>(exp.run(&mut context).unwrap()),
            Ok(Box::new(Some(1)))
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
            downcast::<Int>(cond_exp.run(&mut context).unwrap()),
            Ok(Box::new(Some(1)))
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
        context.create_var("cond", Box::new(true));
        context.create_var("exp1", Box::new(Some(1)));
        context.create_var("exp2", Box::new(Some(2)));

        assert_eq!(
            downcast::<Int>(cond_exp.run(&mut context).unwrap()),
            Ok(Box::new(Some(1)))
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
        context.create_var("hello", Box::new(series));

        assert_eq!(
            downcast::<Series<Int>>(exp.run(&mut context).unwrap()),
            Ok(Box::new(Series::from(Some(1))))
        );
    }

    #[test]
    fn var_rv_test() {
        let exp = Exp::VarName(VarName("hello"));
        let mut context = Context::new(None, ContextType::Normal);
        context.create_var("hello", Box::new(Some(1)));

        assert_eq!(
            downcast::<Int>(exp.rv_run(&mut context).unwrap()),
            Ok(Box::new(Some(1)))
        );
    }

    #[test]
    fn simple_exp_test() {
        fn simple_exp<'a, D: PineStaticType + PartialEq + Debug>(exp: Exp<'a>, v: D) {
            let mut context = Context::new(None, ContextType::Normal);
            assert_eq!(
                downcast::<D>(exp.run(&mut context).unwrap()),
                Ok(Box::new(v))
            );
        }

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

    #[test]
    fn simple_rv_exp_test() {
        fn simple_exp<'a, D: PineStaticType + PartialEq + Debug>(exp: Exp<'a>, v: D) {
            let mut context = Context::new(None, ContextType::Normal);
            context.create_var("name", Box::new(Some(1)));
            assert_eq!(
                downcast::<D>(exp.rv_run(&mut context).unwrap()),
                Ok(Box::new(v))
            );
        }

        simple_exp::<NA>(Exp::Na, NA);
        simple_exp::<Bool>(Exp::Bool(false), false);
        simple_exp(Exp::Num(Numeral::Float(0f64)), Some(0f64));
        simple_exp(Exp::Num(Numeral::Int(1)), Some(1));
        simple_exp(Exp::Str(String::from("hello")), String::from("hello"));
        simple_exp(Exp::Color("#12"), Color("#12"));
        simple_exp(Exp::VarName(VarName("name")), Some(1));
        simple_exp(
            Exp::TypeCast(Box::new(TypeCast {
                data_type: DataType::Float,
                exp: Exp::VarName(VarName("name")),
            })),
            Some(1f64),
        );
    }

    #[test]
    fn tuple_exp_test() {
        let exp = Exp::Tuple(Box::new(vec![Exp::VarName(VarName("name"))]));
        let mut context = Context::new(None, ContextType::Normal);

        let tuple_res = downcast::<Tuple>(exp.run(&mut context).unwrap()).unwrap();
        let vec_res: Vec<Box<PineVar>> = tuple_res
            .0
            .into_iter()
            .map(|s| downcast::<PineVar>(s).unwrap())
            .collect();
        assert_eq!(vec_res, vec![Box::new(PineVar("name"))]);
    }

    #[test]
    fn rv_tuple_exp_test() {
        let exp = Exp::Tuple(Box::new(vec![Exp::VarName(VarName("name"))]));
        let mut context = Context::new(None, ContextType::Normal);
        context.create_var("name", Box::new(Some(1)));

        let tuple_res = downcast::<Tuple>(exp.rv_run(&mut context).unwrap()).unwrap();
        let vec_res: Vec<Box<Int>> = tuple_res
            .0
            .into_iter()
            .map(|s| downcast::<Int>(s).unwrap())
            .collect();
        assert_eq!(vec_res, vec![Box::new(Some(1))]);
    }
}

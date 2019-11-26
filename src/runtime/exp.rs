use super::context::{Ctx, Runner};
use super::op::{binary_op_run, unary_op_run};
use crate::ast::name::VarName;
use crate::ast::num::Numeral;
pub use crate::ast::stat_expr_types::{
    Condition, DataType, Exp, FunctionCall, PrefixExp, RefCall, Statement, TypeCast,
};
use crate::types::{
    downcast, Bool, Color, ConvertErr, DataType as FirstType, Float, Int, Object, PineFrom,
    PineStaticType, PineType, PineVar, SecondType, Series, Tuple, NA,
};

impl<'a> Runner<'a> for Exp<'a> {
    fn run(&'a self, _context: &mut dyn Ctx<'a>) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        match *self {
            Exp::Na => Ok(Box::new(NA)),
            Exp::Bool(b) => Ok(Box::new(b)),
            Exp::Num(Numeral::Float(f)) => Ok(Box::new(Some(f))),
            Exp::Num(Numeral::Int(n)) => Ok(Box::new(Some(n))),
            Exp::Str(ref s) => Ok(Box::new(String::from(s))),
            Exp::Color(s) => Ok(Box::new(Color(s))),
            Exp::VarName(VarName(s)) => Ok(Box::new(PineVar(s))),
            Exp::RetTuple(ref tuple) => {
                let col: Vec<Box<dyn PineType + 'a>> = tuple
                    .iter()
                    .map(|&VarName(v)| Box::new(PineVar(v)) as Box<dyn PineType + 'a>)
                    .collect();
                Ok(Box::new(Tuple(col)))
            }
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
            // Ite(Box<IfThenElse<'a>>),
            // ForRange(Box<ForRange<'a>>),
            Exp::UnaryExp(ref op, ref exp) => unary_op_run(op, exp, _context),
            Exp::BinaryExp(ref op, ref exp1, ref exp2) => binary_op_run(op, exp1, exp2, _context),
            _ => unreachable!(),
        }
    }
}

impl<'a> Runner<'a> for TypeCast<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        let result = self.exp.run(context)?;
        match self.data_type {
            DataType::Bool => Ok(Bool::explicity_from(result)?),
            DataType::Int => Ok(Int::explicity_from(result)?),
            DataType::Float => Ok(Float::explicity_from(result)?),
            DataType::Color => Ok(Color::explicity_from(result)?),
            DataType::String => Ok(String::explicity_from(result)?),
            _ => Err(ConvertErr::NotCompatible),
        }
    }
}

impl<'a> Runner<'a> for PrefixExp<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        let varname = self.var_chain[0].0;
        let var = context.move_var(varname);
        if var.is_none() {
            return Err(ConvertErr::NotSupportOperator);
        }
        let var_unwrap = var.unwrap();
        if var_unwrap.get_type() != (FirstType::Object, SecondType::Simple) {
            return Err(ConvertErr::InvalidVarType(format!(
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
                _ => return Err(ConvertErr::NotSupportOperator),
            }
        }
        context.update_var(varname, object);
        Ok(subobj)
    }
}

impl<'a> Runner<'a> for Condition<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        let cond = self.cond.run(context)?;
        let bool_val = Bool::implicity_from(cond)?;
        match *downcast::<Bool>(bool_val).unwrap() {
            true => self.exp1.run(context),
            false => self.exp2.run(context),
        }
    }
}

fn get_slice<'a, D: Default + PineType<'a> + PineStaticType + 'a + Clone>(
    context: &mut dyn Ctx<'a>,
    name: &'a str,
    obj: Box<dyn PineType<'a> + 'a>,
    arg: Box<dyn PineType<'a> + 'a>,
) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
    let s: Box<Series<D>> = Series::implicity_from(obj)?;
    let arg_type = arg.get_type();
    let i = Int::implicity_from(arg)?;
    match *i {
        None => Err(ConvertErr::InvalidVarType(format!(
            "Expect simple int, but get {:?} {:?}",
            arg_type.1, arg_type.0
        ))),
        Some(i) => {
            let res = Box::new(s.index(i as usize)?.clone());
            context.update_var(name, s);
            Ok(res)
        }
    }
}

impl<'a> Runner<'a> for RefCall<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        let name = self.name.run(context)?;
        let arg = self.arg.run(context)?;
        if name.get_type() != (FirstType::PineVar, SecondType::Simple) {
            return Err(ConvertErr::NotSupportOperator);
        }
        let varname = downcast::<PineVar>(name).unwrap().0;
        let var_opt = context.move_var(varname);
        if var_opt.is_none() {
            return Err(ConvertErr::NotSupportOperator);
        }

        let var = var_opt.unwrap();
        match var.get_type() {
            (FirstType::Int, _) => get_slice::<Int>(context, varname, var, arg),
            (FirstType::Float, _) => get_slice::<Float>(context, varname, var, arg),
            (FirstType::Bool, _) => get_slice::<Bool>(context, varname, var, arg),
            (FirstType::Color, _) => get_slice::<Color>(context, varname, var, arg),
            (FirstType::String, _) => get_slice::<String>(context, varname, var, arg),
            _ => Err(ConvertErr::NotSupportOperator),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::context::{Context, ContextType};
    use crate::types::PineClass;

    #[test]
    fn prefix_exp_test() {
        struct A;
        impl<'a> PineClass<'a> for A {
            fn custom_type(&self) -> &str {
                "Custom A"
            }

            fn get(&self, name: &str) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
                match name {
                    "int" => Ok(Box::new(Some(1i32))),
                    "object" => Ok(Box::new(Object::new(Box::new(A)))),
                    _ => Err(ConvertErr::NotSupportOperator),
                }
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
            downcast::<Int>(exp.run(&mut context).unwrap()),
            Ok(Box::new(Some(1)))
        );
    }
}

use super::context::{Context, Runner};
use super::op::{binary_op_run, unary_op_run};
use crate::ast::name::VarName;
use crate::ast::num::Numeral;
pub use crate::ast::stat_expr_types::{
    Condition, DataType, Exp, FunctionCall, PrefixExp, RefCall, Statement, TypeCast,
};
use crate::types::{
    downcast, Bool, Callable, Color, ConvertErr, DataType as FirstType, Float, Int, Object,
    PineFrom, PineType, PineVar, SecondType, Tuple, NA,
};

impl<'a> Runner<'a> for Exp<'a> {
    fn run(&self, _context: &mut Context<'a>) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
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
            // Exp::RefCall(ref ref_call) => {}
            Exp::UnaryExp(ref op, ref exp) => unary_op_run(op, exp, _context),
            Exp::BinaryExp(ref op, ref exp1, ref exp2) => binary_op_run(op, exp1, exp2, _context),
            _ => unreachable!(),
        }
    }
}

impl<'a> Runner<'a> for TypeCast<'a> {
    fn run(&self, _context: &mut Context<'a>) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        let result = self.exp.run(_context)?;
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

impl<'a> Runner<'a> for FunctionCall<'a> {
    fn run(&self, _context: &mut Context<'a>) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        let result = self.method.run(_context)?;
        match result.get_type() {
            (FirstType::Callable, SecondType::Simple) => {
                let callable = downcast::<Callable>(result).unwrap();
                let mut pos_args = vec![];
                for exp in self.pos_args.iter() {
                    pos_args.push(exp.run(_context)?);
                }
                let mut dict_args = vec![];
                for (n, exp) in self.dict_args.iter() {
                    dict_args.push((n.0, exp.run(_context)?));
                }
                callable.call(pos_args, dict_args)
            }
            _ => Err(ConvertErr::NotSupportOperator),
        }
    }
}

impl<'a> Runner<'a> for PrefixExp<'a> {
    fn run(&self, context: &mut Context<'a>) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        let varname = self.var_chain[0].0;
        let var = context.objects.get(varname);
        if var.is_none() {
            return Err(ConvertErr::NotSupportOperator);
        }
        let var_unwrap = var.unwrap();
        let name = self.var_chain[1];
        let mut subobj = var_unwrap.get(name.0)?;
        for name in self.var_chain[2..].iter() {
            match subobj.get_type() {
                (FirstType::Object, SecondType::Simple) => {
                    let obj = downcast::<Object>(subobj).unwrap();
                    subobj = obj.get(name.0)?;
                }
                _ => return Err(ConvertErr::NotSupportOperator),
            }
        }
        Ok(subobj)
    }
}

impl<'a> Runner<'a> for Condition<'a> {
    fn run(&self, context: &mut Context<'a>) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        let cond = self.cond.run(context)?;
        let bool_val = Bool::implicity_from(cond)?;
        match *downcast::<Bool>(bool_val).unwrap() {
            true => self.exp1.run(context),
            false => self.exp2.run(context),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::PineClass;
    use std::collections::HashMap;

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

        let mut objects: HashMap<&str, Box<Object>> = HashMap::new();
        objects.insert("obja", Box::new(Object::new(Box::new(A))));
        let mut context = Context::new(objects);

        let exp = PrefixExp {
            var_chain: vec![VarName("obja"), VarName("object"), VarName("int")],
        };
        assert_eq!(
            downcast::<Int>(exp.run(&mut context).unwrap()),
            Ok(Box::new(Some(1)))
        );
        // Context::new()
    }

    #[test]
    fn condition_test() {
        let mut context = Context::new(HashMap::new());
        let cond_exp = Condition {
            cond: Exp::Bool(true),
            exp1: Exp::Num(Numeral::Int(1)),
            exp2: Exp::Num(Numeral::Int(2)),
        };
        assert_eq!(
            downcast::<Int>(cond_exp.run(&mut context).unwrap()),
            Ok(Box::new(Some(1)))
        );
    }
}

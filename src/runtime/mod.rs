use super::ast::name::VarName;
use super::ast::num::Numeral;
use super::ast::stat_expr_types::{DataType, Exp, FunctionCall, Statement, TypeCast};
use super::types::{
    downcast, Bool, Callable, Color, ConvertErr, DataType as FirstType, Float, Int, PineFrom,
    PineType, PineVar, SecondType, Tuple, NA,
};
use std::collections::HashMap;

struct Context<'a> {
    input: &'a str,
    vars: HashMap<&'a str, Box<dyn PineType<'a>>>,
}

trait Runner<'a> {
    fn run(&self, context: &mut Context<'a>) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr>;
}

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
            _ => unreachable!(),
        }
    }
}

impl<'a> Runner<'a> for TypeCast<'a> {
    fn run(&self, _context: &mut Context<'a>) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        let result = self.exp.run(_context)?;
        match self.data_type {
            DataType::Bool => Bool::explicity_from(result),
            DataType::Int => Int::explicity_from(result),
            DataType::Float => Float::explicity_from(result),
            DataType::Color => Color::explicity_from(result),
            DataType::String => String::explicity_from(result),
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {}
}

use super::ast::name::VarName;
use super::ast::num::Numeral;
use super::ast::stat_expr_types::{Exp, Statement, TypeCast};
use super::types::{Color, ConvertErr, DataType, PineType, PineVar, Tuple, NA};
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
            // Exp::Num(Numeral::Float(f)) => Ok(Box::new(f)),
            // Exp::Num(Numeral::Int(n)) => Ok(Box::new(n)),
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
            // Exp::TypeCast(ref type_cast) => {
            //     let result: Box<dyn PineType<'a> + 'a> = type_cast.exp.run(_context)?;
            //     result.into(&type_cast.data_type)
            // }
            // Exp::FuncCall(ref func_call) => {}
            _ => unreachable!(),
        }
    }
}

// impl<'a> Runner<'a> for TypeCast<'a> {
//     fn run(&self, _context: &mut Context<'a>) -> Box<dyn PineType<'a>> {
//         let result = self.exp.run(_context);
//     }
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {}
}

use super::ast::name::VarName;
use super::ast::num::Numeral;
use super::ast::stat_expr_types::{Exp, Statement, TypeCast};
use super::types::{Color, PineType, PineVar, Tuple, NA};
use std::collections::HashMap;

struct Context<'a> {
    input: &'a str,
    vars: HashMap<&'a str, Box<dyn PineType<'a>>>,
}

trait Runner<'a> {
    fn run(&self, context: &mut Context<'a>) -> Box<dyn PineType<'a>>;
}

impl<'a> Runner<'a> for Exp<'a> {
    fn run(&self, _context: &mut Context<'a>) -> Box<dyn PineType<'a>> {
        match *self {
            Exp::Na => Box::new(NA),
            Exp::Bool(b) => Box::new(b),
            Exp::Num(Numeral::Float(f)) => Box::new(f),
            Exp::Num(Numeral::Int(n)) => Box::new(n),
            Exp::Str(ref s) => Box::new(String::from(s)),
            Exp::Color(s) => Box::new(Color(s)),
            Exp::VarName(VarName(s)) => Box::new(PineVar(s)),
            Exp::RetTuple(ref tuple) => {
                let col: Vec<Box<dyn PineType + 'a>> = tuple
                    .iter()
                    .map(|&VarName(v)| Box::new(PineVar(v)) as Box<dyn PineType + 'a>)
                    .collect();
                Box::new(Tuple(col))
            }
            Exp::Tuple(ref tuple) => {
                let col: Vec<Box<dyn PineType + 'a>> = tuple
                    .iter()
                    .map(|exp| exp.run(_context) as Box<dyn PineType + 'a>)
                    .collect();
                Box::new(Tuple(col))
            }
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

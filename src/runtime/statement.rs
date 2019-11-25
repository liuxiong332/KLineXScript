use super::context::{Context, Runner, StmtRunner};
use crate::ast::stat_expr_types::{Assignment, DataType, Statement};
use crate::types::{
    Bool, Color, ConvertErr, DataType as FirstType, Float, Int, PineFrom, PineType,
};

impl<'a> StmtRunner<'a> for Statement<'a> {
    fn run(&self, context: &mut Context<'a>) -> Result<(), ConvertErr> {
        match *self {
            Statement::Assignment(ref assign) => assign.run(context),
            _ => Err(ConvertErr::NotSupportOperator),
        }
    }
}

impl<'a> StmtRunner<'a> for Assignment<'a> {
    fn run(&self, context: &mut Context<'a>) -> Result<(), ConvertErr> {
        let name = self.name.0;
        if context.declare_vars.contains(name) {
            return Err(ConvertErr::NameDeclared);
        }
        context.declare_vars.insert(name);

        // For variable declare with var type, it only need initialize once.
        if self.var && context.vars.contains_key(name) {
            return Ok(());
        }
        let val = self.val.run(context)?;
        let true_val: Box<dyn PineType<'a> + 'a> = match self.var_type {
            None => val,
            Some(DataType::Int) => Int::explicity_from(val)?,
            Some(DataType::Bool) => Bool::explicity_from(val)?,
            Some(DataType::Float) => Float::explicity_from(val)?,
            Some(DataType::Color) => Color::explicity_from(val)?,
            Some(DataType::String) => String::explicity_from(val)?,
            _ => return Err(ConvertErr::InvalidTypeCast),
        };
        if let (FirstType::NA, _) = true_val.get_type() {
            return Err(ConvertErr::InvalidNADeclarer);
        }
        context.vars.insert(name, true_val);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::name::VarName;
    use crate::ast::num::Numeral;
    use crate::runtime::exp::Exp;
    use std::collections::HashMap;

    #[test]
    fn assignment_test() {
        let mut context = Context::new(HashMap::new());
        let test_val = |context: &mut Context, int_val| {
            let val = Int::explicity_from(context.vars.remove("hello").unwrap());
            assert_eq!(val, Ok(Box::new(Some(int_val))));
            context.vars.insert("hello", val.unwrap());
        };
        let assign = Statement::Assignment(Box::new(Assignment::new(
            VarName("hello"),
            Exp::Num(Numeral::Int(12)),
            false,
            None,
        )));
        assert_eq!(assign.run(&mut context), Ok(()));
        test_val(&mut context, 12);

        context.declare_vars.clear();
        let assign = Statement::Assignment(Box::new(Assignment::new(
            VarName("hello"),
            Exp::Num(Numeral::Int(23)),
            true,
            None,
        )));
        assert_eq!(assign.run(&mut context), Ok(()));
        test_val(&mut context, 12);

        context.declare_vars.clear();
        let assign = Statement::Assignment(Box::new(Assignment::new(
            VarName("hello"),
            Exp::Num(Numeral::Int(23)),
            false,
            Some(DataType::Int),
        )));
        assert_eq!(assign.run(&mut context), Ok(()));
        test_val(&mut context, 23);
    }
}

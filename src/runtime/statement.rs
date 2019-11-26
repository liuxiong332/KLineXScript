use super::context::{Context, Runner, StmtRunner};
use crate::ast::stat_expr_types::{Assignment, DataType, Statement, VarAssignment};
use crate::types::{
    Bool, Color, ConvertErr, DataType as FirstType, Float, Int, PineFrom, PineStaticType, PineType,
    Series,
};
use std::collections::HashMap;

impl<'a> StmtRunner<'a> for Statement<'a> {
    fn run(&self, context: &mut Context<'a>) -> Result<(), ConvertErr> {
        match *self {
            Statement::Assignment(ref assign) => assign.run(context),
            Statement::VarAssignment(ref var_assign) => var_assign.run(context),
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

fn update_series<'a, D: Default + PineType<'a> + PineStaticType + 'a + PineFrom<'a, D> + Clone>(
    vars: &mut HashMap<&'a str, Box<dyn PineType<'a> + 'a>>,
    name: &'a str,
    exist_val: Box<dyn PineType<'a> + 'a>,
    val: Box<dyn PineType<'a> + 'a>,
) -> Result<(), ConvertErr> {
    let mut s = Series::implicity_from(exist_val)?;
    s.update(*D::implicity_from(val)?);
    vars.insert(name, s);
    Ok(())
}

impl<'a> StmtRunner<'a> for VarAssignment<'a> {
    fn run(&self, context: &mut Context<'a>) -> Result<(), ConvertErr> {
        let name = self.name.0;
        if !context.declare_vars.contains(name) {
            return Err(ConvertErr::NameNotDeclard);
        }
        let val = self.val.run(context)?;
        let exist_val = context.vars.remove(name).unwrap();
        let vars = &mut context.vars;
        match exist_val.get_type() {
            (FirstType::Bool, _) => update_series::<Bool>(vars, name, exist_val, val),
            (FirstType::Int, _) => update_series::<Int>(vars, name, exist_val, val),
            (FirstType::Float, _) => update_series::<Float>(vars, name, exist_val, val),
            (FirstType::Color, _) => update_series::<Color>(vars, name, exist_val, val),
            (FirstType::String, _) => update_series::<String>(vars, name, exist_val, val),
            _ => Err(ConvertErr::NotSupportOperator),
        }
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

    #[test]
    fn var_assignment_test() {
        let mut context = Context::new(HashMap::new());
        context.vars.insert("hello", Box::new(Some(12)));
        context.declare_vars.insert("hello");

        let test_val = |vars: &mut HashMap<_, _>, int_val| {
            let s: Box<Series<Int>> =
                Series::implicity_from(vars.remove("hello").unwrap()).unwrap();
            assert_eq!(s, Box::new(Series::from(Some(int_val))));
            vars.insert("hello", s);
        };
        let assign = VarAssignment::new(VarName("hello"), Exp::Num(Numeral::Int(24)));
        assert_eq!(assign.run(&mut context), Ok(()));
        test_val(&mut context.vars, 24);

        let assign = VarAssignment::new(VarName("hello"), Exp::Num(Numeral::Int(36)));
        assert_eq!(assign.run(&mut context), Ok(()));
        test_val(&mut context.vars, 36);
    }
}

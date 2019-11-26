use super::context::{Context, Runner, StmtRunner};
use crate::ast::stat_expr_types::{Assignment, DataType, Statement, VarAssignment};
use crate::types::{
    Bool, Color, ConvertErr, DataType as FirstType, Float, Int, PineFrom, PineStaticType, PineType,
    Series,
};

impl<'a, 'b> StmtRunner<'a, 'b> for Statement<'a> {
    fn run(&self, context: &mut Context<'a, 'b>) -> Result<(), ConvertErr> {
        match *self {
            Statement::Assignment(ref assign) => assign.run(context),
            Statement::VarAssignment(ref var_assign) => var_assign.run(context),
            _ => Err(ConvertErr::NotSupportOperator),
        }
    }
}

impl<'a, 'b> StmtRunner<'a, 'b> for Assignment<'a> {
    fn run(&self, context: &mut Context<'a, 'b>) -> Result<(), ConvertErr> {
        let name = self.name.0;
        if context.contains_declare(name) {
            return Err(ConvertErr::NameDeclared);
        }
        context.create_declare(name);

        // For variable declare with var type, it only need initialize once.
        if self.var && context.contains_var(name) {
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
        context.create_var(name, true_val);
        Ok(())
    }
}

fn update_series<'a, 'b, D>(
    context: &mut Context<'a, 'b>,
    name: &'a str,
    exist_val: Box<dyn PineType<'a> + 'a>,
    val: Box<dyn PineType<'a> + 'a>,
) -> Result<(), ConvertErr>
where
    D: Default + PineType<'a> + PineStaticType + 'a + PineFrom<'a, D> + Clone,
{
    let mut s = Series::implicity_from(exist_val)?;
    s.update(*D::implicity_from(val)?);
    context.update_var(name, s);
    Ok(())
}

impl<'a, 'b> StmtRunner<'a, 'b> for VarAssignment<'a> {
    fn run(&self, context: &mut Context<'a, 'b>) -> Result<(), ConvertErr> {
        let name = self.name.0;
        if !context.contains_declare(name) {
            return Err(ConvertErr::NameNotDeclard);
        }
        let val = self.val.run(context)?;
        let exist_val = context.move_var(name).unwrap();
        match exist_val.get_type() {
            (FirstType::Bool, _) => update_series::<Bool>(context, name, exist_val, val),
            (FirstType::Int, _) => update_series::<Int>(context, name, exist_val, val),
            (FirstType::Float, _) => update_series::<Float>(context, name, exist_val, val),
            (FirstType::Color, _) => update_series::<Color>(context, name, exist_val, val),
            (FirstType::String, _) => update_series::<String>(context, name, exist_val, val),
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

    #[test]
    fn assignment_test() {
        let mut context = Context::new(None);
        let test_val = |context: &mut Context, int_val| {
            let val = Int::explicity_from(context.move_var("hello").unwrap());
            assert_eq!(val, Ok(Box::new(Some(int_val))));
            context.update_var("hello", val.unwrap());
        };
        let assign = Statement::Assignment(Box::new(Assignment::new(
            VarName("hello"),
            Exp::Num(Numeral::Int(12)),
            false,
            None,
        )));
        assert_eq!(assign.run(&mut context), Ok(()));
        test_val(&mut context, 12);

        context.clear_declare();
        let assign = Statement::Assignment(Box::new(Assignment::new(
            VarName("hello"),
            Exp::Num(Numeral::Int(23)),
            true,
            None,
        )));
        assert_eq!(assign.run(&mut context), Ok(()));
        test_val(&mut context, 12);

        context.clear_declare();
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
        let mut context = Context::new(None);
        context.create_var("hello", Box::new(Some(12)));
        context.create_declare("hello");

        let test_val = |context: &mut Context, int_val| {
            let s: Box<Series<Int>> =
                Series::implicity_from(context.move_var("hello").unwrap()).unwrap();
            assert_eq!(s, Box::new(Series::from(Some(int_val))));
            context.update_var("hello", s);
        };
        let assign = VarAssignment::new(VarName("hello"), Exp::Num(Numeral::Int(24)));
        assert_eq!(assign.run(&mut context), Ok(()));
        test_val(&mut context, 24);

        let assign = VarAssignment::new(VarName("hello"), Exp::Num(Numeral::Int(36)));
        assert_eq!(assign.run(&mut context), Ok(()));
        test_val(&mut context, 36);
    }
}

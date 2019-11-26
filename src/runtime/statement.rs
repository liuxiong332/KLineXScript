use super::context::{Context, ContextType, Ctx, Runner, StmtRunner};
use super::function::Function;
use crate::ast::stat_expr_types::{
    Assignment, Block, DataType, ForRange, FunctionCall, FunctionDef, IfThenElse, Statement,
    VarAssignment,
};
use crate::types::{
    downcast, Bool, Callable, Color, ConvertErr, DataType as FirstType, Float, Int, PineFrom,
    PineStaticType, PineType, SecondType, Series, NA,
};

impl<'a> StmtRunner<'a> for Statement<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<(), ConvertErr> {
        match *self {
            Statement::None => Ok(()),
            Statement::Break => Err(ConvertErr::Break),
            Statement::Continue => Err(ConvertErr::Continue),
            Statement::Assignment(ref assign) => assign.run(context),
            Statement::VarAssignment(ref var_assign) => var_assign.run(context),
            Statement::Ite(ref ite) => StmtRunner::run(ite.as_ref(), context),
            Statement::ForRange(ref fr) => StmtRunner::run(fr.as_ref(), context),
            Statement::FuncCall(ref fun_call) => StmtRunner::run(fun_call.as_ref(), context),
            Statement::FuncDef(ref fun_def) => fun_def.run(context),
            _ => Err(ConvertErr::NotSupportOperator),
        }
    }
}

impl<'a> StmtRunner<'a> for Assignment<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<(), ConvertErr> {
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
    context: &mut (dyn 'b + Ctx<'a>),
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

impl<'a> StmtRunner<'a> for VarAssignment<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<(), ConvertErr> {
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

impl<'a> Runner<'a> for IfThenElse<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        let cond = self.cond.run(context)?;
        let cond_bool = Bool::implicity_from(cond)?;
        if *cond_bool {
            let mut new_context = Context::new(Some(context), ContextType::IfElseBlock);
            self.then_blk.run(&mut new_context)
        } else if let Some(ref else_blk) = self.else_blk {
            let mut new_context = Context::new(Some(context), ContextType::IfElseBlock);
            else_blk.run(&mut new_context)
        } else {
            Ok(Box::new(NA))
        }
    }
}

impl<'a> StmtRunner<'a> for IfThenElse<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<(), ConvertErr> {
        match Runner::run(self, context) {
            Ok(_) => Ok(()),
            Err(e) => Err(e),
        }
    }
}

impl<'a> Runner<'a> for ForRange<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        let iter_name = self.var.0;
        let start = self.start;
        let end = self.end;
        let step = if let Some(s) = self.step {
            s
        } else if start < end {
            1
        } else {
            -1
        };
        let mut iter = start;
        let mut ret_val: Box<dyn PineType<'a> + 'a> = Box::new(NA);
        while (step > 0 && iter < end) || (step < 0 && iter > end) {
            let mut new_context = Context::new(Some(context), ContextType::ForRangeBlock);
            new_context.create_var(iter_name, Box::new(Some(iter)));
            ret_val = self.do_blk.run(&mut new_context)?;
            iter += step;
        }
        Ok(ret_val)
    }
}

impl<'a> StmtRunner<'a> for ForRange<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<(), ConvertErr> {
        match Runner::run(self, context) {
            Ok(_) => Ok(()),
            Err(e) => Err(e),
        }
    }
}

impl<'a> Runner<'a> for FunctionCall<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        let result = self.method.run(context)?;
        match result.get_type() {
            (FirstType::Callable, SecondType::Simple) => {
                let callable = downcast::<Callable>(result).unwrap();
                let mut pos_args = vec![];
                for exp in self.pos_args.iter() {
                    pos_args.push(exp.run(context)?);
                }
                let mut dict_args = vec![];
                for (n, exp) in self.dict_args.iter() {
                    dict_args.push((n.0, exp.run(context)?));
                }
                callable.call(pos_args, dict_args)
            }
            _ => Err(ConvertErr::NotSupportOperator),
        }
    }
}

impl<'a> StmtRunner<'a> for FunctionCall<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<(), ConvertErr> {
        match Runner::run(self, context) {
            Ok(_) => Ok(()),
            Err(e) => Err(e),
        }
    }
}

impl<'a> StmtRunner<'a> for FunctionDef<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<(), ConvertErr> {
        let func = Function::new(self);
        context.create_var(self.name.0, Box::new(func));
        Ok(())
    }
}

impl<'a> Runner<'a> for Block<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<Box<dyn PineType<'a> + 'a>, ConvertErr> {
        for st in self.stmts.iter() {
            st.run(context)?;
        }
        if let Some(ref exp) = self.ret_stmt {
            exp.run(context)
        } else {
            Ok(Box::new(NA))
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
        let test_val = |context: &mut Context, int_val| {
            let val = Int::explicity_from(context.move_var("hello").unwrap());
            assert_eq!(val, Ok(Box::new(Some(int_val))));
            context.update_var("hello", val.unwrap());
        };
        let assign1 = Statement::Assignment(Box::new(Assignment::new(
            VarName("hello"),
            Exp::Num(Numeral::Int(12)),
            false,
            None,
        )));
        let assign2 = Statement::Assignment(Box::new(Assignment::new(
            VarName("hello"),
            Exp::Num(Numeral::Int(23)),
            true,
            None,
        )));
        let assign3 = Statement::Assignment(Box::new(Assignment::new(
            VarName("hello"),
            Exp::Num(Numeral::Int(23)),
            false,
            Some(DataType::Int),
        )));
        let mut context = Context::new(None, ContextType::Normal);

        assert_eq!(assign1.run(&mut context), Ok(()));
        test_val(&mut context, 12);

        context.clear_declare();
        assert_eq!(assign2.run(&mut context), Ok(()));
        test_val(&mut context, 12);

        context.clear_declare();
        assert_eq!(assign3.run(&mut context), Ok(()));
        test_val(&mut context, 23);
    }

    #[test]
    fn var_assignment_test() {
        let assign1 = VarAssignment::new(VarName("hello"), Exp::Num(Numeral::Int(24)));
        let assign2 = VarAssignment::new(VarName("hello"), Exp::Num(Numeral::Int(36)));

        let mut context = Context::new(None, ContextType::Normal);
        context.create_var("hello", Box::new(Some(12)));
        context.create_declare("hello");

        let test_val = |context: &mut Context, int_val| {
            let s: Box<Series<Int>> =
                Series::implicity_from(context.move_var("hello").unwrap()).unwrap();
            assert_eq!(s, Box::new(Series::from(Some(int_val))));
            context.update_var("hello", s);
        };
        assert_eq!(assign1.run(&mut context), Ok(()));
        test_val(&mut context, 24);

        assert_eq!(assign2.run(&mut context), Ok(()));
        test_val(&mut context, 36);
    }

    #[test]
    fn for_range_test() {
        let assign = Statement::Assignment(Box::new(Assignment::new(
            VarName("a"),
            Exp::Num(Numeral::Int(1)),
            false,
            None,
        )));
        let block = Block::new(vec![assign], Some(Exp::Num(Numeral::Int(10))));
        let for_range = ForRange::new(VarName("i"), 1, 10, None, block);

        let mut context = Context::new(None, ContextType::Normal);

        let result = Runner::run(&for_range, &mut context);
        assert!(result.is_ok());

        assert_eq!(Int::implicity_from(result.unwrap()), Ok(Box::new(Some(10))));

        assert!(context.move_var("a").is_none());
    }
}

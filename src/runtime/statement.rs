use super::context::{Context, ContextType, Ctx, RVRunner, Runner, StmtRunner};
use super::function::Function;
use crate::ast::name::VarName;
use crate::ast::stat_expr_types::{
    Assignment, Block, DataType, ForRange, FunctionCall, FunctionDef, IfThenElse, Statement,
    VarAssignment,
};
use crate::types::{
    downcast, Bool, Callable, Color, DataType as FirstType, Float, Int, PineFrom, PineStaticType,
    PineType, RuntimeErr, SecondType, Series, Tuple, NA,
};

impl<'a> StmtRunner<'a> for Statement<'a> {
    fn st_run(&'a self, context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        match *self {
            Statement::None => Ok(()),
            Statement::Break => Err(RuntimeErr::Break),
            Statement::Continue => Err(RuntimeErr::Continue),
            Statement::Assignment(ref assign) => assign.st_run(context),
            Statement::VarAssignment(ref var_assign) => var_assign.st_run(context),
            Statement::Ite(ref ite) => StmtRunner::st_run(ite.as_ref(), context),
            Statement::ForRange(ref fr) => StmtRunner::st_run(fr.as_ref(), context),
            Statement::FuncCall(ref fun_call) => StmtRunner::st_run(fun_call.as_ref(), context),
            Statement::FuncDef(ref fun_def) => fun_def.st_run(context),
        }
    }
}

trait RunnerForName<'a> {
    fn run_name(
        &'a self,
        context: &mut dyn Ctx<'a>,
        name: &VarName<'a>,
        val: Box<dyn PineType<'a> + 'a>,
    ) -> Result<(), RuntimeErr>;
}

fn series_from<'a, D: Default + PineStaticType + PineType<'a> + Clone + PineFrom<'a, D> + 'a>(
    val: Box<dyn PineType<'a> + 'a>,
    name: &'a str,
    context: &mut dyn Ctx<'a>,
) -> Result<(), RuntimeErr> {
    let val = D::implicity_from(val)?;
    let mut s: Box<Series<D>> = Box::new(Series::from(*val));
    context.create_var(name, s);
    Ok(())
}

fn series_update<'a, D: Default + PineStaticType + PineType<'a> + Clone + PineFrom<'a, D> + 'a>(
    series: Box<dyn PineType<'a> + 'a>,
    val: Box<dyn PineType<'a> + 'a>,
    name: &'a str,
    context: &mut dyn Ctx<'a>,
) -> Result<(), RuntimeErr> {
    let mut s: Box<Series<D>> = Series::implicity_from(series)?;
    let val = D::implicity_from(val)?;
    s.update(*val);
    context.update_var(name, s);
    Ok(())
}

impl<'a> RunnerForName<'a> for Assignment<'a> {
    fn run_name(
        &'a self,
        context: &mut dyn Ctx<'a>,
        vn: &VarName<'a>,
        val: Box<dyn PineType<'a> + 'a>,
    ) -> Result<(), RuntimeErr> {
        let name = vn.0;
        if context.contains_declare(name) {
            return Err(RuntimeErr::NameDeclared);
        }
        context.create_declare(name);

        // For variable declare with var type, it only need initialize once.
        if self.var && context.contains_var(name) {
            return Ok(());
        }
        // let val = self.val.rv_run(context)?;
        let true_val: Box<dyn PineType<'a> + 'a> = match self.var_type {
            None => val,
            Some(DataType::Int) => Int::explicity_from(val)?,
            Some(DataType::Bool) => Bool::explicity_from(val)?,
            Some(DataType::Float) => Float::explicity_from(val)?,
            Some(DataType::Color) => Color::explicity_from(val)?,
            Some(DataType::String) => String::explicity_from(val)?,
            _ => return Err(RuntimeErr::InvalidTypeCast),
        };
        if let (FirstType::NA, _) = true_val.get_type() {
            return Err(RuntimeErr::InvalidNADeclarer);
        }
        if true_val.get_type().1 == SecondType::Series {
            return match context.move_var(name) {
                None => match true_val.get_type().0 {
                    FirstType::Int => series_from::<Int>(true_val, name, context),
                    FirstType::Float => series_from::<Float>(true_val, name, context),
                    FirstType::Bool => series_from::<Bool>(true_val, name, context),
                    _ => Err(RuntimeErr::TypeMismatch(String::from(
                        "Series type can only be Int, Float and Bool",
                    ))),
                },
                Some(current_val) => match current_val.get_type().0 {
                    FirstType::Int => series_update::<Int>(current_val, true_val, name, context),
                    FirstType::Float => {
                        series_update::<Float>(current_val, true_val, name, context)
                    }
                    FirstType::Bool => series_update::<Bool>(current_val, true_val, name, context),
                    _ => Err(RuntimeErr::TypeMismatch(String::from(
                        "Series type can only be Int, Float and Bool",
                    ))),
                },
            };
        }
        context.create_var(name, true_val);
        Ok(())
    }
}

impl<'a> StmtRunner<'a> for Assignment<'a> {
    fn st_run(&'a self, context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        let val = self.val.rv_run(context)?;
        if self.names.len() == 1 {
            return self.run_name(context, &self.names[0], val);
        }
        match val.get_type() {
            (FirstType::Tuple, SecondType::Simple) => {
                let mut tuple = downcast::<Tuple>(val)?;

                if tuple.0.len() != self.names.len() {
                    return Err(RuntimeErr::TupleMismatch);
                }

                for n in self.names.iter().rev() {
                    self.run_name(context, n, tuple.0.pop().unwrap())?;
                }
                Ok(())
            }
            _ => Err(RuntimeErr::NotSupportOperator),
        }
    }
}

fn update_series<'a, 'b, D>(
    context: &mut (dyn 'b + Ctx<'a>),
    name: &'a str,
    exist_val: Box<dyn PineType<'a> + 'a>,
    val: Box<dyn PineType<'a> + 'a>,
) -> Result<(), RuntimeErr>
where
    D: Default + PineType<'a> + PineStaticType + 'a + PineFrom<'a, D> + Clone,
{
    let mut s = Series::implicity_from(exist_val)?;
    s.update(*D::implicity_from(val)?);
    context.update_var(name, s);
    Ok(())
}

impl<'a> StmtRunner<'a> for VarAssignment<'a> {
    fn st_run(&'a self, context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        let name = self.name.0;
        if !context.contains_declare(name) {
            return Err(RuntimeErr::NameNotDeclard);
        }
        let val = self.val.rv_run(context)?;
        let exist_val = context.move_var(name).unwrap();
        match exist_val.get_type() {
            (FirstType::Bool, _) => update_series::<Bool>(context, name, exist_val, val),
            (FirstType::Int, _) => update_series::<Int>(context, name, exist_val, val),
            (FirstType::Float, _) => update_series::<Float>(context, name, exist_val, val),
            (FirstType::Color, _) => update_series::<Color>(context, name, exist_val, val),
            (FirstType::String, _) => update_series::<String>(context, name, exist_val, val),
            _ => Err(RuntimeErr::NotSupportOperator),
        }
    }
}

impl<'a> Runner<'a> for IfThenElse<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<Box<dyn PineType<'a> + 'a>, RuntimeErr> {
        let cond = self.cond.rv_run(context)?;
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
    fn st_run(&'a self, context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        match Runner::run(self, context) {
            Ok(_) => Ok(()),
            Err(e) => Err(e),
        }
    }
}

impl<'a> Runner<'a> for ForRange<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<Box<dyn PineType<'a> + 'a>, RuntimeErr> {
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
            match self.do_blk.run(&mut new_context) {
                Ok(val) => {
                    ret_val = val;
                    iter += step;
                }
                Err(RuntimeErr::Break) => {
                    if let Some(ref exp) = self.do_blk.ret_stmt {
                        ret_val = exp.rv_run(&mut new_context)?
                    }
                    break;
                }
                Err(RuntimeErr::Continue) => {
                    if let Some(ref exp) = self.do_blk.ret_stmt {
                        ret_val = exp.rv_run(&mut new_context)?
                    }
                    iter += step;
                    continue;
                }
                e => return e,
            }
        }
        Ok(ret_val)
    }
}

impl<'a> StmtRunner<'a> for ForRange<'a> {
    fn st_run(&'a self, context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        match Runner::run(self, context) {
            Ok(_) => Ok(()),
            Err(e) => Err(e),
        }
    }
}

fn extract_args<'a>(
    context: &mut dyn Ctx<'a>,
    exp: &'a FunctionCall<'a>,
) -> Result<
    (
        Vec<Box<dyn PineType<'a> + 'a>>,
        Vec<(&'a str, Box<dyn PineType<'a> + 'a>)>,
    ),
    RuntimeErr,
> {
    let mut ret_pos = vec![];
    for exp in exp.pos_args.iter() {
        ret_pos.push(exp.rv_run(context)?);
    }
    let mut ret_dict = vec![];
    for (n, exp) in exp.dict_args.iter() {
        ret_dict.push((n.0, exp.rv_run(context)?));
    }
    Ok((ret_pos, ret_dict))
}

impl<'a> Runner<'a> for FunctionCall<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<Box<dyn PineType<'a> + 'a>, RuntimeErr> {
        let result = self.method.rv_run(context)?;
        match result.get_type() {
            (FirstType::Callable, SecondType::Simple) => {
                let callable = downcast::<Callable>(result).unwrap();
                let (pos_args, dict_args) = extract_args(context, self)?;
                let result = callable.call(context, pos_args, dict_args);
                context.create_callable(callable);
                result
            }
            (FirstType::Function, SecondType::Simple) => {
                let callable = downcast::<Function>(result).unwrap();
                let (pos_args, dict_args) = extract_args(context, self)?;
                callable.call(context, pos_args, dict_args)
            }
            _ => Err(RuntimeErr::NotSupportOperator),
        }
    }
}

impl<'a> StmtRunner<'a> for FunctionCall<'a> {
    fn st_run(&'a self, context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        match Runner::run(self, context) {
            Ok(_) => Ok(()),
            Err(e) => Err(e),
        }
    }
}

impl<'a> StmtRunner<'a> for FunctionDef<'a> {
    fn st_run(&'a self, context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        let func = Function::new(self);
        context.create_var(self.name.0, Box::new(func));
        Ok(())
    }
}

impl<'a> Runner<'a> for Block<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<Box<dyn PineType<'a> + 'a>, RuntimeErr> {
        for st in self.stmts.iter() {
            st.st_run(context)?;
        }
        if let Some(ref exp) = self.ret_stmt {
            exp.rv_run(context)
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
            vec![VarName("hello")],
            Exp::Num(Numeral::Int(12)),
            false,
            None,
        )));
        let assign2 = Statement::Assignment(Box::new(Assignment::new(
            vec![VarName("hello")],
            Exp::Num(Numeral::Int(23)),
            true,
            None,
        )));
        let assign3 = Statement::Assignment(Box::new(Assignment::new(
            vec![VarName("hello")],
            Exp::Num(Numeral::Int(23)),
            false,
            Some(DataType::Int),
        )));
        let mut context = Context::new(None, ContextType::Normal);
        context.create_var("newvar", Box::new(Some(100)));

        assert_eq!(assign1.st_run(&mut context), Ok(()));
        test_val(&mut context, 12);

        context.clear_declare();
        assert_eq!(assign2.st_run(&mut context), Ok(()));
        test_val(&mut context, 12);

        context.clear_declare();
        assert_eq!(assign3.st_run(&mut context), Ok(()));
        test_val(&mut context, 23);
    }

    #[test]
    fn rv_assignment_test() {
        let assign = Statement::Assignment(Box::new(Assignment::new(
            vec![VarName("myvar")],
            Exp::VarName(VarName("newvar")),
            false,
            None,
        )));
        let mut context = Context::new(None, ContextType::Normal);
        context.create_var("newvar", Box::new(Some(100)));

        assert_eq!(assign.st_run(&mut context), Ok(()));
        let val = Int::explicity_from(context.move_var("myvar").unwrap());
        assert_eq!(val, Ok(Box::new(Some(100))));
        context.update_var("myvar", val.unwrap());
    }

    #[test]
    fn tuple_assignment_test() {
        let assign = Statement::Assignment(Box::new(Assignment::new(
            vec![VarName("var1"), VarName("var2")],
            Exp::Tuple(Box::new(vec![
                Exp::VarName(VarName("nv1")),
                Exp::VarName(VarName("nv2")),
            ])),
            false,
            None,
        )));
        let mut context = Context::new(None, ContextType::Normal);
        context.create_var("nv1", Box::new(Some(100)));
        context.create_var("nv2", Box::new(Some(200)));

        assert_eq!(assign.st_run(&mut context), Ok(()));
        assert_eq!(
            Int::explicity_from(context.move_var("var1").unwrap()),
            Ok(Box::new(Some(100)))
        );
        assert_eq!(
            Int::explicity_from(context.move_var("var2").unwrap()),
            Ok(Box::new(Some(200)))
        );
    }

    #[test]
    fn var_assignment_test() {
        let assign1 = VarAssignment::new(VarName("hello"), Exp::Num(Numeral::Int(24)));
        let assign2 = VarAssignment::new(VarName("hello"), Exp::Num(Numeral::Int(36)));
        let assign3 = VarAssignment::new(VarName("hello"), Exp::VarName(VarName("newvar")));

        let mut context = Context::new(None, ContextType::Normal);
        context.create_var("hello", Box::new(Some(12)));
        context.create_var("newvar", Box::new(Some(100)));
        context.create_declare("hello");

        let test_val = |context: &mut Context, int_val| {
            let s: Box<Series<Int>> =
                Series::implicity_from(context.move_var("hello").unwrap()).unwrap();
            assert_eq!(s, Box::new(Series::from(Some(int_val))));
            context.update_var("hello", s);
        };
        assert_eq!(assign1.st_run(&mut context), Ok(()));
        test_val(&mut context, 24);

        assert_eq!(assign2.st_run(&mut context), Ok(()));
        test_val(&mut context, 36);

        assert_eq!(assign3.st_run(&mut context), Ok(()));
        test_val(&mut context, 100);
    }

    #[test]
    fn if_then_else_test() {
        let ite = IfThenElse::new(
            Exp::VarName(VarName("cond")),
            Block::new(vec![], Some(Exp::VarName(VarName("then")))),
            Some(Block::new(vec![], Some(Exp::VarName(VarName("else"))))),
        );
        let mut context = Context::new(None, ContextType::Normal);
        context.create_var("cond", Box::new(true));
        context.create_var("then", Box::new(Some(2)));
        context.create_var("else", Box::new(Some(4)));

        assert_eq!(
            downcast::<Int>(ite.run(&mut context).unwrap()),
            Ok(Box::new(Some(2)))
        );
        // If-Then-Else Run as statement
        assert_eq!(ite.st_run(&mut context), Ok(()));
    }

    #[test]
    fn for_range_test() {
        let assign = Statement::Assignment(Box::new(Assignment::new(
            vec![VarName("a")],
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

    #[test]
    fn func_call_exp_test() {
        use std::collections::HashMap;
        let exp = FunctionCall {
            method: Exp::VarName(VarName("name")),
            pos_args: vec![Exp::Bool(true)],
            dict_args: vec![],
        };
        let mut context = Context::new(None, ContextType::Normal);

        fn test_func<'a>(
            _context: &mut dyn Ctx<'a>,
            mut h: HashMap<&'a str, Box<dyn PineType<'a> + 'a>>,
        ) -> Result<Box<dyn PineType<'a> + 'a>, RuntimeErr> {
            match h.remove("arg") {
                None => Err(RuntimeErr::NotValidParam),
                Some(arg) => Ok(arg),
            }
        }

        context.create_var(
            "name",
            Box::new(Callable::new(Some(test_func), None, vec!["arg"])),
        );

        let res = downcast::<Bool>(exp.run(&mut context).unwrap()).unwrap();
        assert_eq!(res, Box::new(true));
    }

    #[test]
    fn func_call_exp2_test() {
        use crate::ast::stat_expr_types::Exp;

        let exp = FunctionCall {
            method: Exp::VarName(VarName("name")),
            pos_args: vec![Exp::Bool(true)],
            dict_args: vec![],
        };
        let def_exp = FunctionDef {
            name: VarName("name"),
            params: vec![VarName("arg")],
            body: Block {
                stmts: vec![],
                ret_stmt: Some(Exp::VarName(VarName("arg"))),
            },
        };
        let mut context = Context::new(None, ContextType::Normal);
        context.create_var("name", Box::new(Function::new(&def_exp)));

        let res = downcast::<Bool>(exp.run(&mut context).unwrap()).unwrap();
        assert_eq!(res, Box::new(true));
    }

    #[test]
    fn for_range_break_test() {
        let block = Block::new(vec![Statement::Break], Some(Exp::VarName(VarName("i"))));
        let for_range = ForRange::new(VarName("i"), 1, 10, None, block);

        let mut context = Context::new(None, ContextType::Normal);

        let result = Runner::run(&for_range, &mut context);
        assert!(result.is_ok());

        assert_eq!(Int::implicity_from(result.unwrap()), Ok(Box::new(Some(1))));

        assert!(context.move_var("a").is_none());
    }

    #[test]
    fn for_range_continue_test() {
        let block = Block::new(vec![Statement::Continue], Some(Exp::VarName(VarName("i"))));
        let for_range = ForRange::new(VarName("i"), 1, 10, None, block);

        let mut context = Context::new(None, ContextType::Normal);

        let result = Runner::run(&for_range, &mut context);
        assert!(result.is_ok());

        assert_eq!(Int::implicity_from(result.unwrap()), Ok(Box::new(Some(9))));

        assert!(context.move_var("a").is_none());
    }
}

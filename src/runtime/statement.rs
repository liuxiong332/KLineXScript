use super::context::{downcast_ctx, Context, ContextType, Ctx, RVRunner, Runner, StmtRunner};
use super::function::Function;
use crate::ast::name::VarName;
use crate::ast::stat_expr_types::{
    Assignment, Block, DataType, ForRange, FunctionCall, FunctionDef, IfThenElse, Statement,
    VarAssignment,
};
use crate::types::{
    downcast_pf, Bool, Callable, Color, DataType as FirstType, Float, Int, PineFrom, PineRef,
    PineStaticType, PineType, RefData, RuntimeErr, SecondType, Series, Tuple, NA,
};
use std::fmt::Debug;

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
        val: PineRef<'a>,
    ) -> Result<(), RuntimeErr>;
}

fn from_series<'a, D>(
    val: PineRef<'a>,
    name: &'a str,
    context: &mut dyn Ctx<'a>,
) -> Result<(), RuntimeErr>
where
    D: Default + PineStaticType + PineType<'a> + PartialEq + Clone + Debug + PineFrom<'a, D> + 'a,
{
    context.create_var(name, val);
    Ok(())
}

pub fn process_assign_val<'a>(
    true_val: PineRef<'a>,
    context: &mut dyn Ctx<'a>,
    name: &'a str,
) -> Result<(), RuntimeErr> {
    if true_val.get_type().1 == SecondType::Series {
        return match context.move_var(name) {
            None => {
                // When assignment, must copy new variable.
                let true_val = true_val.copy_inner();
                match true_val.get_type().0 {
                    FirstType::Int => from_series::<Int>(true_val, name, context),
                    FirstType::Float => from_series::<Float>(true_val, name, context),
                    FirstType::Bool => from_series::<Bool>(true_val, name, context),
                    _ => Err(RuntimeErr::TypeMismatch(format!(
                        "Series type can only be Int, Float and Bool, but get {:?}",
                        true_val.get_type()
                    ))),
                }
            }
            Some(current_val) => match current_val.get_type().0 {
                FirstType::Int => update_series::<Int>(context, name, current_val, true_val),
                FirstType::Float => update_series::<Float>(context, name, current_val, true_val),
                FirstType::Bool => update_series::<Bool>(context, name, current_val, true_val),
                _ => Err(RuntimeErr::TypeMismatch(format!(
                    "Series type can only be Int, Float and Bool, but get {:?}",
                    current_val.get_type()
                ))),
            },
        };
    }
    context.create_var(name, true_val);
    Ok(())
}

impl<'a> RunnerForName<'a> for Assignment<'a> {
    fn run_name(
        &'a self,
        context: &mut dyn Ctx<'a>,
        vn: &VarName<'a>,
        val: PineRef<'a>,
    ) -> Result<(), RuntimeErr> {
        let name = vn.0;
        if context.contains_declare_scope(name) {
            return Err(RuntimeErr::NameDeclared);
        }
        context.create_declare(name);

        // For variable declare with var type, it only need initialize once.
        if self.var && context.contains_var(name) {
            return Ok(());
        }
        // let val = self.val.rv_run(context)?;
        let true_val: PineRef<'a> = match self.var_type {
            None => val,
            Some(DataType::Int) => Int::explicity_from(val)?.into_pf(),
            Some(DataType::Bool) => Bool::explicity_from(val)?.into_pf(),
            Some(DataType::Float) => Float::explicity_from(val)?.into_pf(),
            Some(DataType::Color) => Color::explicity_from(val)?.into_pf(),
            Some(DataType::String) => String::explicity_from(val)?.into_pf(),
            _ => return Err(RuntimeErr::InvalidTypeCast),
        };
        if let (FirstType::NA, _) = true_val.get_type() {
            return Err(RuntimeErr::InvalidNADeclarer);
        }

        process_assign_val(true_val, context, name)
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
                let mut tuple = downcast_pf::<Tuple>(val)?;

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
    exist_val: PineRef<'a>,
    val: PineRef<'a>,
) -> Result<(), RuntimeErr>
where
    D: Default + PineType<'a> + PineStaticType + 'a + PineFrom<'a, D> + Clone + PartialEq + Debug,
{
    let mut s = Series::implicity_from(exist_val)?;
    let v = D::implicity_from(val)?;
    s.update(v.into_inner());
    context.update_var(name, s.into_pf());
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
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, RuntimeErr> {
        let cond = self.cond.rv_run(context)?;
        let cond_bool = Bool::implicity_from(cond)?;
        if *cond_bool {
            let mut new_context = Context::new(Some(context), ContextType::IfElseBlock);
            self.then_blk.run(&mut new_context)
        } else if let Some(ref else_blk) = self.else_blk {
            let mut new_context = Context::new(Some(context), ContextType::IfElseBlock);
            else_blk.run(&mut new_context)
        } else {
            Ok(PineRef::new_box(NA))
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
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, RuntimeErr> {
        let iter_name = self.var.0;
        let start: i32;
        match *Int::implicity_from(self.start.rv_run(context)?)? {
            Some(val) => start = val,
            None => {
                return Err(RuntimeErr::NotCompatible(format!(
                    "The start index of for-range must be not na int, but get na"
                )))
            }
        }
        let end: i32;
        match *Int::implicity_from(self.end.rv_run(context)?)? {
            Some(val) => end = val,
            None => {
                return Err(RuntimeErr::NotCompatible(format!(
                    "The start index of for-range must be not na int, but get na"
                )))
            }
        }
        let step = if let Some(s) = &self.step {
            match *Int::implicity_from(s.rv_run(context)?)? {
                Some(val) => val,
                None => {
                    return Err(RuntimeErr::NotCompatible(format!(
                        "The start index of for-range must be not na int, but get na"
                    )))
                }
            }
        } else if start < end {
            1
        } else {
            -1
        };
        let mut iter = start;
        let mut ret_val: PineRef<'a> = PineRef::new_box(NA);
        while (step > 0 && iter < end) || (step < 0 && iter > end) {
            let mut new_context = Context::new(Some(context), ContextType::ForRangeBlock);
            new_context.create_var(iter_name, PineRef::new_box(Some(iter)));
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
) -> Result<(Vec<PineRef<'a>>, Vec<(&'a str, PineRef<'a>)>), RuntimeErr> {
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
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, RuntimeErr> {
        let result = self.method.rv_run(context)?;
        let ctx_name = format!("@{:?}", self.ctxid);

        let ctx_instance = downcast_ctx(context)?;
        let sub_context;
        // Get or create sub context for the function call context.
        if !ctx_instance.contains_sub_context(&ctx_name) {
            sub_context =
                ctx_instance.create_sub_context(ctx_name.clone(), ContextType::FuncDefBlock);
        } else {
            sub_context = ctx_instance.get_sub_context(&ctx_name).unwrap();
        }
        let ctx_ref = &mut **sub_context;

        match result.get_type() {
            (FirstType::Callable, SecondType::Simple) => {
                let callable = downcast_pf::<Callable>(result).unwrap();
                let (pos_args, dict_args) = extract_args(context, self)?;
                let result = callable.call(context, pos_args, dict_args);
                context.create_callable(callable);
                result
            }
            (FirstType::Function, SecondType::Simple) => {
                let callable = downcast_pf::<Function>(result).unwrap();
                let (pos_args, dict_args) = extract_args(ctx_ref, self)?;
                callable.call(ctx_ref, pos_args, dict_args)
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
        context.create_var(self.name.0, PineRef::new_rc(func));
        Ok(())
    }
}

impl<'a> Runner<'a> for Block<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, RuntimeErr> {
        for st in self.stmts.iter() {
            st.st_run(context)?;
        }
        if let Some(ref exp) = self.ret_stmt {
            exp.rv_run(context)
        } else {
            Ok(PineRef::new_box(NA))
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
            assert_eq!(val, Ok(RefData::new_box(Some(int_val))));
            context.update_var("hello", val.unwrap().into_pf());
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
        context.create_var("newvar", PineRef::new_box(Some(100)));

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
        context.create_var("newvar", PineRef::new_box(Some(100)));

        assert_eq!(assign.st_run(&mut context), Ok(()));
        let val = Int::explicity_from(context.move_var("myvar").unwrap());
        assert_eq!(val, Ok(RefData::new_box(Some(100))));
        context.update_var("myvar", val.unwrap().into_pf());
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
        context.create_var("nv1", PineRef::new_box(Some(100)));
        context.create_var("nv2", PineRef::new_box(Some(200)));

        assert_eq!(assign.st_run(&mut context), Ok(()));
        assert_eq!(
            Int::explicity_from(context.move_var("var1").unwrap()),
            Ok(RefData::new_box(Some(100)))
        );
        assert_eq!(
            Int::explicity_from(context.move_var("var2").unwrap()),
            Ok(RefData::new_box(Some(200)))
        );
    }

    fn check_val<'a>(context: &mut dyn Ctx<'a>, varname: &'static str, val: Int) {
        let res: RefData<Series<Int>> =
            Series::explicity_from(context.move_var(varname).unwrap()).unwrap();
        let series: Series<Int> = Series::from(val);
        let expect_val: RefData<Series<Int>> = RefData::new_rc(series);
        assert_eq!(res, expect_val);
        context.update_var("myvar", res.into_pf());
    }

    #[test]
    fn series_assignment_test() {
        let assign = Statement::Assignment(Box::new(Assignment::new(
            vec![VarName("myvar")],
            Exp::VarName(VarName("newvar")),
            false,
            None,
        )));
        let mut context = Context::new(None, ContextType::Normal);
        context.create_var("newvar", PineRef::new_rc(Series::from(Some(100))));

        assert_eq!(assign.st_run(&mut context), Ok(()));
        check_val(&mut context, "myvar", Some(100));
        context.commit();

        context.clear_declare();
        context.create_var("newvar", PineRef::new_rc(Series::from(Some(100))));
        assert_eq!(assign.st_run(&mut context), Ok(()));

        let val: RefData<Series<Int>> =
            Series::explicity_from(context.move_var("myvar").unwrap()).unwrap();
        let mut dest_s = RefData::new_rc(Series::from_vec(vec![Some(100)]));
        dest_s.update(Some(100));
        assert_eq!(val, dest_s);
        context.update_var("myvar", val.into_pf());
    }

    #[test]
    fn var_assignment_test() {
        let assign1 = VarAssignment::new(VarName("hello"), Exp::Num(Numeral::Int(24)));
        let assign2 = VarAssignment::new(VarName("hello"), Exp::Num(Numeral::Int(36)));
        let assign3 = VarAssignment::new(VarName("hello"), Exp::VarName(VarName("newvar")));

        let mut context = Context::new(None, ContextType::Normal);
        context.create_var("hello", PineRef::new_box(Some(12)));
        context.create_var("newvar", PineRef::new_box(Some(100)));
        context.create_declare("hello");

        let test_val = |context: &mut Context, int_val| {
            let s: RefData<Series<Int>> =
                Series::implicity_from(context.move_var("hello").unwrap()).unwrap();
            assert_eq!(s, RefData::new_rc(Series::from(Some(int_val))));
            context.update_var("hello", s.into_pf());
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
        let ite = IfThenElse::new_no_ctxid(
            Exp::VarName(VarName("cond")),
            Block::new(vec![], Some(Exp::VarName(VarName("then")))),
            Some(Block::new(vec![], Some(Exp::VarName(VarName("else"))))),
        );
        let mut context = Context::new(None, ContextType::Normal);
        context.create_var("cond", PineRef::new_box(true));
        context.create_var("then", PineRef::new_box(Some(2)));
        context.create_var("else", PineRef::new_box(Some(4)));

        assert_eq!(
            downcast_pf::<Int>(ite.run(&mut context).unwrap()),
            Ok(RefData::new_box(Some(2)))
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
        let for_range = ForRange::new_no_ctxid(
            VarName("i"),
            Exp::Num(Numeral::Int(1)),
            Exp::Num(Numeral::Int(10)),
            None,
            block,
        );

        let mut context = Context::new(None, ContextType::Normal);

        let result = Runner::run(&for_range, &mut context);
        assert!(result.is_ok());

        assert_eq!(
            Int::implicity_from(result.unwrap()),
            Ok(RefData::new_box(Some(10)))
        );

        assert!(context.move_var("a").is_none());
    }

    #[test]
    fn for_range_exp_test() {
        let assign = Statement::Assignment(Box::new(Assignment::new(
            vec![VarName("a")],
            Exp::Num(Numeral::Int(1)),
            false,
            None,
        )));
        let block = Block::new(vec![assign], Some(Exp::Num(Numeral::Int(10))));
        let for_range = ForRange::new_no_ctxid(
            VarName("i"),
            Exp::VarName(VarName("start")),
            Exp::VarName(VarName("end")),
            Some(Exp::VarName(VarName("step"))),
            block,
        );

        let mut context = Context::new(None, ContextType::Normal);
        context.create_var("start", PineRef::new(Some(1)));
        context.create_var("end", PineRef::new(Some(10)));
        context.create_var("step", PineRef::new(Some(5)));

        let result = Runner::run(&for_range, &mut context);
        assert!(result.is_ok());

        assert_eq!(
            Int::implicity_from(result.unwrap()),
            Ok(RefData::new_box(Some(10)))
        );

        assert!(context.move_var("a").is_none());
    }

    #[test]
    fn func_call_exp_test() {
        use std::collections::HashMap;
        let exp = FunctionCall::new_no_ctxid(
            Exp::VarName(VarName("name")),
            vec![Exp::Bool(true)],
            vec![],
        );
        let mut context = Context::new(None, ContextType::Normal);

        fn test_func<'a>(
            _context: &mut dyn Ctx<'a>,
            mut h: HashMap<&'a str, PineRef<'a>>,
        ) -> Result<PineRef<'a>, RuntimeErr> {
            match h.remove("arg") {
                None => Err(RuntimeErr::NotValidParam),
                Some(arg) => Ok(arg),
            }
        }

        context.create_var(
            "name",
            PineRef::new_rc(Callable::new(Some(test_func), None, vec!["arg"])),
        );

        let res = downcast_pf::<Bool>(exp.run(&mut context).unwrap()).unwrap();
        assert_eq!(res, RefData::new_box(true));
    }

    #[test]
    fn func_call_exp2_test() {
        use crate::ast::stat_expr_types::Exp;

        let exp = FunctionCall::new_no_ctxid(
            Exp::VarName(VarName("name")),
            vec![Exp::Bool(true)],
            vec![],
        );
        let def_exp = FunctionDef {
            name: VarName("name"),
            params: vec![VarName("arg")],
            body: Block {
                stmts: vec![],
                ret_stmt: Some(Exp::VarName(VarName("arg"))),
            },
        };
        let mut context = Context::new(None, ContextType::Normal);
        context.create_var("name", PineRef::new_rc(Function::new(&def_exp)));

        let res = downcast_pf::<Bool>(exp.run(&mut context).unwrap()).unwrap();
        assert_eq!(res, RefData::new_box(true));
    }

    #[test]
    fn for_range_break_test() {
        let block = Block::new(vec![Statement::Break], Some(Exp::VarName(VarName("i"))));
        let for_range = ForRange::new_no_ctxid(
            VarName("i"),
            Exp::Num(Numeral::Int(1)),
            Exp::Num(Numeral::Int(10)),
            None,
            block,
        );

        let mut context = Context::new(None, ContextType::Normal);

        let result = Runner::run(&for_range, &mut context);
        assert!(result.is_ok());

        assert_eq!(
            Int::implicity_from(result.unwrap()),
            Ok(RefData::new_box(Some(1)))
        );

        assert!(context.move_var("a").is_none());
    }

    #[test]
    fn for_range_continue_test() {
        let block = Block::new(vec![Statement::Continue], Some(Exp::VarName(VarName("i"))));
        let for_range = ForRange::new_no_ctxid(
            VarName("i"),
            Exp::Num(Numeral::Int(1)),
            Exp::Num(Numeral::Int(10)),
            None,
            block,
        );

        let mut context = Context::new(None, ContextType::Normal);

        let result = Runner::run(&for_range, &mut context);
        assert!(result.is_ok());

        assert_eq!(
            Int::implicity_from(result.unwrap()),
            Ok(RefData::new_box(Some(9)))
        );

        assert!(context.move_var("a").is_none());
    }
}

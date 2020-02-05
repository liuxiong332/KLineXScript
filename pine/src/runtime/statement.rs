use super::context::{
    downcast_ctx, ContextType, Ctx, PineRuntimeError, RVRunner, Runner, StmtRunner, VarOperate,
};
use super::function::Function;
use super::runtime_convert::convert;
use crate::ast::input::StrRange;
use crate::ast::name::VarName;
use crate::ast::stat_expr_types::{
    Assignment, Block, DataType, ForRange, FunctionCall, FunctionDef, IfThenElse, Statement,
    VarAssignment, VarIndex,
};
use crate::types::{
    downcast_pf, Bool, Callable, CallableFactory, Color, DataType as FirstType, Float, Int,
    PineFrom, PineRef, PineStaticType, PineType, RefData, RuntimeErr, SecondType, Series, Tuple,
    NA,
};
use std::fmt::Debug;

impl<'a> StmtRunner<'a> for Statement<'a> {
    fn st_run(&'a self, context: &mut dyn Ctx<'a>) -> Result<(), PineRuntimeError> {
        match *self {
            Statement::None(_) => Ok(()),
            Statement::Break(range) => Err(PineRuntimeError::new(RuntimeErr::Break, range)),
            Statement::Continue(range) => Err(PineRuntimeError::new(RuntimeErr::Continue, range)),
            Statement::Assignment(ref assign) => assign.st_run(context),
            Statement::VarAssignment(ref var_assign) => var_assign.st_run(context),
            Statement::Ite(ref ite) => StmtRunner::st_run(ite.as_ref(), context),
            Statement::ForRange(ref fr) => StmtRunner::st_run(fr.as_ref(), context),
            Statement::FuncCall(ref fun_call) => StmtRunner::st_run(fun_call.as_ref(), context),
            Statement::FuncDef(ref fun_def) => fun_def.st_run(context),
            Statement::Exp(ref exp) => {
                exp.run(context)?;
                Ok(())
            }
        }
    }
}

trait RunnerForName<'a> {
    fn run_name(
        &'a self,
        context: &mut dyn Ctx<'a>,
        name: &VarName<'a>,
        val: PineRef<'a>,
        varid: i32,
    ) -> Result<PineRef<'a>, RuntimeErr>;
}

// fn from_series<'a, D>(
//     val: PineRef<'a>,
//     name: &'a str,
//     context: &mut dyn Ctx<'a>,
// ) -> Result<(), RuntimeErr>
// where
//     D: Default + PineStaticType + PineType<'a> + PartialEq + Clone + Debug + PineFrom<'a, D> + 'a,
// {
//     context.create_var(name, val);
//     Ok(())
// }

pub fn process_assign_val<'a>(
    true_val: PineRef<'a>,
    context: &mut dyn VarOperate<'a>,
    varid: i32,
) -> Result<PineRef<'a>, RuntimeErr> {
    let index = VarIndex::new(varid, 0);
    match context.move_var(index) {
        None => {
            // When assignment, must copy new variable.
            let true_val = true_val.copy_inner();
            context.create_var(varid, true_val.clone());
            Ok(true_val)
        }
        Some(current_val) => match (current_val.get_type(), true_val.get_type()) {
            ((FirstType::Bool, SecondType::Series), _)
            | (_, (FirstType::Bool, SecondType::Series)) => {
                update_series::<Bool>(context, index, current_val, true_val)
            }
            ((FirstType::Float, SecondType::Series), _)
            | (_, (FirstType::Float, SecondType::Series)) => {
                update_series::<Float>(context, index, current_val, true_val)
            }
            ((FirstType::Int, SecondType::Series), _)
            | (_, (FirstType::Int, SecondType::Series)) => {
                update_series::<Int>(context, index, current_val, true_val)
            }
            ((FirstType::Color, SecondType::Series), _)
            | (_, (FirstType::Color, SecondType::Series)) => {
                update_series::<Color>(context, index, current_val, true_val)
            }
            ((FirstType::String, SecondType::Series), _)
            | (_, (FirstType::String, SecondType::Series)) => {
                update_series::<String>(context, index, current_val, true_val)
            }
            ((_, SecondType::Series), _) | (_, (_, SecondType::Series)) => {
                // Err(RuntimeErr::TypeMismatch(format!(
                //     "Series type can only be Int, Float, Bool, Color, String, but get {:?}",
                //     current_val.get_type()
                // )))
                Err(RuntimeErr::UnknownRuntimeErr)
            }
            ((FirstType::Bool, SecondType::Simple), _)
            | (_, (FirstType::Bool, SecondType::Simple)) => {
                let val = Bool::implicity_from(true_val)?.into_pf();
                context.create_var(varid, val.clone());
                Ok(val)
            }
            ((FirstType::Float, SecondType::Simple), _)
            | (_, (FirstType::Float, SecondType::Simple)) => {
                let val = Float::implicity_from(true_val)?.into_pf();
                context.create_var(varid, val.clone());
                Ok(val)
            }
            ((FirstType::Int, SecondType::Simple), _)
            | (_, (FirstType::Int, SecondType::Simple)) => {
                let val = Int::implicity_from(true_val)?.into_pf();
                context.create_var(varid, val.clone());
                Ok(val)
            }
            ((FirstType::Color, SecondType::Simple), _)
            | (_, (FirstType::Color, SecondType::Simple)) => {
                let val = Color::implicity_from(true_val)?.into_pf();
                context.create_var(varid, val.clone());
                Ok(val)
            }
            ((FirstType::String, SecondType::Simple), _)
            | (_, (FirstType::String, SecondType::Simple)) => {
                let val = String::implicity_from(true_val)?.into_pf();
                context.create_var(varid, val.clone());
                Ok(val)
            }
            _ => {
                // return Err(RuntimeErr::TypeMismatch(format!(
                //     "Variable type can only be Int, Float, Bool, Color, String, but get {:?}",
                //     true_val.get_type()
                // )))
                Err(RuntimeErr::UnknownRuntimeErr)
            }
        },
    }
}

fn update_series<'a, 'b, D>(
    context: &mut (dyn 'b + VarOperate<'a>),
    var_index: VarIndex,
    exist_val: PineRef<'a>,
    val: PineRef<'a>,
) -> Result<PineRef<'a>, RuntimeErr>
where
    D: Default + PineType<'a> + PineStaticType + 'a + PineFrom<'a, D> + Clone + PartialEq + Debug,
{
    let mut s = Series::implicity_from(exist_val)?;
    let v = D::implicity_from(val)?;
    s.update(v.clone_inner());
    context.update_var(var_index, s.into_pf());
    Ok(v.into_pf())
}

fn update_series_range<'a, 'b, D>(
    context: &mut (dyn 'b + VarOperate<'a>),
    var_index: VarIndex,
    exist_val: PineRef<'a>,
    val: PineRef<'a>,
    range: StrRange,
) -> Result<PineRef<'a>, PineRuntimeError>
where
    D: Default + PineType<'a> + PineStaticType + 'a + PineFrom<'a, D> + Clone + PartialEq + Debug,
{
    match update_series::<D>(context, var_index, exist_val, val) {
        Ok(val) => Ok(val),
        Err(code) => Err(PineRuntimeError::new(code, range)),
    }
}

impl<'a> RunnerForName<'a> for Assignment<'a> {
    fn run_name(
        &'a self,
        context: &mut dyn Ctx<'a>,
        vn: &VarName<'a>,
        val: PineRef<'a>,
        varid: i32,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        // let name = vn.value;
        // if context.contains_declare_scope(name) {
        //     return Err(RuntimeErr::NameDeclared);
        // }
        // context.create_declare(name);

        // For variable declare with var type, it only need initialize once.
        if self.var && context.contains_var_scope(varid) {
            return Ok(context
                .get_var(VarIndex::new(varid, 0))
                .as_ref()
                .unwrap()
                .clone());
        }
        // let val = self.val.rv_run(context)?;
        let true_val: PineRef<'a> = match self.var_type {
            None => val,
            Some(DataType::Int) => match val.get_type().1 {
                SecondType::Series => Series::<Int>::implicity_from(val)?.into_pf(),
                _ => Int::implicity_from(val)?.into_pf(),
            },
            Some(DataType::Bool) => match val.get_type().1 {
                SecondType::Series => Series::<Bool>::implicity_from(val)?.into_pf(),
                _ => Bool::implicity_from(val)?.into_pf(),
            },
            Some(DataType::Float) => match val.get_type().1 {
                SecondType::Series => Series::<Float>::implicity_from(val)?.into_pf(),
                _ => Float::implicity_from(val)?.into_pf(),
            },
            Some(DataType::Color) => match val.get_type().1 {
                SecondType::Series => Series::<Color>::implicity_from(val)?.into_pf(),
                _ => Color::implicity_from(val)?.into_pf(),
            },
            Some(DataType::String) => match val.get_type().1 {
                SecondType::Series => Series::<String>::implicity_from(val)?.into_pf(),
                _ => String::implicity_from(val)?.into_pf(),
            },
        };
        if let (FirstType::NA, _) = true_val.get_type() {
            return Err(RuntimeErr::InvalidNADeclarer);
        }

        process_assign_val(true_val, downcast_ctx(context), varid)
    }
}

impl<'a> Runner<'a> for Assignment<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, PineRuntimeError> {
        let val = self.val.rv_run(context)?;
        if self.names.len() == 1 {
            let varid = self.varids.as_ref().unwrap()[0];
            return match self.run_name(context, &self.names[0], val, varid) {
                Err(code) => Err(PineRuntimeError::new(code, self.range)),
                Ok(val) => Ok(val),
            };
        }
        match val.get_type() {
            (FirstType::Tuple, SecondType::Simple) => {
                let mut tuple = downcast_pf::<Tuple>(val).unwrap();

                if tuple.0.len() != self.names.len() {
                    return Err(PineRuntimeError::new(
                        RuntimeErr::UnknownRuntimeErr,
                        self.range,
                    ));
                    // return Err(PineRuntimeError::new(RuntimeErr::TupleMismatch, self.range));
                }

                let varids = self.varids.as_ref().unwrap().iter();
                let mut ret_val = vec![];
                for (n, varid) in self.names.iter().zip(varids).rev() {
                    match self.run_name(context, n, tuple.0.pop().unwrap(), *varid) {
                        Err(code) => {
                            return Err(PineRuntimeError::new(code, self.range));
                        }
                        Ok(val) => ret_val.push(val),
                    }
                }
                Ok(PineRef::new_box(Tuple(ret_val)))
            }
            _ => Err(PineRuntimeError::new(
                RuntimeErr::NotSupportOperator,
                self.range,
            )),
        }
    }
}

impl<'a> StmtRunner<'a> for Assignment<'a> {
    fn st_run(&'a self, context: &mut dyn Ctx<'a>) -> Result<(), PineRuntimeError> {
        self.run(context)?;
        Ok(())
    }
}

impl<'a> Runner<'a> for VarAssignment<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, PineRuntimeError> {
        let val = self.val.rv_run(context)?;

        let index = self.var_index;
        let exist_val = context.move_var(index).unwrap();
        let ctx_instance = downcast_ctx(context);
        match exist_val.get_type() {
            (FirstType::Bool, _) => {
                update_series_range::<Bool>(ctx_instance, index, exist_val, val, self.range)
            }
            (FirstType::Int, _) => {
                update_series_range::<Int>(ctx_instance, index, exist_val, val, self.range)
            }
            (FirstType::Float, _) => {
                update_series_range::<Float>(ctx_instance, index, exist_val, val, self.range)
            }
            (FirstType::Color, _) => {
                update_series_range::<Color>(ctx_instance, index, exist_val, val, self.range)
            }
            (FirstType::String, _) => {
                update_series_range::<String>(ctx_instance, index, exist_val, val, self.range)
            }
            _ => Err(PineRuntimeError::new(
                RuntimeErr::NotSupportOperator,
                self.range,
            )),
        }
    }
}

impl<'a> StmtRunner<'a> for VarAssignment<'a> {
    fn st_run(&'a self, context: &mut dyn Ctx<'a>) -> Result<(), PineRuntimeError> {
        self.run(context)?;
        Ok(())
    }
}

impl<'a> Runner<'a> for IfThenElse<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, PineRuntimeError> {
        let cond = self.cond.rv_run(context)?;
        let cond_bool = Bool::implicity_from(cond).unwrap();
        if *cond_bool {
            let subctx = create_sub_ctx(
                context,
                self.then_ctxid,
                ContextType::IfElseBlock,
                self.get_then_var_count(),
                self.get_then_libfun_count(),
                self.get_then_subctx_count(),
            );
            let result = self.then_blk.run(subctx);
            subctx.set_is_run(true);
            Ok(convert(result?, &self.result_type))
        } else if let Some(ref else_blk) = self.else_blk {
            let subctx = create_sub_ctx(
                context,
                self.else_ctxid,
                ContextType::IfElseBlock,
                self.get_else_var_count(),
                self.get_else_libfun_count(),
                self.get_else_subctx_count(),
            );
            let result = else_blk.run(subctx);
            subctx.set_is_run(true);
            Ok(convert(result?, &self.result_type))
        } else {
            Ok(convert(PineRef::new_box(NA), &self.result_type))
        }
    }
}

impl<'a> StmtRunner<'a> for IfThenElse<'a> {
    fn st_run(&'a self, context: &mut dyn Ctx<'a>) -> Result<(), PineRuntimeError> {
        match Runner::run(self, context) {
            Ok(_) => Ok(()),
            Err(e) => Err(e),
        }
    }
}

fn extract_int(val: Int, range: StrRange) -> Result<i32, PineRuntimeError> {
    match val {
        Some(ival) => Ok(ival),
        None => Err(PineRuntimeError::new(RuntimeErr::ForRangeIndexIsNA, range)),
    }
}

impl<'a> Runner<'a> for ForRange<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, PineRuntimeError> {
        let start = extract_int(
            *Int::implicity_from(self.start.rv_run(context)?).unwrap(),
            self.range,
        )?;
        let end = extract_int(
            *Int::implicity_from(self.end.rv_run(context)?).unwrap(),
            self.range,
        )?;
        let step = if let Some(s) = &self.step {
            extract_int(
                *Int::implicity_from(s.rv_run(context)?).unwrap(),
                self.range,
            )?
        } else if start < end {
            1
        } else {
            -1
        };
        let mut iter = start;
        let mut ret_val: PineRef<'a> = PineRef::new_box(NA);
        let subctx = create_sub_ctx(
            context,
            self.ctxid,
            ContextType::ForRangeBlock,
            self.get_var_count(),
            self.get_libfun_count(),
            self.get_subctx_count(),
        );
        while (step > 0 && iter < end) || (step < 0 && iter > end) {
            subctx.create_var(self.varid, PineRef::new_box(Some(iter)));

            match self.do_blk.run(subctx) {
                Ok(val) => {
                    ret_val = val;
                    iter += step;
                }
                Err(PineRuntimeError {
                    code: RuntimeErr::Break,
                    range: _,
                }) => {
                    if let Some(ref exp) = self.do_blk.ret_stmt {
                        ret_val = exp.rv_run(subctx)?
                    }
                    break;
                }
                Err(PineRuntimeError {
                    code: RuntimeErr::Continue,
                    range: _,
                }) => {
                    if let Some(ref exp) = self.do_blk.ret_stmt {
                        ret_val = exp.rv_run(subctx)?
                    }
                    iter += step;
                    continue;
                }
                e => return e,
            }
            // subctx.clear_declare();
        }
        subctx.set_is_run(true);
        // update_sub_ctx(context, self.ctxid, subctx);
        Ok(convert(ret_val, &self.result_type))
    }
}

impl<'a> StmtRunner<'a> for ForRange<'a> {
    fn st_run(&'a self, context: &mut dyn Ctx<'a>) -> Result<(), PineRuntimeError> {
        match Runner::run(self, context) {
            Ok(_) => Ok(()),
            Err(e) => Err(e),
        }
    }
}

fn extract_args<'a>(
    context: &mut dyn Ctx<'a>,
    exp: &'a FunctionCall<'a>,
) -> Result<(Vec<PineRef<'a>>, Vec<(&'a str, PineRef<'a>)>), PineRuntimeError> {
    let mut ret_pos = vec![];
    for exp in exp.pos_args.iter() {
        ret_pos.push(exp.rv_run(context)?);
    }
    let mut ret_dict = vec![];
    for (n, exp) in exp.dict_args.iter() {
        ret_dict.push((n.value, exp.rv_run(context)?));
    }
    Ok((ret_pos, ret_dict))
}

pub fn create_sub_ctx<'a, 'b, 'c>(
    context: &'c mut (dyn Ctx<'a> + 'c),
    ctxid: i32,
    ctx_type: ContextType,
    var_count: i32,
    libfun_count: i32,
    subctx_count: i32,
) -> &'c mut (dyn Ctx<'a> + 'c) {
    let ctx_instance = downcast_ctx(context);
    let sub_context;
    // Get or create sub context for the function call context.
    if !ctx_instance.contains_sub_context(ctxid) {
        sub_context =
            ctx_instance.create_sub_context(ctxid, ctx_type, var_count, subctx_count, libfun_count);
    } else {
        sub_context = ctx_instance.get_sub_context(ctxid).unwrap();
    }
    &mut **sub_context
}

pub fn update_sub_ctx<'a, 'b, 'c>(
    context: &'c mut (dyn Ctx<'a> + 'c),
    ctxid: i32,
    subctx: Box<dyn Ctx<'a> + 'c>,
) {
    // let ctx_name = format!("@{:?}", ctxid);
    let ctx_instance = downcast_ctx(context);
    ctx_instance.update_sub_context(ctxid, subctx);
}

pub fn get_sub_ctx<'a, 'b, 'c>(
    context: &'c mut (dyn Ctx<'a> + 'c),
    ctxid: i32,
) -> &'c mut (dyn Ctx<'a> + 'c) {
    // let ctx_name = format!("@{:?}", ctxid);
    let ctx_instance = downcast_ctx(context);
    let sub_context = ctx_instance.get_sub_context(ctxid).unwrap();
    &mut **sub_context
}

impl<'a> Runner<'a> for FunctionCall<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, PineRuntimeError> {
        let result = self.method.rv_run(context)?;

        let (pos_args, dict_args) = extract_args(context, self)?;

        let result = match result.get_type() {
            (FirstType::Callable, SecondType::Simple) => {
                let mut callable = downcast_pf::<Callable>(result).unwrap();
                // let ctx_ref = create_sub_ctx(context, self.ctxid, ContextType::FuncDefBlock, 0, 0);
                let func_type = self.func_type.as_ref().unwrap().clone();
                let result = callable.call(context, pos_args, dict_args, func_type);
                // ctx_ref.set_is_run(true);
                context.create_callable(callable);
                result
            }
            (FirstType::CallableFactory, SecondType::Simple) => {
                let mut opt_instance = context.move_fun_instance(self.ctxid);
                if opt_instance.is_none() {
                    let factory = downcast_pf::<CallableFactory>(result).unwrap();
                    context.create_fun_instance(self.ctxid, RefData::new_rc(factory.create()));
                    opt_instance = context.move_fun_instance(self.ctxid);
                }
                let mut callable = opt_instance.unwrap();

                let func_type = self.func_type.as_ref().unwrap().clone();
                let result = callable.call(context, pos_args, dict_args, func_type);

                context.create_fun_instance(self.ctxid, RefData::clone(&callable));
                context.create_callable(callable);
                result
            }
            (FirstType::Function, SecondType::Simple) => {
                let callable = downcast_pf::<Function>(result).unwrap();
                let def = callable.get_def();
                let true_def = &def.spec_defs.as_ref().unwrap()[self.spec_index as usize];
                let function = Function::new(true_def);
                let ctx_ref = create_sub_ctx(
                    context,
                    self.ctxid,
                    ContextType::FuncDefBlock,
                    function.get_var_count(),
                    function.get_libfun_count(),
                    function.get_subctx_count(),
                );
                let result = function.call(ctx_ref, pos_args, dict_args, self.range);
                ctx_ref.set_is_run(true);
                return result;
            }
            _ => Err(RuntimeErr::NotSupportOperator),
        };
        match result {
            Ok(val) => Ok(val),
            Err(code) => Err(PineRuntimeError::new(code, self.range)),
        }
    }
}

impl<'a> StmtRunner<'a> for FunctionCall<'a> {
    fn st_run(&'a self, context: &mut dyn Ctx<'a>) -> Result<(), PineRuntimeError> {
        match Runner::run(self, context) {
            Ok(_) => Ok(()),
            Err(e) => Err(e),
        }
    }
}

impl<'a> StmtRunner<'a> for FunctionDef<'a> {
    fn st_run(&'a self, context: &mut dyn Ctx<'a>) -> Result<(), PineRuntimeError> {
        let func = Function::new(self);
        context.create_var(self.name_varid, PineRef::new_rc(func));
        Ok(())
    }
}

impl<'a> Runner<'a> for Block<'a> {
    fn run(&'a self, context: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, PineRuntimeError> {
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
    use crate::ast::input::StrRange;
    use crate::ast::name::VarName;
    use crate::ast::num::Numeral;
    use crate::ast::stat_expr_types::{BoolNode, RVVarName, TupleNode};
    use crate::ast::syntax_type::{SimpleSyntaxType, SyntaxType};
    use crate::runtime::context::Context;
    use crate::runtime::exp::Exp;
    use crate::syntax::SyntaxParser;
    use crate::types::RefData;

    #[test]
    fn assignment_test() {
        let test_val = |context: &mut Context, int_val| {
            let val = Int::explicity_from(context.move_var(VarIndex::new(1, 0)).unwrap());
            assert_eq!(val, Ok(RefData::new_box(Some(int_val))));
            context.update_var(VarIndex::new(1, 0), val.unwrap().into_pf());
        };
        let assign1 = Statement::Assignment(Box::new(Assignment::new_with_varids(
            vec![VarName::new_no_input("hello")],
            Exp::Num(Numeral::from_i32(12)),
            false,
            None,
            vec![1],
        )));
        let assign2 = Statement::Assignment(Box::new(Assignment::new_with_varids(
            vec![VarName::new_no_input("hello")],
            Exp::Num(Numeral::from_i32(23)),
            true,
            None,
            vec![1],
        )));
        let assign3 = Statement::Assignment(Box::new(Assignment::new_with_varids(
            vec![VarName::new_no_input("hello")],
            Exp::Num(Numeral::from_i32(23)),
            false,
            Some(DataType::Int),
            vec![1],
        )));
        let mut context = Context::new(None, ContextType::Normal);
        context.init_vars(vec![
            Some(PineRef::new_box(Some(100))),
            Some(PineRef::new_box(Some(1))),
        ]);

        assert_eq!(assign1.st_run(&mut context), Ok(()));
        test_val(&mut context, 12);

        assert_eq!(assign2.st_run(&mut context), Ok(()));
        test_val(&mut context, 12);

        assert_eq!(assign3.st_run(&mut context), Ok(()));
        test_val(&mut context, 23);
    }

    #[test]
    fn rv_assignment_test() {
        let myvar_index = VarIndex::new(1, 0);
        let newvar_index = VarIndex::new(0, 0);
        let assign = Statement::Assignment(Box::new(Assignment::new_with_varids(
            vec![VarName::new_no_input("myvar")],
            Exp::VarName(RVVarName::new_with_index("newvar", newvar_index)),
            false,
            None,
            vec![myvar_index.varid],
        )));
        let mut context = Context::new(None, ContextType::Normal);
        context.init_vars(vec![
            Some(PineRef::new_box(Some(100))), // newvar
            Some(PineRef::new(Some(1))),       // myvar
        ]);

        assert_eq!(assign.st_run(&mut context), Ok(()));
        let val = Int::explicity_from(context.move_var(myvar_index).unwrap());
        assert_eq!(val, Ok(RefData::new_box(Some(100))));
        context.update_var(myvar_index, val.unwrap().into_pf());
    }

    #[test]
    fn tuple_assignment_test() {
        let var1_index = VarIndex::new(0, 0);
        let var2_index = VarIndex::new(1, 0);
        let nv1_index = VarIndex::new(2, 0);
        let nv2_index = VarIndex::new(3, 0);
        let assign = Statement::Assignment(Box::new(Assignment::new_with_varids(
            vec![VarName::new_no_input("var1"), VarName::new_no_input("var2")],
            Exp::Tuple(Box::new(TupleNode::new(
                vec![
                    Exp::VarName(RVVarName::new_with_index("nv1", nv1_index)),
                    Exp::VarName(RVVarName::new_with_index("nv2", nv2_index)),
                ],
                StrRange::new_empty(),
            ))),
            false,
            None,
            vec![var1_index.varid, var2_index.varid],
        )));
        let mut context = Context::new(None, ContextType::Normal);
        context.init(4, 0, 0);
        context.create_var(nv1_index.varid, PineRef::new_box(Some(100)));
        context.create_var(nv2_index.varid, PineRef::new_box(Some(200)));

        assert_eq!(assign.st_run(&mut context), Ok(()));
        assert_eq!(
            Int::explicity_from(context.move_var(var1_index).unwrap()),
            Ok(RefData::new_box(Some(100)))
        );
        assert_eq!(
            Int::explicity_from(context.move_var(var2_index).unwrap()),
            Ok(RefData::new_box(Some(200)))
        );
    }

    fn check_val<'a>(context: &mut dyn Ctx<'a>, var_index: VarIndex, val: Int) {
        let res: RefData<Series<Int>> =
            Series::explicity_from(context.move_var(var_index).unwrap()).unwrap();
        let series: Series<Int> = Series::from(val);
        let expect_val: RefData<Series<Int>> = RefData::new_rc(series);
        assert_eq!(res, expect_val);
        context.update_var(var_index, res.into_pf());
    }

    #[test]
    fn series_assignment_test() {
        let myvar_index = VarIndex::new(0, 0);
        let newvar_index = VarIndex::new(1, 0);
        let assign = Statement::Assignment(Box::new(Assignment::new_with_varids(
            vec![VarName::new_no_input("myvar")],
            Exp::VarName(RVVarName::new_with_index("newvar", newvar_index)),
            false,
            None,
            vec![myvar_index.varid],
        )));
        let mut context = Context::new(None, ContextType::Normal);
        context.init(2, 0, 0);
        context.create_var(newvar_index.varid, PineRef::new_rc(Series::from(Some(100))));

        assert_eq!(assign.st_run(&mut context), Ok(()));
        check_val(&mut context, myvar_index, Some(100));
        context.commit();

        context.create_var(newvar_index.varid, PineRef::new_rc(Series::from(Some(100))));
        assert_eq!(assign.st_run(&mut context), Ok(()));

        let val: RefData<Series<Int>> =
            Series::explicity_from(context.move_var(myvar_index).unwrap()).unwrap();
        let mut dest_s = RefData::new_rc(Series::from_vec(vec![Some(100)]));
        dest_s.update(Some(100));
        assert_eq!(val, dest_s);
        context.update_var(myvar_index, val.into_pf());
    }

    #[test]
    fn var_assignment_test() {
        let hello_index = VarIndex::new(0, 0);
        let newvar_index = VarIndex::new(1, 0);
        let assign1 = VarAssignment::new_with_index(
            VarName::new_no_input("hello"),
            Exp::Num(Numeral::from_i32(24)),
            hello_index,
        );
        let assign2 = VarAssignment::new_with_index(
            VarName::new_no_input("hello"),
            Exp::Num(Numeral::from_i32(36)),
            hello_index,
        );
        let assign3 = VarAssignment::new_with_index(
            VarName::new_no_input("hello"),
            Exp::VarName(RVVarName::new_with_index("newvar", newvar_index)),
            hello_index,
        );

        let mut context = Context::new(None, ContextType::Normal);
        context.init_vars(vec![
            Some(PineRef::new_box(Some(12))),
            Some(PineRef::new_box(Some(100))),
        ]);

        let test_val = |context: &mut Context, int_val| {
            let s: RefData<Series<Int>> =
                Series::implicity_from(context.move_var(hello_index).unwrap()).unwrap();
            assert_eq!(s, RefData::new_rc(Series::from(Some(int_val))));
            context.update_var(hello_index, s.into_pf());
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
        let mut ite = IfThenElse::new_no_ctxid(
            Exp::VarName(RVVarName::new_no_range("cond")),
            Block::new_no_input(vec![], Some(Exp::VarName(RVVarName::new_no_range("then")))),
            Some(Block::new_no_input(
                vec![],
                Some(Exp::VarName(RVVarName::new_no_range("else"))),
            )),
            StrRange::new_empty(),
        );
        // ite.result_type = SyntaxType::Series(SimpleSyntaxType::Int);
        SyntaxParser::new_with_vars(
            &[
                ("cond", SyntaxType::Simple(SimpleSyntaxType::Bool)),
                ("then", SyntaxType::Simple(SimpleSyntaxType::Int)),
                ("else", SyntaxType::Simple(SimpleSyntaxType::Int)),
            ]
            .iter()
            .cloned()
            .collect(),
        )
        .parse_ifthenelse_exp(&mut ite)
        .unwrap();
        let mut context = Context::new(None, ContextType::Normal);
        context.init_vars(vec![
            Some(PineRef::new_box(true)),
            Some(PineRef::new_box(Some(2))),
            Some(PineRef::new_box(Some(4))),
        ]);
        context.init_sub_contexts(vec![None, None]);

        assert_eq!(
            downcast_pf::<Series<Int>>(ite.run(&mut context).unwrap()),
            Ok(RefData::new_rc(Series::from(Some(2))))
        );
        // If-Then-Else Run as statement
        assert_eq!(ite.st_run(&mut context), Ok(()));
    }

    #[test]
    fn for_range_test() {
        use crate::syntax::SyntaxParser;

        let assign = Statement::Assignment(Box::new(Assignment::new_no_input(
            vec![VarName::new_no_input("a")],
            Exp::Num(Numeral::from_i32(1)),
            false,
            None,
        )));
        let block = Block::new_no_input(vec![assign], Some(Exp::Num(Numeral::from_i32(10))));
        let mut for_range = ForRange::new_no_ctxid(
            VarName::new_no_input("i"),
            Exp::Num(Numeral::from_i32(1)),
            Exp::Num(Numeral::from_i32(10)),
            None,
            block,
            StrRange::new_empty(),
        );
        SyntaxParser::new()
            .parse_forrange_exp(&mut for_range)
            .unwrap();

        let mut context = Context::new(None, ContextType::Normal);
        context.init(0, 1, 0);

        let result = Runner::run(&for_range, &mut context);
        assert!(result.is_ok());

        assert_eq!(
            downcast_pf::<Series<Int>>(result.unwrap()),
            Ok(RefData::new_rc(Series::from(Some(10))))
        );
        // assert!(context.move_var(VarIndex::new(1, 0)).is_none());
    }

    #[test]
    fn for_range_exp_test() {
        let assign = Statement::Assignment(Box::new(Assignment::new_with_varids(
            vec![VarName::new_no_input("a")],
            Exp::Num(Numeral::from_i32(1)),
            false,
            None,
            vec![1],
        )));
        let block =
            Block::new_with_count(vec![assign], Some(Exp::Num(Numeral::from_i32(10))), 2, 0, 0);
        let for_range = ForRange::new_with_ctxid(
            VarName::new_no_input("i"),
            Exp::VarName(RVVarName::new_with_index("start", VarIndex::new(0, 0))),
            Exp::VarName(RVVarName::new_with_index("end", VarIndex::new(1, 0))),
            Some(Exp::VarName(RVVarName::new_with_index(
                "step",
                VarIndex::new(2, 0),
            ))),
            block,
            StrRange::new_empty(),
            0,
            SyntaxType::Simple(SimpleSyntaxType::Int),
        );

        let mut context = Context::new(None, ContextType::Normal);
        context.init(3, 1, 0);
        context.create_var(0, PineRef::new(Some(1)));
        context.create_var(1, PineRef::new(Some(10)));
        context.create_var(2, PineRef::new(Some(5)));

        let result = Runner::run(&for_range, &mut context);
        assert!(result.is_ok());

        assert_eq!(
            Int::implicity_from(result.unwrap()),
            Ok(RefData::new_box(Some(10)))
        );

        // assert!(context.move_var("a").is_none());
    }

    #[test]
    fn func_call_exp_test() {
        use crate::ast::syntax_type::FunctionType;

        let mut exp = FunctionCall::new_no_ctxid(
            Exp::VarName(RVVarName::new_with_index("name", VarIndex::new(0, 0))),
            vec![Exp::Bool(BoolNode::new_no_range(true))],
            vec![],
            StrRange::new_empty(),
        );
        let bool_type = SyntaxType::Simple(SimpleSyntaxType::Bool);
        exp.func_type = Some(FunctionType((vec![("arg", bool_type.clone())], bool_type)));
        let mut context = Context::new(None, ContextType::Normal);

        fn test_func<'a>(
            _context: &mut dyn Ctx<'a>,
            h: Vec<Option<PineRef<'a>>>,
            _type: FunctionType<'a>,
        ) -> Result<PineRef<'a>, RuntimeErr> {
            match &h[0] {
                None => Err(RuntimeErr::NotValidParam),
                Some(arg) => Ok(arg.clone()),
            }
        }

        context.init(1, 1, 0);
        context.init_vars(vec![Some(PineRef::new_rc(Callable::new(
            Some(test_func),
            None,
            None,
        )))]);

        let res = downcast_pf::<Bool>(exp.run(&mut context).unwrap()).unwrap();
        assert_eq!(res, RefData::new_box(true));

        // Test for CallableFactory
        let mut context = Context::new(None, ContextType::Normal);
        context.init(1, 1, 1);
        context.init_vars(vec![Some(PineRef::new_rc(CallableFactory::new(|| {
            Callable::new(Some(test_func), None, None)
        })))]);
        let res = downcast_pf::<Bool>(exp.run(&mut context).unwrap()).unwrap();
        assert_eq!(res, RefData::new_box(true));
        assert!(context.move_fun_instance(0).is_some());
    }

    #[test]
    fn func_call_exp2_test() {
        use crate::ast::stat_expr_types::Exp;
        let name_index = VarIndex::new(0, 0);
        let series_index = VarIndex::new(1, 0);

        let exp = FunctionCall::new_no_ctxid(
            Exp::VarName(RVVarName::new_with_index("name", name_index)),
            vec![Exp::Bool(BoolNode::new_no_range(true))],
            vec![],
            StrRange::new_empty(),
        );
        let series_exp = FunctionCall::new_no_ctxid(
            Exp::VarName(RVVarName::new_with_index("name", name_index)),
            vec![Exp::VarName(RVVarName::new_with_index(
                "series",
                series_index,
            ))],
            vec![],
            StrRange::new_empty(),
        );
        let def_exp = FunctionDef::new(
            VarName::new_no_input("name"),
            vec![VarName::new_no_input("arg")],
            Block::new_with_count(
                vec![],
                Some(Exp::VarName(RVVarName::new_with_index(
                    "arg",
                    VarIndex::new(0, 0),
                ))),
                2,
                0,
                0,
            ),
            StrRange::new_empty(),
        );
        let mut parent_def = def_exp.clone();
        parent_def.spec_defs.as_mut().unwrap().push(def_exp);
        let mut context = Context::new(None, ContextType::Normal);
        context.init(2, 2, 0);
        context.create_var(
            name_index.varid,
            PineRef::new_rc(Function::new(&parent_def)),
        );

        let res = downcast_pf::<Bool>(exp.run(&mut context).unwrap()).unwrap();
        assert_eq!(res, RefData::new_box(true));

        let subctx = context.get_sub_context(0).unwrap();
        assert_eq!(
            downcast_ctx(&mut **subctx).move_var(VarIndex::new(0, 0)),
            Some(PineRef::new_box(true))
        );

        context.create_var(series_index.varid, PineRef::new_rc(Series::from(Some(10))));
        // println!("{:?}", context.move_var("series"));
        assert_eq!(
            series_exp.run(&mut context).unwrap(),
            PineRef::new(Series::from(Some(10)))
        );
        context.commit();
        context.update_var(series_index, PineRef::new_rc(Series::from(Some(100))));

        assert_eq!(
            series_exp.run(&mut context).unwrap(),
            PineRef::new(Series::from_cur_history(Some(100), vec![Some(10)]))
        );
    }

    #[test]
    fn for_range_break_test() {
        let block = Block::new_with_count(
            vec![Statement::Break(StrRange::new_empty())],
            Some(Exp::VarName(RVVarName::new_with_index(
                "i",
                VarIndex::new(0, 0),
            ))),
            1,
            0,
            0,
        );
        let for_range = ForRange::new_with_ctxid(
            VarName::new_no_input("i"),
            Exp::Num(Numeral::from_i32(1)),
            Exp::Num(Numeral::from_i32(10)),
            None,
            block,
            StrRange::new_empty(),
            0,
            SyntaxType::Simple(SimpleSyntaxType::Int),
        );

        let mut context = Context::new(None, ContextType::Normal);
        context.init(0, 1, 0);

        let result = Runner::run(&for_range, &mut context);
        assert!(result.is_ok());

        assert_eq!(
            Int::implicity_from(result.unwrap()),
            Ok(RefData::new_box(Some(1)))
        );

        // assert!(context.move_var("a").is_none());
    }

    #[test]
    fn for_range_continue_test() {
        let block = Block::new_with_count(
            vec![Statement::Continue(StrRange::new_empty())],
            Some(Exp::VarName(RVVarName::new_with_index(
                "i",
                VarIndex::new(0, 0),
            ))),
            1,
            0,
            0,
        );
        let for_range = ForRange::new_with_ctxid(
            VarName::new_no_input("i"),
            Exp::Num(Numeral::from_i32(1)),
            Exp::Num(Numeral::from_i32(10)),
            None,
            block,
            StrRange::new_empty(),
            0,
            SyntaxType::Simple(SimpleSyntaxType::Int),
        );

        let mut context = Context::new(None, ContextType::Normal);
        context.init(0, 1, 0);

        let result = Runner::run(&for_range, &mut context);
        assert!(result.is_ok());

        assert_eq!(
            Int::implicity_from(result.unwrap()),
            Ok(RefData::new_box(Some(9)))
        );

        // assert!(context.move_var("a").is_none());
    }
}

use super::context::{Ctx, PineRuntimeError, RVRunner, Runner, RunnerForFunc, RunnerForObj};
pub use crate::ast::stat_expr_types::{
    Condition, DataType, Exp, FunctionCall, PrefixExp, RVVarName, RefCall, Statement, TypeCast,
    VarIndex,
};
use crate::ast::syntax_type::{FunctionType, SimpleSyntaxType, SyntaxType};
use crate::types::{
    downcast_pf, downcast_pf_mut, Bool, CallObjEval, Callable, CallableEvaluate, CallableFactory,
    CallableObject, Color, DataType as FirstType, Evaluate, EvaluateFactory, Float, Int, Object,
    PineFrom, PineRef, PineStaticType, PineType, PineVar, RefData, RuntimeErr, SecondType, Series,
    Tuple, NA,
};
use std::fmt::Debug;

// Invoke evaluate variable for eval factory variable.
pub fn call_eval_factory<'a>(
    context: &mut dyn Ctx<'a>,
    eval_id: i32,
    s: PineRef<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    // Get evaluate instance from context
    let mut eval_instance = context.move_fun_instance(eval_id);
    if eval_instance.is_none() {
        let eval_val = match s.get_type().0 {
            FirstType::CallableEvaluate => {
                let eval_factory = downcast_pf::<CallableEvaluate>(s.clone()).unwrap();
                eval_factory.create_eval()
            }
            FirstType::CallableObjectEvaluate => {
                let eval_factory = downcast_pf::<CallObjEval>(s.clone()).unwrap();
                eval_factory.create_eval()
            }
            _ => {
                let factory = downcast_pf::<EvaluateFactory>(s.clone()).unwrap();
                factory.create()
            }
        };
        // let factory = downcast_pf::<EvaluateFactory>(s.clone()).unwrap();
        context.create_fun_instance(eval_id, PineRef::new_rc(eval_val));
        eval_instance = context.move_fun_instance(eval_id);
    }
    let mut eval_val = downcast_pf::<Evaluate>(eval_instance.unwrap()).unwrap();
    let result = eval_val.call(context);

    context.create_fun_instance(eval_id, RefData::clone(&eval_val).into_pf());
    context.create_runnable(RefData::clone(&eval_val).into_rc());
    result
}

pub fn call_func_factory<'a>(
    context: &mut dyn Ctx<'a>,
    func_id: i32,
    s: PineRef<'a>,
    pos_args: Vec<PineRef<'a>>,
    dict_args: Vec<(&'a str, PineRef<'a>)>,
    func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    let mut opt_instance = context.move_fun_instance(func_id);
    if opt_instance.is_none() {
        let func_val = match s.get_type().0 {
            FirstType::CallableFactory => {
                let factory = downcast_pf::<CallableFactory>(s).unwrap();
                factory.create()
            }
            FirstType::CallableObject => {
                let factory = downcast_pf::<CallableObject>(s).unwrap();
                factory.create()
            }
            FirstType::CallableEvaluate => {
                let factory = downcast_pf::<CallableEvaluate>(s).unwrap();
                factory.create()
            }
            FirstType::CallableObjectEvaluate => {
                let factory = downcast_pf::<CallObjEval>(s).unwrap();
                factory.create()
            }
            _ => unreachable!(),
        };
        context.create_fun_instance(func_id, PineRef::new_rc(func_val));
        opt_instance = context.move_fun_instance(func_id);
    }
    let mut callable = downcast_pf::<Callable>(opt_instance.unwrap()).unwrap();

    let result = callable.call(context, pos_args, dict_args, func_type);

    context.create_fun_instance(func_id, RefData::clone(&callable).into_pf());
    context.create_runnable(callable.into_rc());
    result
}

pub fn type_cast_custom<'a>(
    context: &mut dyn Ctx<'a>,
    cast_index: VarIndex,
    func_index: i32,
    result: PineRef<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    match context.move_var(cast_index) {
        None => Err(RuntimeErr::VarNotFound),
        Some(s) => {
            let func_type = FunctionType::new((
                vec![("x", SyntaxType::Simple(SimpleSyntaxType::Na))],
                SyntaxType::Any,
            ));
            let result = call_func_factory(
                context,
                func_index,
                s.copy(),
                vec![result],
                vec![],
                func_type,
            );
            context.update_var(cast_index, s);
            result
        }
    }
}

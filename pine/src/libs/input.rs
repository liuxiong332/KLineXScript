use super::VarResult;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::runtime::context::{downcast_ctx, Ctx, InputVal};
use crate::types::{
    Bool, Callable, CallableFactory, DataType, Float, Int, ParamCollectCall, PineFrom, PineRef,
    PineType, RefData, RuntimeErr, SecondType, Series, NA,
};
use std::rc::Rc;

fn input_for_bool<'a>(
    context: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
) -> Result<PineRef<'a>, RuntimeErr> {
    let input_val = downcast_ctx(context).copy_next_input();
    match input_val {
        Some(InputVal::Bool(val)) => Ok(PineRef::new_box(val)),
        _ => match param.remove(0) {
            Some(val) => Ok(val),
            _ => Err(RuntimeErr::NotValidParam),
        },
    }
    // match param.remove(0) {
    //     Some(item_val) => {
    //         plot_val(item_val, context)?;
    //         Ok(PineRef::new_box(NA))
    //     }
    //     _ => Err(RuntimeErr::NotSupportOperator),
    // }
}

thread_local!(static BOOL_TYPE: FunctionType<'static> = FunctionType((
    vec![
        ("defval", SyntaxType::bool()),
        ("title", SyntaxType::string()),
        ("type", SyntaxType::string()),
        ("confirm", SyntaxType::bool()),
    ],
    SyntaxType::bool(),
)));

thread_local!(static INT_TYPE: FunctionType<'static> = 
    FunctionType((
        vec![
            ("defval", SyntaxType::int()),
            ("title", SyntaxType::string()),
            ("type", SyntaxType::string()),
            ("minval", SyntaxType::int()),
            ("maxval", SyntaxType::int()),
            ("confirm", SyntaxType::bool()),
            ("step", SyntaxType::int()),
            ("options", SyntaxType::List(SimpleSyntaxType::Int)),
        ],
        SyntaxType::int(),
    ))
);

fn pine_input<'a>(
    context: &mut dyn Ctx<'a>,
    param: Vec<Option<PineRef<'a>>>,
    func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    if func_type.arg_names().len() == 4 {
        input_for_bool(context, param)
    } else {
        unreachable!();
    }
}

pub const VAR_NAME: &'static str = "input";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(CallableFactory::new(|| {
        Callable::new(Some(pine_input), None)
    }));
    /*
        input(defval, title, type, confirm) → input bool
        input(defval, title, type, minval, maxval, confirm, step, options) → input integer
        input(defval, title, type, minval, maxval, confirm, step, options) → input float
        input(defval, title, type, confirm, options) → input string
        input(defval, title, type) → series[float]
    */
    let syntax_type = SyntaxType::Function(Rc::new(FunctionTypes(vec![
        BOOL_TYPE.with(|s| s.clone()),
        INT_TYPE.with(|s| s.clone()),
    ])));
    VarResult::new(value, syntax_type, VAR_NAME)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::stat_expr_types::{Block, VarIndex};
    use crate::runtime::context::{Context, VarOperate};
    use crate::runtime::data_src::{Callback, NoneCallback};
    use crate::types::PineRef;
    use crate::{LibInfo, PineParser, PineRunner, PineRuntimeError};

    #[test]
    fn input_type_test() {
        let lib_info = LibInfo::new(vec![declare_var()], vec![]);
        PineParser::new("input(true, 'title', 'bool', false)", &lib_info);
        PineParser::new(
            "input(defval = true, title = 'title', type = 'bool', confirm = false)",
            &lib_info,
        );
        PineParser::new(
            "input(true, 'hello', defval = true, title = 'title', type = 'bool', confirm = false)",
            &lib_info,
        );
    }

    #[test]
    fn bool_input_test<'a>() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = "m = input(true, 'title', 'bool', false)";

        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner.run(&vec![("close", vec![Some(1f64)])]).unwrap();
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(2, 0)),
            Some(PineRef::new_box(true))
        );

        runner.change_inputs(vec![InputVal::Bool(false)]);
        runner.run(&vec![("close", vec![Some(1f64)])]).unwrap();
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(2, 0)),
            Some(PineRef::new_box(false))
        );
    }
}

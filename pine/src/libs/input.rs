use super::VarResult;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::err_msgs::*;
use crate::helper::str_replace;
use crate::runtime::context::{downcast_ctx, Ctx, InputVal};
use crate::runtime::output::{BoolInputInfo, InputInfo};
use crate::types::{
    Bool, Callable, CallableFactory, DataType, Float, Int, ParamCollectCall, PineFrom, PineRef,
    PineType, RefData, RuntimeErr, SecondType, Series, SeriesCall, NA,
};
use std::cell::RefCell;
use std::mem;
use std::rc::Rc;

const BOOL_TYPE_STR: &'static str = "bool";
const INT_TYPE_STR: &'static str = "int";
const FLOAT_TYPE_STR: &'static str = "float";

#[derive(Debug, PartialEq, Clone)]
struct InputCall<'a> {
    val: RefCell<Option<PineRef<'a>>>,
}

impl<'a> InputCall<'a> {
    pub fn new() -> InputCall<'a> {
        InputCall {
            val: RefCell::new(None),
        }
    }
}

impl<'a> SeriesCall<'a> for InputCall<'a> {
    fn step(
        &self,
        context: &mut dyn Ctx<'a>,
        val: Vec<Option<PineRef<'a>>>,
        func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        if let Some(val) = &*self.val.borrow() {
            return Ok(val.clone());
        }
        match pine_input(context, val, func_type) {
            Err(e) => Err(e),
            Ok(res) => {
                self.val.replace(Some(res.clone()));
                Ok(res)
            }
        }
    }

    fn run(&self, _context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        self.val.replace(None);
        Ok(())
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        Box::new(self.clone())
    }
}

fn pine_ref_to_bool<'a>(val: Option<PineRef<'a>>) -> Option<bool> {
    if val.is_none() {
        return None;
    }
    match Bool::implicity_from(val.unwrap()) {
        Ok(res) => Some(res.into_inner()),
        Err(_) => None,
    }
}

fn pine_ref_to_string<'a>(val: Option<PineRef<'a>>) -> Option<String> {
    if val.is_none() {
        return None;
    }
    match String::implicity_from(val.unwrap()) {
        Ok(res) => Some(res.into_inner()),
        Err(_) => None,
    }
}

fn move_element<T>(vector: &mut Vec<Option<T>>, index: usize) -> Option<T> {
    mem::replace(&mut vector[index], None)
}

fn input_for_bool<'a>(
    context: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
) -> Result<PineRef<'a>, RuntimeErr> {
    let ctx_ins = downcast_ctx(context);
    if !ctx_ins.check_is_io_info_ready() {
        let type_str = pine_ref_to_string(move_element(&mut param, 2));

        if type_str.is_some() && type_str.as_ref().unwrap() != BOOL_TYPE_STR {
            // type must be BOOL_TYPE_STR
            return Err(RuntimeErr::FuncCallParamNotValid(str_replace(
                EXP_VAL_BUT_GET_VAL,
                vec![
                    String::from(BOOL_TYPE_STR),
                    String::from(type_str.as_ref().unwrap()),
                ],
            )));
        }
        ctx_ins.push_input_info(InputInfo::Bool(BoolInputInfo {
            defval: pine_ref_to_bool(param[0].clone()),
            title: pine_ref_to_string(move_element(&mut param, 1)),
            input_type: String::from(BOOL_TYPE_STR),
            confirm: pine_ref_to_bool(move_element(&mut param, 3)),
        }));
    }

    let input_val = ctx_ins.copy_next_input();
    match input_val {
        Some(InputVal::Bool(val)) => Ok(PineRef::new_box(val)),
        _ => match move_element(&mut param, 0) {
            Some(val) => Ok(val),
            _ => Err(RuntimeErr::NotValidParam),
        },
    }
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
        Callable::new(None, Some(Box::new(InputCall::new())))
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

        runner.change_inputs(vec![Some(InputVal::Bool(false))]);
        runner.run(&vec![("close", vec![Some(1f64)])]).unwrap();
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(2, 0)),
            Some(PineRef::new_box(false))
        );
        assert_eq!(
            runner.get_context().get_io_info().get_inputs(),
            &vec![InputInfo::Bool(BoolInputInfo {
                defval: Some(true),
                title: Some(String::from("title")),
                input_type: String::from("bool"),
                confirm: Some(false),
            })]
        )
    }
}

use super::VarResult;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::runtime::context::{downcast_ctx_const, Ctx};
use crate::types::{
    Bool, Callable, DataType, Float, Int, ParamCollectCall, PineFrom, PineRef, PineType, RefData,
    RuntimeErr, SecondType, Series, SimpleCall, NA,
};
use std::borrow::Borrow;
use std::rc::Rc;

trait IntoTarget<D> {
    fn into(&self) -> D;
}

impl IntoTarget<i32> for Int {
    fn into(&self) -> i32 {
        self.unwrap()
    }
}

impl IntoTarget<f64> for Float {
    fn into(&self) -> f64 {
        self.unwrap()
    }
}

impl IntoTarget<bool> for Bool {
    fn into(&self) -> bool {
        *self
    }
}

fn plot_series<'a>(item_val: PineRef<'a>, context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
    let items: RefData<Series<Float>> = Series::implicity_from(item_val).unwrap();
    let s: Vec<f64> = items
        .get_history()
        .iter()
        .map(|v| IntoTarget::into(v))
        .collect();
    context.get_callback().unwrap().plot(s);
    Ok(())
}

fn plot_val<'a>(item_val: PineRef<'a>, context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
    // println!("print val type {:?}", item_val.get_type());
    match item_val.get_type() {
        (DataType::Float, SecondType::Series) => plot_series(item_val, context),
        // (DataType::Int, SecondType::Series) => plot_series::<Int, i32>(item_val, context),
        // (DataType::Bool, SecondType::Series) => plot_series::<Bool, bool>(item_val, context),
        t => Err(RuntimeErr::NotImplement(format!(
            "The plot now only support int, float, bool type, but get {:?}",
            t
        ))),
    }
}

fn input_for_bool<'a>(
    context: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
) -> Result<PineRef<'a>, RuntimeErr> {
    match param.remove(0) {
        Some(item_val) => {
            plot_val(item_val, context)?;
            Ok(PineRef::new_box(NA))
        }
        _ => Err(RuntimeErr::NotSupportOperator),
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

struct PineInput {}

impl<'a> SimpleCall<'a> for PineInput {
    fn prepare(&mut self, _syntax_ctx: &mut dyn SyntaxCtx<'a>) {}
}

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
    let value = PineRef::new(Callable::new(Some(pine_input), None, None));
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
mod tests {}

use super::VarResult;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::runtime::context::Ctx;
use crate::types::{
    Bool, Callable, DataType, Float, Int, ParamCollectCall, PineFrom, PineRef, PineStaticType,
    PineType, RefData, RuntimeErr, SecondType, Series, NA,
};
use std::collections::HashMap;
use std::fmt::Debug;
use std::rc::Rc;

trait Format {
    fn fmt(&self) -> String;
}

impl Format for Int {
    fn fmt(&self) -> String {
        match self {
            None => String::from("na"),
            Some(f) => f.to_string(),
        }
    }
}

impl Format for Float {
    fn fmt(&self) -> String {
        match self {
            None => String::from("na"),
            Some(f) => f.to_string(),
        }
    }
}

impl Format for Bool {
    fn fmt(&self) -> String {
        if *self {
            String::from("true")
        } else {
            String::from("false")
        }
    }
}

fn print_series<'a, D>(item_val: PineRef<'a>, context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr>
where
    D: Default
        + Format
        + Clone
        + PartialEq
        + Debug
        + PineStaticType
        + PineFrom<'a, D>
        + PineType<'a>
        + 'a,
{
    let items: RefData<Series<D>> = Series::implicity_from(item_val).unwrap();
    let s: String = items
        .get_history()
        .iter()
        .map(|v| Format::fmt(v))
        .collect::<Vec<String>>()
        .join(",");
    context.get_callback().unwrap().print(s);
    Ok(())
}

fn print_array<'a, D>(item_val: PineRef<'a>, context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr>
where
    D: Format + Clone + PartialEq + Debug + PineFrom<'a, D> + PineStaticType + 'a,
{
    let items: RefData<Vec<D>> = Vec::implicity_from(item_val).unwrap();
    println!("items {:?}", items);
    let s: String = items
        .iter()
        .map(|v| Format::fmt(v))
        .collect::<Vec<String>>()
        .join(",");

    println!("items str {:?}", s);

    context.get_callback().unwrap().print(s);
    Ok(())
}

fn print_simple<'a, D>(item_val: PineRef<'a>, context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr>
where
    D: Format + Clone + PartialEq + Debug + PineFrom<'a, D> + 'a,
{
    let item: RefData<D> = D::implicity_from(item_val).unwrap();
    let s: String = Format::fmt(&*item);
    context.get_callback().unwrap().print(s);
    Ok(())
}

fn print_val<'a>(item_val: PineRef<'a>, context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
    println!("print val type {:?}", item_val.get_type());
    match item_val.get_type() {
        (DataType::Float, SecondType::Series) => print_series::<Float>(item_val, context),
        (DataType::Int, SecondType::Series) => print_series::<Int>(item_val, context),
        (DataType::Bool, SecondType::Series) => print_series::<Bool>(item_val, context),
        (DataType::Float, SecondType::Array) => print_array::<Float>(item_val, context),
        (DataType::Int, SecondType::Array) => print_array::<Int>(item_val, context),
        (DataType::Bool, SecondType::Array) => print_array::<Bool>(item_val, context),
        (DataType::Float, SecondType::Simple) => print_simple::<Float>(item_val, context),
        (DataType::Int, SecondType::Simple) => print_simple::<Int>(item_val, context),
        (DataType::Bool, SecondType::Simple) => print_simple::<Bool>(item_val, context),
        t => Err(RuntimeErr::NotImplement(format!(
            "The print now only support int, float, bool type, but get {:?}",
            t
        ))),
    }
}

fn pine_print<'a>(
    context: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    // println!("pine print Series run, {:?}", param.get("item"));
    match param.remove(0) {
        None => Err(RuntimeErr::NotSupportOperator),
        Some(item_val) => {
            print_val(item_val, context)?;
            Ok(PineRef::new_box(NA))
        }
    }
}

pub const VAR_NAME: &'static str = "print";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(Callable::new(
        None,
        Some(Box::new(ParamCollectCall::new(pine_print, &vec!["item"]))),
        vec!["item"],
    ));
    let syntax_type = SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType((
        vec![("item", SyntaxType::Series(SimpleSyntaxType::Float))],
        SyntaxType::Void,
    ))])));
    VarResult::new(value, syntax_type, VAR_NAME)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::context::Context;
    use crate::runtime::data_src::Callback;
    use crate::types::{downcast_pf, PineRef};

    fn new_func_type<'a>() -> FunctionType<'a> {
        FunctionType((
            vec![("item", SyntaxType::Series(SimpleSyntaxType::Float))],
            SyntaxType::Void,
        ))
    }

    #[test]
    fn print_test() {
        struct MyCallback;
        impl Callback for MyCallback {
            fn print(&self, _str: String) {
                assert_eq!(_str, String::from("2"));
            }
        }

        let callable = downcast_pf::<Callable>(declare_var().value).unwrap();
        let mut ctx = Context::new_with_callback(&MyCallback);
        callable
            .call(
                &mut ctx,
                vec![PineRef::new(Some(1f64))],
                vec![],
                new_func_type(),
            )
            .unwrap();
        callable
            .call(
                &mut ctx,
                vec![PineRef::new(Some(2f64))],
                vec![],
                new_func_type(),
            )
            .unwrap();
        callable.run(&mut ctx).unwrap();
    }

    fn run_call<'a>(val: PineRef<'a>, callback: &'a dyn Callback) {
        let mut ctx = Context::new_with_callback(callback);
        let mut map = vec![Some(val)];
        assert!(pine_print(&mut ctx, map, new_func_type()).is_ok());
    }

    #[test]
    fn int_print_test() {
        struct MyCallback;
        impl Callback for MyCallback {
            fn print(&self, _str: String) {
                println!("get str {:?}", _str);
                assert_eq!(_str, String::from("1"));
            }
        }
        run_call(PineRef::new(Some(1)), &MyCallback);
        run_call(PineRef::new(vec![Some(1)]), &MyCallback);
        let mut series = Series::from(Some(1));
        series.commit();
        run_call(PineRef::new(series), &MyCallback);
    }

    #[test]
    fn float_print_test() {
        struct MyCallback;
        impl Callback for MyCallback {
            fn print(&self, _str: String) {
                println!("get str {:?}", _str);
                assert_eq!(_str, String::from("1"));
            }
        }
        run_call(PineRef::new(Some(1f64)), &MyCallback);
        run_call(PineRef::new(vec![Some(1f64)]), &MyCallback);
        let mut series = Series::from(Some(1f64));
        series.commit();
        run_call(PineRef::new(series), &MyCallback);
    }

    #[test]
    fn bool_print_test() {
        struct MyCallback;
        impl Callback for MyCallback {
            fn print(&self, _str: String) {
                assert_eq!(_str, String::from("true"));
            }
        }
        run_call(PineRef::new(true), &MyCallback);
        run_call(PineRef::new(vec![true]), &MyCallback);
        let mut series = Series::from(true);
        series.commit();
        run_call(PineRef::new(series), &MyCallback);
    }
}

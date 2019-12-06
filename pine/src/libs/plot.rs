use crate::runtime::context::Ctx;
use crate::types::{
    Bool, Callable, DataType, Float, Int, ParamCollectCall, PineFrom, PineRef, PineStaticType,
    PineType, RefData, RuntimeErr, SecondType, Series, NA,
};
use std::collections::HashMap;
use std::fmt::Debug;

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

// fn plot_series<'a, D, T>(item_val: PineRef<'a>, context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr>
// where
//     D: Default
//         + IntoTarget<T>
//         + Clone
//         + PartialEq
//         + Debug
//         + PineStaticType
//         + PineFrom<'a, D>
//         + PineType<'a>
//         + 'a,
// {
//     let items: RefData<Series<D>> = Series::implicity_from(item_val).unwrap();
//     let s: Vec<T> = items
//         .get_history()
//         .iter()
//         .map(|v| IntoTarget::into(v))
//         .collect();
//     context.get_callback().unwrap().plot(s);
//     Ok(())
// } 

fn plot_series<'a>(item_val: PineRef<'a>, context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr>
{
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
    println!("print val type {:?}", item_val.get_type());
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

fn pine_plot<'a>(
    context: &mut dyn Ctx<'a>,
    mut param: HashMap<&'a str, PineRef<'a>>,
) -> Result<PineRef<'a>, RuntimeErr> {
    match param.remove("item") {
        None => Err(RuntimeErr::NotSupportOperator),
        Some(item_val) => {
            plot_val(item_val, context)?;
            Ok(PineRef::new_box(NA))
        }
    }
}

pub const VAR_NAME: &'static str = "plot";

pub fn declare_var<'a>() -> PineRef<'a> {
    PineRef::new(Callable::new(
        None,
        Some(Box::new(ParamCollectCall::new(pine_plot))),
        vec!["item"],
    ))
}

#[cfg(test)]
mod tests {
}

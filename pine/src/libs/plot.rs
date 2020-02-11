use super::VarResult;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::{
    move_element, pine_ref_to_bool, pine_ref_to_color, pine_ref_to_f64, pine_ref_to_i32,
    pine_ref_to_string,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::output::{OutputData, OutputInfo, PlotInfo};
use crate::types::{
    Bool, Callable, CallableFactory, DataType, Float, Int, ParamCollectCall, PineFrom, PineRef,
    PineType, RefData, RuntimeErr, SecondType, Series, NA,
};
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

fn plot_series<'a>(
    item_val: PineRef<'a>,
    _context: &mut dyn Ctx<'a>,
) -> Result<Vec<Option<f64>>, RuntimeErr> {
    let mut items: RefData<Series<Float>> = Series::implicity_from(item_val).unwrap();
    // let s: Vec<Option<f64>> = items.get_history().iter().cloned().collect();
    // context.get_callback().unwrap().plot(s);
    // downcast_ctx(context).push_output_data()

    // Move out the series history
    Ok(items.move_history())
}

fn plot_val<'a>(
    item_val: PineRef<'a>,
    context: &mut dyn Ctx<'a>,
) -> Result<Vec<Option<f64>>, RuntimeErr> {
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

fn pine_plot<'a>(
    context: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<(), RuntimeErr> {
    move_tuplet!(
        (
            series, title, color, linewidth, style, trackprice, transp, histbase, offset, join,
            editable, show_last, display
        ) = param
    );
    if !downcast_ctx(context).check_is_output_info_ready() {
        let plot_info = PlotInfo {
            title: pine_ref_to_string(title),
            color: pine_ref_to_color(color),
            linewidth: pine_ref_to_i32(linewidth),
            style: pine_ref_to_string(style),
            transp: pine_ref_to_i32(transp),
            trackprice: pine_ref_to_bool(trackprice),
            histbase: pine_ref_to_f64(histbase),
            offset: pine_ref_to_i32(offset),
            join: pine_ref_to_bool(join),
            editable: pine_ref_to_bool(editable),
            show_last: pine_ref_to_i32(show_last),
            display: pine_ref_to_bool(display),
        };
        downcast_ctx(context).push_output_info(OutputInfo::Plot(plot_info));
    }
    match series {
        Some(item_val) => {
            let data = plot_val(item_val, context)?;
            let ctx_ins = downcast_ctx(context);
            let data_range = ctx_ins.get_data_range();
            ctx_ins.push_output_data(Some(OutputData::new(data_range.0, data_range.1, data)));
            Ok(())
        }
        _ => Err(RuntimeErr::NotSupportOperator),
    }
}

pub const VAR_NAME: &'static str = "plot";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(CallableFactory::new(|| {
        Callable::new(None, Some(Box::new(ParamCollectCall::new(pine_plot))))
    }));

    // plot(series, title, color, linewidth, style, trackprice, transp, histbase, offset, join, editable, show_last) → plot

    let syntax_type = SyntaxType::Function(Rc::new(FunctionTypes(vec![FunctionType::new((
        vec![
            ("series", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("title", SyntaxType::string()),
            ("color", SyntaxType::color()),
            ("linewidth", SyntaxType::int()),
            ("style", SyntaxType::string()),
            ("trackprice", SyntaxType::bool()),
            ("transp", SyntaxType::int()),
            ("histbase", SyntaxType::float()),
            ("offset", SyntaxType::int()),
            ("join", SyntaxType::bool()),
            ("editable", SyntaxType::bool()),
            ("show_last", SyntaxType::int()),
            ("display", SyntaxType::bool()),
        ],
        SyntaxType::Void,
    ))])));
    VarResult::new(value, syntax_type, VAR_NAME)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::data_src::NoneCallback;
    use crate::{LibInfo, PineParser, PineRunner};

    #[test]
    fn plot_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = "plot(close)\nplot(close + 1)";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(&vec![("close", vec![Some(1f64), Some(2f64)])])
            .unwrap();

        assert_eq!(
            runner.get_context().move_output_data(),
            vec![
                Some(OutputData::new(
                    Some(0),
                    Some(2),
                    vec![Some(1f64), Some(2f64)]
                )),
                Some(OutputData::new(
                    Some(0),
                    Some(2),
                    vec![Some(2f64), Some(3f64)]
                ))
            ]
        );
        assert_eq!(runner.get_context().get_io_info().get_outputs().len(), 2);

        runner
            .update(&vec![("close", vec![Some(10f64), Some(11f64)])])
            .unwrap();
        assert_eq!(
            runner.get_context().move_output_data(),
            vec![
                Some(OutputData::new(
                    Some(1),
                    Some(3),
                    vec![Some(10f64), Some(11f64)]
                )),
                Some(OutputData::new(
                    Some(1),
                    Some(3),
                    vec![Some(11f64), Some(12f64)]
                )),
            ]
        );
        assert_eq!(runner.get_context().get_io_info().get_outputs().len(), 2);

        runner
            .update_from(
                &vec![("close", vec![Some(100f64), Some(101f64), Some(102f64)])],
                1,
            )
            .unwrap();
        assert_eq!(
            runner.get_context().move_output_data(),
            vec![
                Some(OutputData::new(
                    Some(1),
                    Some(4),
                    vec![Some(100f64), Some(101f64), Some(102f64)]
                )),
                Some(OutputData::new(
                    Some(1),
                    Some(4),
                    vec![Some(101f64), Some(102f64), Some(103f64)]
                )),
            ]
        );
    }

    #[test]
    fn plot_info_test() {
        use crate::runtime::OutputInfo;

        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = r"plot(close, title='Title', color=#00ffaa, linewidth=2, 
            style='area', transp=70, offset=15, trackprice=true, 
            histbase=0.0, join=true, editable=true, show_last=100, display=true)";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(&vec![("close", vec![Some(1f64), Some(2f64)])])
            .unwrap();
        assert_eq!(
            runner.get_context().get_io_info().get_outputs(),
            &vec![OutputInfo::Plot(PlotInfo {
                title: Some(String::from("Title")),
                color: Some(String::from("#00ffaa")),
                linewidth: Some(2),
                style: Some(String::from("area")),
                trackprice: Some(true),
                transp: Some(70),
                histbase: Some(0.0f64),
                offset: Some(15),
                join: Some(true),
                editable: Some(true),
                show_last: Some(100),
                display: Some(true)
            })]
        )
    }
}

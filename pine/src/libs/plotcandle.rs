use super::VarResult;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::err_msgs::*;
use crate::helper::str_replace;
use crate::helper::{
    move_element, pine_ref_to_bool, pine_ref_to_color, pine_ref_to_i64, pine_ref_to_string,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::{OutputData, OutputInfo, PlotCandleInfo};
use crate::types::{
    Callable, CallableFactory, Float, ParamCollectCall, PineFrom, PineRef, RefData, RuntimeErr,
    Series,
};
use std::rc::Rc;

fn pine_plot<'a>(
    context: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<(), RuntimeErr> {
    move_tuplet!(
        (
            open,
            high,
            low,
            close,
            title,
            color,
            wickcolor,
            editable,
            show_last,
            bordercolor,
            display
        ) = param
    );
    if !downcast_ctx(context).check_is_output_info_ready() {
        let plot_info = PlotCandleInfo {
            title: pine_ref_to_string(title),
            color: pine_ref_to_color(color),
            wickcolor: pine_ref_to_color(wickcolor),
            editable: pine_ref_to_bool(editable),
            show_last: pine_ref_to_i64(show_last),
            bordercolor: pine_ref_to_color(bordercolor),
            display: pine_ref_to_i64(display),
        };
        downcast_ctx(context).push_output_info(OutputInfo::PlotCandle(plot_info));
    }
    match (open, high, low, close) {
        (Some(open_v), Some(high_v), Some(low_v), Some(close_v)) => {
            let mut open_items: RefData<Series<Float>> = Series::implicity_from(open_v).unwrap();
            let mut high_items: RefData<Series<Float>> = Series::implicity_from(high_v).unwrap();
            let mut low_items: RefData<Series<Float>> = Series::implicity_from(low_v).unwrap();
            let mut close_items: RefData<Series<Float>> = Series::implicity_from(close_v).unwrap();
            downcast_ctx(context).push_output_data(Some(OutputData::new(vec![
                open_items.move_history(),
                high_items.move_history(),
                low_items.move_history(),
                close_items.move_history(),
            ])));
            Ok(())
        }
        (o, h, l, c) => {
            let mut missings: Vec<String> = vec![];
            if o.is_none() {
                missings.push(String::from("open"));
            }
            if h.is_none() {
                missings.push(String::from("high"));
            }
            if l.is_none() {
                missings.push(String::from("low"));
            }
            if c.is_none() {
                missings.push(String::from("close"));
            }
            Err(RuntimeErr::MissingParameters(str_replace(
                REQUIRED_PARAMETERS,
                vec![missings.join(", ")],
            )))
        }
    }
}

pub const VAR_NAME: &'static str = "plotcandle";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(CallableFactory::new(|| {
        Callable::new(None, Some(Box::new(ParamCollectCall::new(pine_plot))))
    }));

    let func_type = FunctionTypes(vec![FunctionType::new((
        vec![
            ("open", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("high", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("low", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("close", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("title", SyntaxType::string()),
            ("color", SyntaxType::color()),
            ("wickcolor", SyntaxType::color()),
            ("editable", SyntaxType::bool()),
            ("show_last", SyntaxType::int()),
            ("bordercolor", SyntaxType::color()),
            ("display", SyntaxType::int()),
        ],
        SyntaxType::Void,
    ))]);
    let syntax_type = SyntaxType::Function(Rc::new(func_type));
    VarResult::new(value, syntax_type, VAR_NAME)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::{AnySeries, NoneCallback};
    use crate::{LibInfo, PineParser, PineRunner};

    #[test]
    fn plotbar_info_test() {
        use crate::runtime::OutputInfo;

        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![
                ("close", SyntaxType::Series(SimpleSyntaxType::Float)),
                ("high", SyntaxType::Series(SimpleSyntaxType::Float)),
                ("low", SyntaxType::Series(SimpleSyntaxType::Float)),
                ("open", SyntaxType::Series(SimpleSyntaxType::Float)),
            ],
        );
        let src = r"plotcandle(open, high, low, close, title='Title', color=#ff0000, 
            wickcolor=#000000, editable=true, show_last=100, bordercolor=#111111, display=1) ";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![
                    (
                        "close",
                        AnySeries::from_float_vec(vec![Some(1f64), Some(2f64)]),
                    ),
                    (
                        "open",
                        AnySeries::from_float_vec(vec![Some(0f64), Some(10f64)]),
                    ),
                    (
                        "high",
                        AnySeries::from_float_vec(vec![Some(1f64), Some(12f64)]),
                    ),
                    (
                        "low",
                        AnySeries::from_float_vec(vec![Some(0f64), Some(1f64)]),
                    ),
                ],
                None,
            )
            .unwrap();

        assert_eq!(
            runner.move_output_data(),
            vec![Some(OutputData::new(vec![
                vec![Some(0f64), Some(10f64)],
                vec![Some(1f64), Some(12f64)],
                vec![Some(0f64), Some(1f64)],
                vec![Some(1f64), Some(2f64)],
            ])),]
        );
        assert_eq!(
            runner.get_io_info().get_outputs(),
            &vec![OutputInfo::PlotCandle(PlotCandleInfo {
                title: Some(String::from("Title")),
                color: Some(String::from("#ff0000")),
                wickcolor: Some(String::from("#000000")),
                editable: Some(true),
                show_last: Some(100),
                bordercolor: Some(String::from("#111111")),
                display: Some(1),
            })]
        );
    }
}

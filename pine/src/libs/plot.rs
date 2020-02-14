use super::VarResult;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::err_msgs::*;
use crate::helper::str_replace;
use crate::helper::{
    move_element, pine_ref_to_bool, pine_ref_to_color, pine_ref_to_f64, pine_ref_to_i64,
    pine_ref_to_string,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::output::{OutputData, OutputInfo, PlotInfo};
use crate::types::{
    Bool, Callable, CallableObject, DataType, Float, Int, ParamCollectCall, PineClass, PineFrom,
    PineRef, PineType, RefData, RuntimeErr, SecondType, Series, NA,
};
use std::collections::BTreeMap;
use std::rc::Rc;

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
    match item_val.get_type() {
        (DataType::Float, SecondType::Series) => plot_series(item_val, context),
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
            linewidth: pine_ref_to_i64(linewidth),
            style: pine_ref_to_string(style),
            transp: pine_ref_to_i64(transp),
            trackprice: pine_ref_to_bool(trackprice),
            histbase: pine_ref_to_f64(histbase),
            offset: pine_ref_to_i64(offset),
            join: pine_ref_to_bool(join),
            editable: pine_ref_to_bool(editable),
            show_last: pine_ref_to_i64(show_last),
            display: pine_ref_to_i64(display),
        };
        downcast_ctx(context).push_output_info(OutputInfo::Plot(plot_info));
    }
    match series {
        Some(item_val) => {
            let data = plot_val(item_val, context)?;
            downcast_ctx(context).push_output_data(Some(OutputData::new(vec![data])));
            Ok(())
        }
        _ => Err(RuntimeErr::NotSupportOperator),
    }
}

struct PlotProps;

impl<'a> PineClass<'a> for PlotProps {
    fn custom_type(&self) -> &str {
        "input"
    }

    fn get(&self, _ctx: &mut dyn Ctx<'a>, name: &str) -> Result<PineRef<'a>, RuntimeErr> {
        match name {
            "style_area" => Ok(PineRef::new_rc(String::from("area"))),
            "style_areabr" => Ok(PineRef::new_rc(String::from("areabr"))),
            "style_circles" => Ok(PineRef::new_rc(String::from("circles"))),
            "style_columns" => Ok(PineRef::new_rc(String::from("columns"))),
            "style_cross" => Ok(PineRef::new_rc(String::from("cross"))),
            "style_histogram" => Ok(PineRef::new_rc(String::from("histogram"))),
            "style_line" => Ok(PineRef::new_rc(String::from("line"))),
            "style_linebr" => Ok(PineRef::new_rc(String::from("linebr"))),
            "style_stepline" => Ok(PineRef::new_rc(String::from("stepline"))),
            _ => Err(RuntimeErr::NotImplement(str_replace(
                NO_FIELD_IN_OBJECT,
                vec![String::from(name), String::from("plot")],
            ))),
        }
    }

    fn copy(&self) -> Box<dyn PineClass<'a> + 'a> {
        Box::new(PlotProps)
    }
}

pub const VAR_NAME: &'static str = "plot";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(CallableObject::new(Box::new(PlotProps), || {
        Callable::new(None, Some(Box::new(ParamCollectCall::new(pine_plot))))
    }));

    // plot(series, title, color, linewidth, style, trackprice, transp, histbase, offset, join, editable, show_last) → plot

    let func_type = FunctionTypes(vec![FunctionType::new((
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
            ("display", SyntaxType::int()),
        ],
        SyntaxType::Void,
    ))]);
    let mut obj_type = BTreeMap::new();
    obj_type.insert("style_area", SyntaxType::string());
    obj_type.insert("style_areabr", SyntaxType::string());
    obj_type.insert("style_circles", SyntaxType::string());
    obj_type.insert("style_columns", SyntaxType::string());
    obj_type.insert("style_cross", SyntaxType::string());
    obj_type.insert("style_histogram", SyntaxType::string());
    obj_type.insert("style_line", SyntaxType::string());
    obj_type.insert("style_linebr", SyntaxType::string());
    obj_type.insert("style_stepline", SyntaxType::string());
    let syntax_type = SyntaxType::ObjectFunction(Rc::new(obj_type), Rc::new(func_type));
    VarResult::new(value, syntax_type, VAR_NAME)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::{AnySeries, NoneCallback};
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
            .run(
                &vec![(
                    "close",
                    AnySeries::from_float_vec(vec![Some(1f64), Some(2f64)]),
                )],
                None,
            )
            .unwrap();

        assert_eq!(
            runner.get_context().move_output_data(),
            vec![
                Some(OutputData::new(vec![vec![Some(1f64), Some(2f64)]])),
                Some(OutputData::new(vec![vec![Some(2f64), Some(3f64)]]))
            ]
        );
        assert_eq!(runner.get_context().get_io_info().get_outputs().len(), 2);

        runner
            .update(&vec![(
                "close",
                AnySeries::from_float_vec(vec![Some(10f64), Some(11f64)]),
            )])
            .unwrap();
        assert_eq!(
            runner.get_context().move_output_data(),
            vec![
                Some(OutputData::new(vec![vec![Some(10f64), Some(11f64)]])),
                Some(OutputData::new(vec![vec![Some(11f64), Some(12f64)]])),
            ]
        );
        assert_eq!(runner.get_context().get_io_info().get_outputs().len(), 2);

        runner
            .update_from(
                &vec![(
                    "close",
                    AnySeries::from_float_vec(vec![Some(100f64), Some(101f64), Some(102f64)]),
                )],
                1,
            )
            .unwrap();
        assert_eq!(
            runner.get_context().move_output_data(),
            vec![
                Some(OutputData::new(vec![vec![
                    Some(100f64),
                    Some(101f64),
                    Some(102f64)
                ]])),
                Some(OutputData::new(vec![vec![
                    Some(101f64),
                    Some(102f64),
                    Some(103f64)
                ]])),
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
            histbase=0.0, join=true, editable=true, show_last=100, display=1)";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![(
                    "close",
                    AnySeries::from_float_vec(vec![Some(1f64), Some(2f64)]),
                )],
                None,
            )
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
                display: Some(1)
            })]
        )
    }

    #[test]
    fn plot_fields_test() {
        use crate::ast::stat_expr_types::VarIndex;
        use crate::runtime::VarOperate;
        use crate::types::{downcast_pf, Tuple};

        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::float_series())],
        );
        let src = r"m = [
            plot.style_area, plot.style_areabr, plot.style_circles, plot.style_columns, 
            plot.style_cross, plot.style_histogram, plot.style_line, plot.style_linebr,
            plot.style_stepline
        ]";

        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![("close", AnySeries::from_float_vec(vec![Some(1f64)]))],
                None,
            )
            .unwrap();
        let tuple_res =
            downcast_pf::<Tuple>(runner.get_context().move_var(VarIndex::new(2, 0)).unwrap());
        let tuple_vec = tuple_res.unwrap().into_inner().0;
        assert_eq!(
            tuple_vec,
            vec![
                PineRef::new_rc(String::from("area")),
                PineRef::new_rc(String::from("areabr")),
                PineRef::new_rc(String::from("circles")),
                PineRef::new_rc(String::from("columns")),
                PineRef::new_rc(String::from("cross")),
                PineRef::new_rc(String::from("histogram")),
                PineRef::new_rc(String::from("line")),
                PineRef::new_rc(String::from("linebr")),
                PineRef::new_rc(String::from("stepline")),
            ]
        );
    }
}

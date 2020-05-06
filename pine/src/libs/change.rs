use super::VarResult;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::err_msgs::*;
use crate::helper::str_replace;
use crate::helper::{
    check_ge1_i64, move_element, pine_ref_to_f64, pine_ref_to_f64_series, pine_ref_to_i64,
    require_param, series_index, series_index2,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::types::{
    Arithmetic, Callable, CallableFactory, Float, Int, ParamCollectCall, PineFrom, PineRef,
    RuntimeErr, Series, NA,
};
use std::mem;
use std::rc::Rc;

pub fn series_change(series: &Series<Option<f64>>, length: usize) -> Float {
    series_index2(series, 0).minus(series_index2(series, length))
}

fn change_func<'a>(
    _context: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    move_tuplet!((source, length) = param);
    let series = require_param("series", pine_ref_to_f64_series(source))?;
    let length = check_ge1_i64("length", pine_ref_to_i64(length).unwrap_or(1i64))? as usize;

    let val = series_change(&*series, length);
    Ok(PineRef::new_rc(Series::from(val)))
}

fn declare_var<'a>(name: &'static str) -> VarResult<'a> {
    let value = PineRef::new(CallableFactory::new(|| {
        Callable::new(
            None,
            Some(Box::new(ParamCollectCall::new_with_fns(
                Some(change_func),
                None,
            ))),
        )
    }));

    // plot(series, title, color, linewidth, style, trackprice, transp, histbase, offset, join, editable, show_last) → plot

    let func_type = FunctionTypes(vec![FunctionType::new((
        vec![
            ("source", SyntaxType::float_series()),
            ("length", SyntaxType::int()),
        ],
        SyntaxType::float_series(),
    ))]);
    let syntax_type = SyntaxType::Function(Rc::new(func_type));
    VarResult::new(value, syntax_type, name)
}

pub fn declare_change_var<'a>() -> VarResult<'a> {
    declare_var("change")
}

pub fn declare_mom_var<'a>() -> VarResult<'a> {
    declare_var("mom")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::stat_expr_types::VarIndex;
    use crate::ast::syntax_type::SimpleSyntaxType;
    use crate::runtime::{AnySeries, NoneCallback, VarOperate};
    use crate::{LibInfo, PineParser, PineRunner};

    #[test]
    fn change_test() {
        let lib_info = LibInfo::new(
            vec![declare_change_var(), declare_mom_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = "m1 = change(close)\nm2 = mom(close, 1)";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![(
                    "close",
                    AnySeries::from_float_vec(vec![Some(10f64), Some(20f64)]),
                )],
                None,
            )
            .unwrap();
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(0, 0)),
            Some(PineRef::new(Series::from_vec(vec![None, Some(10f64)])))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(1, 0)),
            Some(PineRef::new(Series::from_vec(vec![None, Some(10f64)])))
        );
    }

    #[test]
    fn change_color_test() {
        use crate::libs::color;
        use crate::libs::plot;
        use crate::runtime::output::{OutputData, StrOptionsData};

        let lib_info = LibInfo::new(
            vec![
                declare_change_var(),
                plot::declare_var(),
                color::declare_var(),
            ],
            vec![
                ("high", SyntaxType::Series(SimpleSyntaxType::Float)),
                ("low", SyntaxType::Series(SimpleSyntaxType::Float)),
            ],
        );
        // 5 10 -10
        let src = "ao = high - low\nplot(ao, color = change(ao) <= 0 ? color.red : color.green, style=plot.style_histogram)";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![
                    (
                        "high",
                        AnySeries::from_float_vec(vec![Some(10f64), Some(20f64), Some(10f64)]),
                    ),
                    (
                        "low",
                        AnySeries::from_float_vec(vec![Some(5f64), Some(10f64), Some(20f64)]),
                    ),
                ],
                None,
            )
            .unwrap();
        assert_eq!(
            runner.move_output_data(),
            vec![Some(OutputData::new_with_sc(
                vec![vec![Some(5f64), Some(10f64), Some(-10f64)]],
                vec![StrOptionsData {
                    options: vec![String::from("#FF5252"), String::from("#4CAF50")],
                    values: vec![Some(0), Some(1), Some(0)]
                }]
            )),]
        );
    }
}

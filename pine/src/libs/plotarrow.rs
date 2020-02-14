use super::VarResult;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::{
    move_element, pine_ref_to_bool, pine_ref_to_color, pine_ref_to_f64, pine_ref_to_i64,
    pine_ref_to_string,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::{OutputData, OutputInfo, PlotArrowInfo};
use crate::types::{
    Bool, Callable, CallableFactory, CallableObject, DataType, Float, Int, ParamCollectCall,
    PineClass, PineFrom, PineRef, PineType, RefData, RuntimeErr, SecondType, Series, NA,
};
use std::rc::Rc;

fn pine_plot<'a>(
    context: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<(), RuntimeErr> {
    move_tuplet!(
        (
            series, title, colorup, colordown, transp, offset, minheight, maxheight, editable,
            show_last, display
        ) = param
    );
    if !downcast_ctx(context).check_is_output_info_ready() {
        let plot_info = PlotArrowInfo {
            title: pine_ref_to_string(title),
            colorup: pine_ref_to_color(colorup),
            colordown: pine_ref_to_color(colordown),
            transp: pine_ref_to_i64(transp),
            offset: pine_ref_to_i64(offset),
            minheight: pine_ref_to_i64(minheight),
            maxheight: pine_ref_to_i64(maxheight),
            editable: pine_ref_to_bool(editable),
            show_last: pine_ref_to_i64(show_last),
            display: pine_ref_to_i64(display),
        };
        downcast_ctx(context).push_output_info(OutputInfo::PlotArrow(plot_info));
    }
    match series {
        Some(item_val) => {
            let mut items: RefData<Series<Float>> = Series::implicity_from(item_val).unwrap();
            let data = items.move_history();
            let ctx_ins = downcast_ctx(context);
            let data_range = ctx_ins.get_data_range();
            ctx_ins.push_output_data(Some(OutputData::new(vec![data])));
            Ok(())
        }
        _ => Err(RuntimeErr::NotSupportOperator),
    }
}

pub const VAR_NAME: &'static str = "plotarrow";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(CallableFactory::new(|| {
        Callable::new(None, Some(Box::new(ParamCollectCall::new(pine_plot))))
    }));

    // plot(series, title, color, linewidth, style, trackprice, transp, histbase, offset, join, editable, show_last) → plot

    let func_type = FunctionTypes(vec![FunctionType::new((
        vec![
            ("series", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("title", SyntaxType::string()),
            ("colorup", SyntaxType::color()),
            ("colordown", SyntaxType::color()),
            ("transp", SyntaxType::int()),
            ("offset", SyntaxType::int()),
            ("minheight", SyntaxType::int()),
            ("maxheight", SyntaxType::int()),
            ("editable", SyntaxType::bool()),
            ("show_last", SyntaxType::int()),
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
    fn plotarrow_info_test() {
        use crate::runtime::OutputInfo;

        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = r"plotarrow(close, title='Title', colorup=#00ffaa, colordown=#00ffbb, 
            transp=70, offset=15, minheight=1, maxheight=10, 
            editable=true, show_last=100, display=1)";
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
            vec![Some(OutputData::new(vec![vec![Some(1f64), Some(2f64)]])),]
        );
        assert_eq!(
            runner.get_context().get_io_info().get_outputs(),
            &vec![OutputInfo::PlotArrow(PlotArrowInfo {
                title: Some(String::from("Title")),
                colorup: Some(String::from("#00ffaa")),
                colordown: Some(String::from("#00ffbb")),
                transp: Some(70),
                offset: Some(15),
                minheight: Some(1),
                maxheight: Some(10),
                editable: Some(true),
                show_last: Some(100),
                display: Some(1)
            })]
        )
    }
}

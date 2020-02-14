use super::VarResult;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::err_msgs::*;
use crate::helper::str_replace;
use crate::helper::{
    move_element, pine_ref_to_bool, pine_ref_to_color, pine_ref_to_i64, pine_ref_to_string,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::output::{OutputData, OutputInfo, PlotShapeInfo};
use crate::types::{
    Bool, Callable, CallableFactory, DataType, Float, Int, ParamCollectCall, PineClass, PineFrom,
    PineRef, PineType, RefData, RuntimeErr, SecondType, Series, NA,
};
use std::rc::Rc;

fn pine_plot<'a>(
    context: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<(), RuntimeErr> {
    move_tuplet!(
        (
            series, title, style, location, color, transp, offset, text, textcolor, editable, size,
            show_last, display
        ) = param
    );
    if !downcast_ctx(context).check_is_output_info_ready() {
        let plot_info = PlotShapeInfo {
            title: pine_ref_to_string(title),
            style: pine_ref_to_string(style),
            location: pine_ref_to_string(location),
            color: pine_ref_to_color(color),
            transp: pine_ref_to_i64(transp),
            offset: pine_ref_to_i64(offset),
            text: pine_ref_to_string(text),
            textcolor: pine_ref_to_color(textcolor),

            editable: pine_ref_to_bool(editable),
            size: pine_ref_to_string(size),

            show_last: pine_ref_to_i64(show_last),
            display: pine_ref_to_i64(display),
        };
        downcast_ctx(context).push_output_info(OutputInfo::PlotShape(plot_info));
    }
    match series {
        Some(item_val) => {
            let mut items: RefData<Series<Float>> = Series::implicity_from(item_val).unwrap();
            downcast_ctx(context)
                .push_output_data(Some(OutputData::new(vec![items.move_history()])));
            Ok(())
        }
        _ => Err(RuntimeErr::MissingParameters(str_replace(
            REQUIRED_PARAMETERS,
            vec![String::from("close")],
        ))),
    }
}

pub const VAR_NAME: &'static str = "plotshape";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(CallableFactory::new(|| {
        Callable::new(None, Some(Box::new(ParamCollectCall::new(pine_plot))))
    }));

    let func_type = FunctionTypes(vec![FunctionType::new((
        vec![
            ("series", SyntaxType::Series(SimpleSyntaxType::Float)),
            ("title", SyntaxType::string()),
            ("style", SyntaxType::string()),
            ("location", SyntaxType::string()),
            ("color", SyntaxType::color()),
            ("transp", SyntaxType::int()),
            ("offset", SyntaxType::int()),
            ("text", SyntaxType::string()),
            ("textcolor", SyntaxType::color()),
            ("editable", SyntaxType::bool()),
            ("size", SyntaxType::string()),
            ("show_last", SyntaxType::int()),
            ("display", SyntaxType::bool()),
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
    fn plot_info_test() {
        use crate::runtime::OutputInfo;

        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = r"plotshape(close, title='Title', style='h', location='a', color=#00ffaa, 
            transp=70, offset=15, text='hello', textcolor=#111111, 
            editable=true, size='t', show_last=100, display=1)";
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
            &vec![OutputInfo::PlotShape(PlotShapeInfo {
                title: Some(String::from("Title")),
                style: Some(String::from("h")),
                location: Some(String::from("a")),
                color: Some(String::from("#00ffaa")),
                transp: Some(70),
                offset: Some(15),
                text: Some(String::from("hello")),
                textcolor: Some(String::from("#111111")),
                editable: Some(true),
                size: Some(String::from("t")),
                show_last: Some(100),
                display: Some(1)
            })]
        )
    }
}

use super::VarResult;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::err_msgs::*;
use crate::helper::str_replace;
use crate::helper::{
    move_element, pine_ref_to_bool, pine_ref_to_color, pine_ref_to_f64, pine_ref_to_i64,
    pine_ref_to_string,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::output::{HLineInfo, OutputData, OutputInfo, StrOptionsData};
use crate::types::{
    Bool, Callable, CallableObject, Color, DataType, Float, Int, ParamCollectCall, PineClass,
    PineFrom, PineRef, PineType, RefData, RuntimeErr, SecondType, Series, SeriesCall, NA,
};
use std::collections::BTreeMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
struct PlotVal {
    output_id: i32,
}

impl PlotVal {
    fn new() -> PlotVal {
        PlotVal { output_id: -1 }
    }
}

impl<'a> SeriesCall<'a> for PlotVal {
    fn step(
        &mut self,
        context: &mut dyn Ctx<'a>,
        mut p: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        println!(
            "is ready {:?}",
            downcast_ctx(context).check_is_output_info_ready(),
        );
        if self.output_id < 0 && !downcast_ctx(context).check_is_output_info_ready() {
            move_tuplet!((price, title, color, linestyle, linewidth, editable) = p);
            let plot_info = HLineInfo {
                price: pine_ref_to_f64(price),
                title: pine_ref_to_string(title),
                color: pine_ref_to_color(color),
                linestyle: pine_ref_to_string(linestyle),
                linewidth: pine_ref_to_i64(linewidth),
                editable: pine_ref_to_bool(editable),
            };
            self.output_id =
                downcast_ctx(context).push_output_info_retindex(OutputInfo::HLine(plot_info));
        }

        println!("get id {:?}", self.output_id);
        Ok(PineRef::Box(Box::new(Some(self.output_id as i64))))
    }

    fn run(&mut self, _context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        downcast_ctx(_context).push_output_data(None);
        Ok(())
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        Box::new(self.clone())
    }
}

struct PlotProps;

impl<'a> PineClass<'a> for PlotProps {
    fn custom_type(&self) -> &str {
        "hline"
    }

    fn get(&self, _ctx: &mut dyn Ctx<'a>, name: &str) -> Result<PineRef<'a>, RuntimeErr> {
        match name {
            "style_dashed" => Ok(PineRef::new_rc(String::from("dashed"))),
            "style_dotted" => Ok(PineRef::new_rc(String::from("dotted"))),
            "style_solid" => Ok(PineRef::new_rc(String::from("solid"))),
            _ => Err(RuntimeErr::NotImplement(str_replace(
                NO_FIELD_IN_OBJECT,
                vec![String::from(name), String::from("hline")],
            ))),
        }
    }

    fn copy(&self) -> Box<dyn PineClass<'a> + 'a> {
        Box::new(PlotProps)
    }
}

pub const VAR_NAME: &'static str = "hline";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(CallableObject::new(Box::new(PlotProps), || {
        Callable::new(None, Some(Box::new(PlotVal::new())))
    }));

    // plot(series, title, color, linewidth, style, trackprice, transp, histbase, offset, join, editable, show_last) → plot

    let func_type = FunctionTypes(vec![FunctionType::new((
        vec![
            ("price", SyntaxType::float()),
            ("title", SyntaxType::string()),
            ("color", SyntaxType::color()),
            ("linestyle", SyntaxType::string()),
            ("linewidth", SyntaxType::int()),
            ("editable", SyntaxType::bool()),
        ],
        SyntaxType::ObjectClass("hline"),
    ))]);
    let mut obj_type = BTreeMap::new();
    obj_type.insert("style_dashed", SyntaxType::string());
    obj_type.insert("style_dotted", SyntaxType::string());
    obj_type.insert("style_solid", SyntaxType::string());
    let syntax_type = SyntaxType::ObjectFunction(Rc::new(obj_type), Rc::new(func_type));
    VarResult::new(value, syntax_type, VAR_NAME)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::stat_expr_types::VarIndex;
    use crate::runtime::context::VarOperate;
    use crate::runtime::{AnySeries, NoneCallback};
    use crate::types::{downcast_pf, Tuple};
    use crate::{LibInfo, PineParser, PineRunner};

    #[test]
    fn plot_const_num() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = "m1 = hline(1, 'h1', #111111, hline.style_dashed, 1, true)
            m2 = [hline.style_dashed, hline.style_dotted, hline.style_solid]";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        assert!(runner
            .run(
                &vec![(
                    "close",
                    AnySeries::from_float_vec(vec![Some(1f64), Some(2f64)]),
                )],
                None,
            )
            .is_ok());
        assert_eq!(runner.move_output_data(), vec![None]);

        assert!(runner
            .run(
                &vec![(
                    "close",
                    AnySeries::from_float_vec(vec![Some(1f64), Some(2f64)]),
                )],
                None,
            )
            .is_ok());
        assert_eq!(
            runner.get_io_info().get_outputs(),
            &vec![OutputInfo::HLine(HLineInfo {
                price: Some(1f64),
                title: Some(String::from("h1")),
                color: Some(String::from("#111111")),
                linestyle: Some(String::from("dashed")),
                linewidth: Some(1i64),
                editable: Some(true),
            })]
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(0, 0)),
            Some(PineRef::new(Some(0i64)))
        );
        let tuple_res =
            downcast_pf::<Tuple>(runner.get_context().move_var(VarIndex::new(1, 0)).unwrap());
        let tuple_vec = tuple_res.unwrap().into_inner().0;

        assert_eq!(
            tuple_vec,
            vec![
                PineRef::new(String::from("dashed")),
                PineRef::new(String::from("dotted")),
                PineRef::new(String::from("solid"))
            ]
        );
    }
}

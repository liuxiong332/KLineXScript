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
    Bool, Callable, Color, DataType, Float, Int, Object, ParamCollectCall, PineClass, PineFrom,
    PineRef, PineType, RefData, RuntimeErr, SecondType, Series, NA,
};
use std::collections::BTreeMap;
use std::rc::Rc;

struct ColorProps;

impl<'a> PineClass<'a> for ColorProps {
    fn custom_type(&self) -> &str {
        "color"
    }

    fn get(&self, name: &str) -> Result<PineRef<'a>, RuntimeErr> {
        match name {
            "aqua" => Ok(PineRef::new_box(Color("#00BCD4"))),
            "black" => Ok(PineRef::new_box(Color("#363A45"))),
            "blue" => Ok(PineRef::new_box(Color("#2196F3"))),
            "fuchsia" => Ok(PineRef::new_box(Color("#E040FB"))),
            "gray" => Ok(PineRef::new_box(Color("#787B86"))),
            "green" => Ok(PineRef::new_box(Color("#4CAF50"))),
            "lime" => Ok(PineRef::new_box(Color("#00E676"))),
            "maroon" => Ok(PineRef::new_box(Color("#880E4F"))),
            "navy" => Ok(PineRef::new_box(Color("#311B92"))),
            "olive" => Ok(PineRef::new_box(Color("#808000"))),
            "orange" => Ok(PineRef::new_box(Color("#FF9800"))),
            "purple" => Ok(PineRef::new_box(Color("#9C27B0"))),
            "red" => Ok(PineRef::new_box(Color("#FF5252"))),
            "silver" => Ok(PineRef::new_box(Color("#B2B5BE"))),
            "teal" => Ok(PineRef::new_box(Color("#00897B"))),
            "white" => Ok(PineRef::new_box(Color("#FFFFFF"))),
            "yellow" => Ok(PineRef::new_box(Color("#FFEB3B"))),
            _ => Err(RuntimeErr::NotImplement(str_replace(
                NO_FIELD_IN_OBJECT,
                vec![String::from(name), String::from("color")],
            ))),
        }
    }

    fn copy(&self) -> Box<dyn PineClass<'a> + 'a> {
        Box::new(ColorProps)
    }
}

pub const VAR_NAME: &'static str = "color";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(Object::new(Box::new(ColorProps)));

    let mut obj_type = BTreeMap::new();
    obj_type.insert("aqua", SyntaxType::color());
    obj_type.insert("black", SyntaxType::color());
    obj_type.insert("blue", SyntaxType::color());
    obj_type.insert("fuchsia", SyntaxType::color());
    obj_type.insert("gray", SyntaxType::color());
    obj_type.insert("green", SyntaxType::color());
    obj_type.insert("lime", SyntaxType::color());
    obj_type.insert("maroon", SyntaxType::color());
    obj_type.insert("navy", SyntaxType::color());
    obj_type.insert("olive", SyntaxType::color());
    obj_type.insert("orange", SyntaxType::color());
    obj_type.insert("purple", SyntaxType::color());
    obj_type.insert("red", SyntaxType::color());
    obj_type.insert("silver", SyntaxType::color());
    obj_type.insert("teal", SyntaxType::color());
    obj_type.insert("white", SyntaxType::color());
    obj_type.insert("yellow", SyntaxType::color());
    let syntax_type = SyntaxType::Object(Rc::new(obj_type));
    VarResult::new(value, syntax_type, VAR_NAME)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::{AnySeries, NoneCallback};
    use crate::{LibInfo, PineParser, PineRunner};

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
            color.aqua, color.black, color.blue, color.fuchsia, color.gray, color.green,
            color.lime, color.maroon, color.navy, color.olive, color.orange, color.purple,
            color.red, color.silver, color.teal, color.white, color.yellow
        ]";

        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(&vec![(
                "close",
                AnySeries::from_float_vec(vec![Some(1f64)]),
            )])
            .unwrap();
        let tuple_res =
            downcast_pf::<Tuple>(runner.get_context().move_var(VarIndex::new(2, 0)).unwrap());
        let tuple_vec = tuple_res.unwrap().into_inner().0;
        assert_eq!(
            tuple_vec,
            vec![
                PineRef::new_box(Color("#00BCD4")),
                PineRef::new_box(Color("#363A45")),
                PineRef::new_box(Color("#2196F3")),
                PineRef::new_box(Color("#E040FB")),
                PineRef::new_box(Color("#787B86")),
                PineRef::new_box(Color("#4CAF50")),
                PineRef::new_box(Color("#00E676")),
                PineRef::new_box(Color("#880E4F")),
                PineRef::new_box(Color("#311B92")),
                PineRef::new_box(Color("#808000")),
                PineRef::new_box(Color("#FF9800")),
                PineRef::new_box(Color("#9C27B0")),
                PineRef::new_box(Color("#FF5252")),
                PineRef::new_box(Color("#B2B5BE")),
                PineRef::new_box(Color("#00897B")),
                PineRef::new_box(Color("#FFFFFF")),
                PineRef::new_box(Color("#FFEB3B")),
            ]
        );
    }
}

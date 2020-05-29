use super::VarResult;
use crate::ast::syntax_type::SyntaxType;
use crate::helper::err_msgs::*;
use crate::helper::str_replace;
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::output::{OutputData, OutputInfo, PlotInfo, StrOptionsData};
use crate::types::{
    Bool, Callable, CallableObject, Color, DataType, Float, Int, Object, ParamCollectCall,
    PineClass, PineFrom, PineRef, PineType, RefData, RuntimeErr, SecondType, Series, SeriesCall,
    NA,
};
use std::collections::BTreeMap;
use std::rc::Rc;

struct PlotProps;

impl<'a> PineClass<'a> for PlotProps {
    fn custom_type(&self) -> &str {
        "extend"
    }

    fn get(&self, _ctx: &mut dyn Ctx<'a>, name: &str) -> Result<PineRef<'a>, RuntimeErr> {
        match name {
            "both" => Ok(PineRef::new(String::from("both"))),
            "left" => Ok(PineRef::new(String::from("left"))),
            "none" => Ok(PineRef::new(String::from("none"))),
            "right" => Ok(PineRef::new(String::from("right"))),
            _ => Err(RuntimeErr::NotImplement(str_replace(
                NO_FIELD_IN_OBJECT,
                vec![String::from(name), String::from("extend")],
            ))),
        }
    }

    fn copy(&self) -> Box<dyn PineClass<'a> + 'a> {
        Box::new(PlotProps)
    }
}

pub const VAR_NAME: &'static str = "extend";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(Object::new(Box::new(PlotProps)));

    let mut obj_type = BTreeMap::new();
    obj_type.insert("both", SyntaxType::string());
    obj_type.insert("left", SyntaxType::string());
    obj_type.insert("none", SyntaxType::string());
    obj_type.insert("right", SyntaxType::string());
    let syntax_type = SyntaxType::Object(Rc::new(obj_type));
    VarResult::new(value, syntax_type, VAR_NAME)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::{AnySeries, NoneCallback};
    use crate::{LibInfo, PineParser, PineRunner};
    #[test]
    fn display_fields_test() {
        use crate::ast::stat_expr_types::VarIndex;
        use crate::types::{downcast_pf, Tuple};

        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::float_series())],
        );
        let src = r"m = [
            extend.both, extend.left, extend.none, extend.right
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
            downcast_pf::<Tuple>(runner.get_context().move_var(VarIndex::new(0, 0)).unwrap());
        let tuple_vec = tuple_res.unwrap().into_inner().0;
        assert_eq!(
            tuple_vec,
            vec![
                PineRef::new(String::from("both")),
                PineRef::new(String::from("left")),
                PineRef::new(String::from("none")),
                PineRef::new(String::from("right")),
            ]
        );
    }
}

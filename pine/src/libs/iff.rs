use super::VarResult;
use crate::ast::stat_expr_types::VarIndex;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::err_msgs::*;
use crate::helper::str_replace;
use crate::helper::{
    move_element, pine_ref_to_bool, pine_ref_to_color2, pine_ref_to_f64, pine_ref_to_i64,
    pine_ref_to_string, require_param,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::InputSrc;
use crate::types::{
    downcast_pf_ref, int2float, Arithmetic, Callable, CallableFactory, Color, Evaluate,
    EvaluateVal, Float, Int, PineRef, RefData, RuntimeErr, Series, SeriesCall, NA,
};
use std::mem;
use std::rc::Rc;

fn iff_func<'a>(
    _ctx: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    move_tuplet!((condition, then, _else) = param);
    let cond = require_param("condition", pine_ref_to_bool(condition))?;
    let val = if cond { then } else { _else };

    match _func_type.signature.1 {
        SyntaxType::Simple(SimpleSyntaxType::Int) => Ok(PineRef::new_box(pine_ref_to_i64(val))),
        SyntaxType::Simple(SimpleSyntaxType::Float) => Ok(PineRef::new_box(pine_ref_to_f64(val))),
        SyntaxType::Simple(SimpleSyntaxType::Bool) => {
            Ok(PineRef::new_box(pine_ref_to_bool(val).unwrap_or(false)))
        }
        SyntaxType::Simple(SimpleSyntaxType::Color) => Ok(PineRef::new_box(
            pine_ref_to_color2(val).unwrap_or(Color("")),
        )),
        SyntaxType::Simple(SimpleSyntaxType::String) => Ok(PineRef::new_rc(
            pine_ref_to_string(val).unwrap_or(String::from("")),
        )),

        SyntaxType::Series(SimpleSyntaxType::Int) => {
            Ok(PineRef::new_rc(Series::from(pine_ref_to_i64(val))))
        }
        SyntaxType::Series(SimpleSyntaxType::Float) => {
            Ok(PineRef::new_rc(Series::from(pine_ref_to_f64(val))))
        }
        SyntaxType::Series(SimpleSyntaxType::Bool) => Ok(PineRef::new_rc(Series::from(
            pine_ref_to_bool(val).unwrap_or(false),
        ))),
        SyntaxType::Series(SimpleSyntaxType::Color) => Ok(PineRef::new_rc(Series::from(
            pine_ref_to_color2(val).unwrap_or(Color("")),
        ))),
        SyntaxType::Series(SimpleSyntaxType::String) => Ok(PineRef::new_rc(Series::from(
            pine_ref_to_string(val).unwrap_or(String::from("")),
        ))),
        _ => unreachable!(),
    }
}

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(Callable::new(Some(iff_func), None));

    let func_type = FunctionTypes(vec![
        FunctionType::new((
            vec![
                ("condition", SyntaxType::bool()),
                ("then", SyntaxType::int()),
                ("_else", SyntaxType::int()),
            ],
            SyntaxType::int(),
        )),
        FunctionType::new((
            vec![
                ("condition", SyntaxType::bool()),
                ("then", SyntaxType::float()),
                ("_else", SyntaxType::float()),
            ],
            SyntaxType::float(),
        )),
        FunctionType::new((
            vec![
                ("condition", SyntaxType::bool()),
                ("then", SyntaxType::bool()),
                ("_else", SyntaxType::bool()),
            ],
            SyntaxType::bool(),
        )),
        FunctionType::new((
            vec![
                ("condition", SyntaxType::bool()),
                ("then", SyntaxType::color()),
                ("_else", SyntaxType::color()),
            ],
            SyntaxType::color(),
        )),
        FunctionType::new((
            vec![
                ("condition", SyntaxType::bool()),
                ("then", SyntaxType::string()),
                ("_else", SyntaxType::string()),
            ],
            SyntaxType::string(),
        )),
        FunctionType::new((
            vec![
                ("condition", SyntaxType::bool_series()),
                ("then", SyntaxType::int_series()),
                ("_else", SyntaxType::int_series()),
            ],
            SyntaxType::int_series(),
        )),
        FunctionType::new((
            vec![
                ("condition", SyntaxType::bool_series()),
                ("then", SyntaxType::float_series()),
                ("_else", SyntaxType::float_series()),
            ],
            SyntaxType::float_series(),
        )),
        FunctionType::new((
            vec![
                ("condition", SyntaxType::bool_series()),
                ("then", SyntaxType::bool_series()),
                ("_else", SyntaxType::bool_series()),
            ],
            SyntaxType::bool_series(),
        )),
        FunctionType::new((
            vec![
                ("condition", SyntaxType::bool_series()),
                ("then", SyntaxType::Series(SimpleSyntaxType::Color)),
                ("_else", SyntaxType::Series(SimpleSyntaxType::Color)),
            ],
            SyntaxType::Series(SimpleSyntaxType::Color),
        )),
        FunctionType::new((
            vec![
                ("condition", SyntaxType::bool_series()),
                ("then", SyntaxType::Series(SimpleSyntaxType::String)),
                ("_else", SyntaxType::Series(SimpleSyntaxType::String)),
            ],
            SyntaxType::Series(SimpleSyntaxType::String),
        )),
    ]);
    let syntax_type = SyntaxType::Function(Rc::new(func_type));
    VarResult::new(value, syntax_type, "iff")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::syntax_type::SyntaxType;
    use crate::runtime::VarOperate;
    use crate::runtime::{AnySeries, NoneCallback};
    use crate::types::Series;
    use crate::{LibInfo, PineParser, PineRunner};
    // use crate::libs::{floor, exp, };

    #[test]
    fn alma_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::float_series())],
        );
        let src = "
        m1 = iff(1, 1, 2)\nm2 = iff(1, 1.0, 2.0)\n
        m3 = iff(1, true, false)\nm4 = iff(1, 'hello', 'world')\n
        m5 = iff(1, #123456, #654321)\n
        n1 = iff(close, 1, 2)\nn2 = iff(close, 1.0, 2.0)\n
        n3 = iff(close, true, false)\nn4 = iff(close, 'hello', 'world')\n
        n5 = iff(close, #123456, #654321)\n
        ";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![("close", AnySeries::from_float_vec(vec![Some(10f64), None]))],
                None,
            )
            .unwrap();

        assert_eq!(
            runner.get_context().move_var(VarIndex::new(2, 0)),
            Some(PineRef::new(Some(1i64)))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(3, 0)),
            Some(PineRef::new(Some(1f64)))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(4, 0)),
            Some(PineRef::new(true))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(5, 0)),
            Some(PineRef::new(String::from("hello")))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(6, 0)),
            Some(PineRef::new(Color("#123456")))
        );

        assert_eq!(
            runner.get_context().move_var(VarIndex::new(7, 0)),
            Some(PineRef::new(Series::from_vec(vec![Some(1i64), Some(2i64)])))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(8, 0)),
            Some(PineRef::new(Series::from_vec(vec![Some(1f64), Some(2f64)])))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(9, 0)),
            Some(PineRef::new(Series::from_vec(vec![true, false])))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(10, 0)),
            Some(PineRef::new(Series::from_vec(vec![
                String::from("hello"),
                String::from("world")
            ])))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(11, 0)),
            Some(PineRef::new(Series::from_vec(vec![
                Color("#123456"),
                Color("#654321")
            ])))
        );
    }
}

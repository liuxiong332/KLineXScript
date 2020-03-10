use super::VarResult;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::{pine_ref_to_f64, pine_ref_to_i64};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::types::{Callable, Float, Int, PineFrom, PineRef, RuntimeErr, Series, NA};
use std::mem;
use std::rc::Rc;

fn int_na<'a>(xval: Option<PineRef<'a>>) -> bool {
    pine_ref_to_i64(xval).is_none()
}

fn float_na<'a>(xval: Option<PineRef<'a>>) -> bool {
    pine_ref_to_f64(xval).is_none()
}

fn na_func<'a>(
    _context: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
    func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    let xval = mem::replace(&mut param[0], None);

    match ((func_type.signature.0)[0]).1 {
        SyntaxType::Simple(SimpleSyntaxType::Int) => {
            let res = int_na(xval);
            Ok(PineRef::new_box(res))
        }
        SyntaxType::Series(SimpleSyntaxType::Int) => {
            let res = int_na(xval);
            Ok(PineRef::new_rc(Series::from(res)))
        }
        SyntaxType::Simple(SimpleSyntaxType::Float) => {
            let res = float_na(xval);
            Ok(PineRef::new_box(res))
        }
        SyntaxType::Series(SimpleSyntaxType::Float) => {
            let res = float_na(xval);
            Ok(PineRef::new_rc(Series::from(res)))
        }
        SyntaxType::Simple(SimpleSyntaxType::Na) => Ok(PineRef::new_box(true)),
        SyntaxType::Series(SimpleSyntaxType::Na) => Ok(PineRef::new_rc(Series::from(true))),
        SyntaxType::Simple(_) => Ok(PineRef::new_box(false)),
        SyntaxType::Series(_) => Ok(PineRef::new_rc(Series::from(false))),
        _ => unreachable!(),
    }
}

pub const VAR_NAME: &'static str = "na";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(Callable::new(Some(na_func), None));

    // plot(series, title, color, linewidth, style, trackprice, transp, histbase, offset, join, editable, show_last) → plot

    let func_type = FunctionTypes(vec![
        FunctionType::new((vec![("x", SyntaxType::int())], SyntaxType::bool())),
        FunctionType::new((vec![("x", SyntaxType::float())], SyntaxType::bool())),
        FunctionType::new((vec![("x", SyntaxType::color())], SyntaxType::bool())),
        FunctionType::new((vec![("x", SyntaxType::string())], SyntaxType::bool())),
        FunctionType::new((vec![("x", SyntaxType::bool())], SyntaxType::bool())),
        FunctionType::new((
            vec![("x", SyntaxType::Simple(SimpleSyntaxType::Na))],
            SyntaxType::bool(),
        )),
        FunctionType::new((
            vec![("x", SyntaxType::int_series())],
            SyntaxType::bool_series(),
        )),
        FunctionType::new((
            vec![("x", SyntaxType::float_series())],
            SyntaxType::bool_series(),
        )),
        FunctionType::new((
            vec![("x", SyntaxType::bool_series())],
            SyntaxType::bool_series(),
        )),
        FunctionType::new((
            vec![("x", SyntaxType::Series(SimpleSyntaxType::Color))],
            SyntaxType::bool_series(),
        )),
        FunctionType::new((
            vec![("x", SyntaxType::Series(SimpleSyntaxType::String))],
            SyntaxType::bool_series(),
        )),
        FunctionType::new((
            vec![("x", SyntaxType::Series(SimpleSyntaxType::Na))],
            SyntaxType::bool_series(),
        )),
    ]);
    let syntax_type = SyntaxType::Function(Rc::new(func_type));
    VarResult::new(value, syntax_type, VAR_NAME)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::stat_expr_types::VarIndex;
    use crate::ast::syntax_type::SimpleSyntaxType;
    use crate::runtime::{AnySeries, NoneCallback, VarOperate};
    use crate::{LibInfo, PineParser, PineRunner};

    #[test]
    fn na_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = "m1 = na(-123)\nm2 = na(int(close))\nm3 = na(-123.23)\nm4 = na(close)\nm5=na(na)";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![("close", AnySeries::from_float_vec(vec![Some(-2f64)]))],
                None,
            )
            .unwrap();
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(2, 0)),
            Some(PineRef::new(false))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(3, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![false])))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(4, 0)),
            Some(PineRef::new(false))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(5, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![false])))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(6, 0)),
            Some(PineRef::new_box(true))
        );
    }

    const NA_SCRIPT: &'static str = "
    v1 = close < 10 ? na : close    // CORRECT

    v2 = close == na ? 1 : close    // CORRECT
    v3 = na(close) ? 1 : close    // CORRECT
    ";

    #[test]
    fn na_expr_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let blk = PineParser::new(NA_SCRIPT, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![("close", AnySeries::from_float_vec(vec![Some(2f64), None]))],
                None,
            )
            .unwrap();
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(2, 0)),
            Some(PineRef::new(Series::from_vec(vec![
                Float::from(None),
                Float::from(None)
            ])))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(3, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![
                Some(2f64),
                Some(1f64),
            ])))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(4, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![
                Some(2f64),
                Some(1f64)
            ])))
        );
    }
}

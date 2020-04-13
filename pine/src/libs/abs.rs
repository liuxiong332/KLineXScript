use super::VarResult;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::{pine_ref_to_f64, pine_ref_to_i64};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::types::{Callable, Float, Int, PineFrom, PineRef, RuntimeErr, Series, NA};
use std::mem;
use std::rc::Rc;

fn int_abs<'a>(xval: Option<PineRef<'a>>) -> Int {
    match pine_ref_to_i64(xval) {
        None => None,
        Some(v) => Some(v.abs()),
    }
}

fn float_abs<'a>(xval: Option<PineRef<'a>>) -> Float {
    match pine_ref_to_f64(xval) {
        None => None,
        Some(v) => Some(v.abs()),
    }
}

fn abs_func<'a>(
    _context: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
    func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    let xval = mem::replace(&mut param[0], None);

    match ((func_type.signature.0)[0]).1 {
        SyntaxType::Simple(SimpleSyntaxType::Int) => {
            let res = int_abs(xval);
            Ok(PineRef::new_box(res))
        }
        SyntaxType::Series(SimpleSyntaxType::Int) => {
            let res = int_abs(xval);
            Ok(PineRef::new_rc(Series::from(res)))
        }
        SyntaxType::Simple(SimpleSyntaxType::Float) => {
            let res = float_abs(xval);
            Ok(PineRef::new_box(res))
        }
        SyntaxType::Series(SimpleSyntaxType::Float) => {
            let res = float_abs(xval);
            Ok(PineRef::new_rc(Series::from(res)))
        }
        _ => unreachable!(),
    }
}

pub const VAR_NAME: &'static str = "abs";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(Callable::new(Some(abs_func), None));

    // plot(series, title, color, linewidth, style, trackprice, transp, histbase, offset, join, editable, show_last) → plot

    let func_type = FunctionTypes(vec![
        FunctionType::new((vec![("x", SyntaxType::int())], SyntaxType::int())),
        FunctionType::new((
            vec![("x", SyntaxType::int_series())],
            SyntaxType::int_series(),
        )),
        FunctionType::new((vec![("x", SyntaxType::float())], SyntaxType::float())),
        FunctionType::new((
            vec![("x", SyntaxType::float_series())],
            SyntaxType::float_series(),
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
    fn abs_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = "m1 = abs(-123)\nm2 = abs(int(close))\nm3 = abs(-123.23)\nm4 = abs(close)";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![("close", AnySeries::from_float_vec(vec![Some(-2f64)]))],
                None,
            )
            .unwrap();
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(0, 0)),
            Some(PineRef::new(Some(123i64)))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(1, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(2i64)])))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(2, 0)),
            Some(PineRef::new(Some(123.23f64)))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(3, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(2f64)])))
        );
    }
}

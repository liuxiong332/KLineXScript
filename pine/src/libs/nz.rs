use super::VarResult;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::{
    move_element, pine_ref_to_bool, pine_ref_to_color, pine_ref_to_f64, pine_ref_to_i64,
    pine_ref_to_string,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::types::{Callable, Float, Int, PineFrom, PineRef, RuntimeErr, Series, NA};
use std::rc::Rc;

fn int_nz<'a>(xval: Option<PineRef<'a>>, yval: Option<PineRef<'a>>) -> Int {
    match pine_ref_to_i64(xval) {
        None => {
            let defint = match pine_ref_to_i64(yval) {
                None => 0i64,
                Some(v) => v,
            };
            Some(defint)
        }
        Some(v) => Some(v),
    }
}

fn float_nz<'a>(xval: Option<PineRef<'a>>, yval: Option<PineRef<'a>>) -> Float {
    match pine_ref_to_f64(xval) {
        None => {
            let deffloat = match pine_ref_to_f64(yval) {
                None => 0f64,
                Some(v) => v,
            };
            Some(deffloat)
        }
        Some(v) => Some(v),
    }
}

fn na_func<'a>(
    _context: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
    func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    move_tuplet!((xval, yval) = param);
    match func_type.get_type(0).unwrap() {
        SyntaxType::Simple(SimpleSyntaxType::Int) => {
            let res = int_nz(xval, yval);
            Ok(PineRef::new_box(res))
        }
        SyntaxType::Series(SimpleSyntaxType::Int) => {
            let res = int_nz(xval, yval);
            Ok(PineRef::new_rc(Series::from(res)))
        }
        SyntaxType::Simple(SimpleSyntaxType::Float) => {
            let res = float_nz(xval, yval);
            Ok(PineRef::new_box(res))
        }
        SyntaxType::Series(SimpleSyntaxType::Float) => {
            let res = float_nz(xval, yval);
            Ok(PineRef::new_rc(Series::from(res)))
        }
        _ => unreachable!(),
    }
}

pub const VAR_NAME: &'static str = "nz";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(Callable::new(Some(na_func), None));

    // plot(series, title, color, linewidth, style, trackprice, transp, histbase, offset, join, editable, show_last) → plot

    let func_type = FunctionTypes(vec![
        FunctionType::new((
            vec![("x", SyntaxType::int()), ("y", SyntaxType::int_series())],
            SyntaxType::int(),
        )),
        FunctionType::new((
            vec![
                ("x", SyntaxType::int_series()),
                ("y", SyntaxType::int_series()),
            ],
            SyntaxType::int_series(),
        )),
        FunctionType::new((
            vec![
                ("x", SyntaxType::float()),
                ("y", SyntaxType::float_series()),
            ],
            SyntaxType::float(),
        )),
        FunctionType::new((
            vec![
                ("x", SyntaxType::float_series()),
                ("y", SyntaxType::float_series()),
            ],
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
    fn na_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = "m2 = nz(int(close))\nm3 = nz(int(close), 1)\nm4 = nz(close)\n";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![("close", AnySeries::from_float_vec(vec![Float::from(None)]))],
                None,
            )
            .unwrap();
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(0, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(0i64)])))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(1, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(1i64)])))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(2, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(0f64)])))
        );
    }
}

use super::VarResult;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::{
    move_element, pine_ref_to_f64, pine_ref_to_f64_series, pine_ref_to_i64, series_index,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::types::{
    Arithmetic, Callable, CallableFactory, Float, Int, ParamCollectCall, PineFrom, PineRef,
    RuntimeErr, Series, NA,
};
use std::mem;
use std::rc::Rc;

fn change_func<'a>(
    _context: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    move_tuplet!((source, length) = param);
    let series = pine_ref_to_f64_series(source);
    let length = pine_ref_to_i64(length).unwrap_or(1i64) as usize;

    let val = series_index(&series, 0).minus(series_index(&series, length));
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
}

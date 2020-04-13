use super::ema::rma_func;
use super::falling::declare_s_var;
use super::VarResult;
use crate::ast::stat_expr_types::VarIndex;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::{
    float_abs, float_max, move_element, pine_ref_to_bool, pine_ref_to_f64, pine_ref_to_f64_series,
    pine_ref_to_i64, require_param, series_index,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::InputSrc;
use crate::types::{
    downcast_pf_ref, int2float, Arithmetic, Callable, CallableFactory, Float, Int, PineRef,
    RefData, RuntimeErr, Series, SeriesCall,
};

pub const VAR_NAME: &'static str = "rising";

fn check_greater<'a>(
    source: RefData<Series<Float>>,
    length: i64,
) -> Result<PineRef<'a>, RuntimeErr> {
    let cur_val = source.index_value(0).unwrap();
    for i in 1..=length as usize {
        if source.index_value(i).unwrap() > cur_val {
            return Ok(PineRef::new_rc(Series::from(false)));
        }
    }
    return Ok(PineRef::new_rc(Series::from(true)));
}

pub fn declare_var<'a>() -> VarResult<'a> {
    declare_s_var(VAR_NAME, check_greater)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::syntax_type::SyntaxType;
    use crate::runtime::VarOperate;
    use crate::runtime::{AnySeries, NoneCallback};
    use crate::types::Series;
    use crate::{LibInfo, PineParser, PineRunner};

    #[test]
    fn accdist_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::float_series())],
        );
        let src = "m = rising(close, 2)";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![(
                    "close",
                    AnySeries::from_float_vec(vec![Some(10f64), Some(20f64), Some(5f64)]),
                )],
                None,
            )
            .unwrap();
        assert_eq!(
            runner.get_context().get_var(VarIndex::new(0, 0)),
            &Some(PineRef::new(Series::from_vec(vec![true, true, false])))
        );
    }
}

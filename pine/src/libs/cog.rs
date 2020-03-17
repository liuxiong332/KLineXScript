use super::sma::declare_ma_var;
use super::VarResult;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::{
    move_element, pine_ref_to_bool, pine_ref_to_f64, pine_ref_to_f64_series, pine_ref_to_i64,
    require_param, series_index,
};
use crate::types::{
    downcast_pf_ref, int2float, Arithmetic, Callable, Evaluate, EvaluateVal, Float, Int, PineRef,
    RefData, RuntimeErr, Series, SeriesCall, NA,
};

fn cog_func<'a>(source: RefData<Series<Float>>, length: i64) -> Result<Float, RuntimeErr> {
    let mut sum_val1 = Some(0f64);
    let mut sum_val2 = Some(0f64);
    for i in 0..length {
        let val = source.index_value(i as usize).unwrap();
        sum_val1 = sum_val1.add(val);
        sum_val2 = sum_val2.add(val.mul(Some((length - i) as f64)))
    }
    match sum_val2.div(sum_val1) {
        Some(val) => Ok(Some(-val)),
        None => Ok(None),
    }
}

pub fn declare_var<'a>() -> VarResult<'a> {
    declare_ma_var("cog", cog_func)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::stat_expr_types::VarIndex;
    use crate::ast::syntax_type::SyntaxType;
    use crate::runtime::VarOperate;
    use crate::runtime::{AnySeries, NoneCallback};
    use crate::types::Series;
    use crate::{LibInfo, PineParser, PineRunner};
    // use crate::libs::{floor, exp, };

    #[test]
    fn sum_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::float_series())],
        );
        let src = "m = cog(close, 2)";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![(
                    "close",
                    AnySeries::from_float_vec(vec![Some(12f64), Some(12f64)]),
                )],
                None,
            )
            .unwrap();

        assert_eq!(
            runner.get_context().move_var(VarIndex::new(2, 0)),
            Some(PineRef::new(Series::from_vec(vec![None, Some(-1.5f64),])))
        );
    }
}

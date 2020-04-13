use super::sma::declare_ma_var;
use super::VarResult;
use crate::ast::stat_expr_types::VarIndex;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::err_msgs::*;
use crate::helper::str_replace;
use crate::helper::{
    move_element, pine_ref_to_bool, pine_ref_to_f64, pine_ref_to_f64_series, pine_ref_to_i64,
    require_param, series_index,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::InputSrc;
use crate::types::{
    downcast_pf_ref, int2float, Arithmetic, Callable, Evaluate, EvaluateVal, Float, Int, PineRef,
    RefData, RuntimeErr, Series, SeriesCall, NA,
};
use std::f64;
use std::mem;
use std::rc::Rc;

fn cmo_func<'a>(source: RefData<Series<Float>>, length: i64) -> Result<Float, RuntimeErr> {
    let mut sm1 = 0f64;
    let mut sm2 = 0f64;

    // float sm1 = sum((mom >= 0) ? mom : 0.0, length)
    // float sm2 = sum((mom >= 0) ? 0.0 : -mom, length)
    // 100 * (sm1 - sm2) / (sm1 + sm2)
    for i in 0..length {
        let cz_val = source
            .index_value(i as usize)
            .unwrap()
            .minus(source.index_value((i + 1) as usize).unwrap());
        let cz1 = match cz_val.clone() {
            Some(cz1) => {
                if cz1 >= 0f64 {
                    cz1
                } else {
                    0f64
                }
            }
            None => 0f64,
        };
        let cz2 = match cz_val.clone() {
            Some(cz_val) => {
                if cz_val >= 0f64 {
                    0f64
                } else {
                    -cz_val
                }
            }
            None => 0f64,
        };
        sm1 += cz1;
        sm2 += cz2;
    }

    let result = 100f64 * (sm1 - sm2) / (sm1 + sm2);
    if result.is_nan() {
        Ok(None)
    } else {
        Ok(Some(result))
    }
}

pub fn declare_var<'a>() -> VarResult<'a> {
    declare_ma_var("cmo", cmo_func)
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
    fn cmo_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::float_series())],
        );
        let src = "m = cmo(close, 2)";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![(
                    "close",
                    AnySeries::from_float_vec(vec![Some(12f64), Some(6f64), Some(12f64)]),
                )],
                None,
            )
            .unwrap();

        assert_eq!(
            runner.get_context().move_var(VarIndex::new(0, 0)),
            Some(PineRef::new(Series::from_vec(vec![
                None,
                Some(-100.0),
                Some(0.0)
            ])))
        );
    }
}

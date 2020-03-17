use super::ema::rma_func;
use super::highestbars::declare_s_var;
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
    downcast_pf_ref, int2float, Arithmetic, Callable, CallableCreator, CallableFactory, Float, Int,
    PineRef, RefData, RuntimeErr, Series, SeriesCall,
};
use std::mem;
use std::rc::Rc;

fn gen_low_index<'a>(ctx: &mut dyn Ctx<'a>) -> VarIndex {
    VarIndex::new(*ctx.get_varname_index("low").unwrap(), 0)
}

fn get_min_val<'a>(source: &Option<RefData<Series<Float>>>, length: i64) -> Int {
    let mut min_val = Some(std::f64::MAX);
    let mut min_i = Some(0);
    for i in 0..length as usize {
        let cur_val = series_index(source, i);
        if cur_val < min_val {
            min_i = Some(i as i64);
            min_val = cur_val;
        }
    }
    min_i
}

pub fn declare_var<'a>() -> VarResult<'a> {
    declare_s_var("lowestbars", gen_low_index, get_min_val)
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
            vec![
                ("close", SyntaxType::float_series()),
                ("low", SyntaxType::float_series()),
            ],
        );
        let src = "m1 = lowestbars(2)\nm2 = lowestbars(close, 2)";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![
                    (
                        "close",
                        AnySeries::from_float_vec(vec![Some(10f64), Some(20f64), Some(5f64)]),
                    ),
                    (
                        "low",
                        AnySeries::from_float_vec(vec![Some(19f64), Some(25f64), Some(10f64)]),
                    ),
                ],
                None,
            )
            .unwrap();
        assert_eq!(
            runner.get_context().get_var(VarIndex::new(3, 0)),
            &Some(PineRef::new(Series::from_vec(vec![
                Some(1i64),
                Some(1i64),
                Some(0i64)
            ])))
        );
    }
}

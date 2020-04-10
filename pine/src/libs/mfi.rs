use super::ema::ema_func;
use super::ema::rma_func;
use super::rsi::calc_rsi_series;
use super::sma::{declare_ma_var, wma_func};
use super::tr::tr_func;
use super::VarResult;
use crate::ast::stat_expr_types::VarIndex;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::err_msgs::*;
use crate::helper::str_replace;
use crate::helper::{
    ensure_srcs, float_abs, float_max2, move_element, pine_ref_to_bool, pine_ref_to_f64,
    pine_ref_to_f64_series, pine_ref_to_i64, require_param, series_index,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::InputSrc;
use crate::types::{
    downcast_pf_ref, float2int, int2float, Arithmetic, Callable, CallableCreator, CallableFactory,
    Evaluate, EvaluateVal, Float, Int, ParamCollectCall, PineRef, RefData, RuntimeErr, Series,
    SeriesCall, Tuple,
};
use std::rc::Rc;

fn sum_vec<'a>(source: &Vec<Float>, length: i64) -> Float {
    let mut sum_val = Some(0f64);
    for i in 0..length {
        if i < source.len() as i64 {
            let val = source[i as usize];
            sum_val = sum_val.add(val);
        }
    }
    sum_val
}

#[derive(Debug, Clone, PartialEq)]
pub struct KcVal {
    volume_index: VarIndex,
    upper_history: Vec<Float>,
    lower_history: Vec<Float>,
}

impl KcVal {
    pub fn new() -> KcVal {
        KcVal {
            volume_index: VarIndex::new(0, 0),
            upper_history: vec![],
            lower_history: vec![],
        }
    }

    fn handle_index<'a>(&mut self, ctx: &mut dyn Ctx<'a>) {
        ensure_srcs(ctx, vec!["volume"], |indexs| {
            self.volume_index = indexs[0];
        });
    }

    fn process_rsi<'a>(
        &mut self,
        _ctx: &mut dyn Ctx<'a>,
        mut param: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<Float, RuntimeErr> {
        self.handle_index(_ctx);
        move_tuplet!((series, length) = param);

        let series = require_param("series", pine_ref_to_f64_series(series))?;
        let length = require_param("length", pine_ref_to_i64(length))?;

        let s0 = series.index_value(0).unwrap();
        let s1 = series.index_value(1).unwrap();
        let volume = pine_ref_to_f64(_ctx.get_var(self.volume_index).clone());

        let upper = if s0.minus(s1) <= Some(0f64) {
            Some(0f64)
        } else {
            s0.mul(volume)
        };

        let lower = if s0.minus(s1) >= Some(0f64) {
            Some(0f64)
        } else {
            s0.mul(volume)
        };

        self.upper_history.push(upper);
        self.lower_history.push(lower);

        let upper_sum = sum_vec(&self.upper_history, length);
        let lower_sum = sum_vec(&self.lower_history, length);

        let res = if lower_sum.is_none() {
            None
        } else {
            let res = calc_rsi_series(upper_sum, lower_sum)?;
            res
        };
        Ok(res)
    }
}

impl<'a> SeriesCall<'a> for KcVal {
    fn step(
        &mut self,
        _ctx: &mut dyn Ctx<'a>,
        param: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        let res = self.process_rsi(_ctx, param, _func_type)?;
        Ok(PineRef::new_rc(Series::from(res)))
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        Box::new(self.clone())
    }
}

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(CallableFactory::new(|| {
        Callable::new(None, Some(Box::new(KcVal::new())))
    }));

    let func_type = FunctionTypes(vec![FunctionType::new((
        vec![
            ("series", SyntaxType::float_series()),
            ("length", SyntaxType::int()),
        ],
        SyntaxType::float_series(),
    ))]);
    let syntax_type = SyntaxType::Function(Rc::new(func_type));
    VarResult::new(value, syntax_type, "mfi")
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
    fn mfi_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![
                ("close", SyntaxType::float_series()),
                ("volume", SyntaxType::float_series()),
            ],
        );
        let src = "m = mfi(close, 2)\n";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![
                    (
                        "close",
                        AnySeries::from_float_vec(vec![Some(20f64), Some(10f64), Some(20f64)]),
                    ),
                    (
                        "volume",
                        AnySeries::from_int_vec(vec![Some(1i64), Some(1i64), Some(1i64)]),
                    ),
                ],
                None,
            )
            .unwrap();

        assert_eq!(
            runner.get_context().move_var(VarIndex::new(3, 0)),
            Some(PineRef::new(Series::from_vec(vec![
                Some(0f64),
                Some(0f64),
                Some(0f64),
            ])))
        );
    }
}

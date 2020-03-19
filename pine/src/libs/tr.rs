use super::ema::rma_func;
use super::VarResult;
use crate::ast::stat_expr_types::VarIndex;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::{
    ensure_srcs, float_abs, float_max, move_element, pine_ref_to_bool, pine_ref_to_f64,
    pine_ref_to_f64_series, pine_ref_to_i64, require_param, series_index,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::InputSrc;
use crate::types::{
    downcast_pf_ref, int2float, Arithmetic, Callable, CallableEvaluate, CallableFactory,
    EvaluateVal, Float, Int, ParamCollectCall, PineRef, RefData, RuntimeErr, Series, SeriesCall,
    NA,
};
use std::mem;
use std::rc::Rc;

pub fn tr_func(high: Float, low: Float, preclose: Float) -> Float {
    float_max(
        high.minus(low),
        float_abs(high.minus(preclose)),
        float_abs(low.minus(preclose)),
    )
}

#[derive(Debug, Clone, PartialEq)]
struct TrVal {
    close_index: VarIndex,
    low_index: VarIndex,
    high_index: VarIndex,
    prev_val: f64,
}

impl TrVal {
    pub fn new() -> TrVal {
        TrVal {
            close_index: VarIndex::new(0, 0),
            low_index: VarIndex::new(0, 0),
            high_index: VarIndex::new(0, 0),
            prev_val: 0f64,
        }
    }

    fn handle_index<'a>(&mut self, ctx: &mut dyn Ctx<'a>) {
        ensure_srcs(ctx, vec!["close", "low", "high"], |indexs| {
            self.close_index = indexs[0];
            self.low_index = indexs[1];
            self.high_index = indexs[2];
        });
    }

    fn calc_tr<'a>(
        &mut self,
        ctx: &mut dyn Ctx<'a>,
        handle_na: bool,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        let high = pine_ref_to_f64(ctx.get_var(self.high_index).clone());
        let low = pine_ref_to_f64(ctx.get_var(self.low_index).clone());

        let close = pine_ref_to_f64_series(ctx.get_var(self.close_index).clone()).unwrap();

        let preclose = close.index_value(1).unwrap();
        let mut res = tr_func(high, low, preclose);

        if handle_na {
            res = match res {
                None => Some(self.prev_val),
                Some(v) => {
                    self.prev_val = v;
                    Some(v)
                }
            };
        }
        // It is max(high - low, abs(high - close[1]), abs(low - close[1]))
        Ok(PineRef::new_rc(Series::from(res)))
    }
}

impl<'a> EvaluateVal<'a> for TrVal {
    fn custom_name(&self) -> &str {
        "tr"
    }

    fn call(&mut self, ctx: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, RuntimeErr> {
        self.handle_index(ctx);
        self.calc_tr(ctx, false)
    }

    fn copy(&self) -> Box<dyn EvaluateVal<'a>> {
        Box::new(self.clone())
    }
}

impl<'a> SeriesCall<'a> for TrVal {
    fn step(
        &mut self,
        ctx: &mut dyn Ctx<'a>,
        mut param: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        self.handle_index(ctx);
        let handle_na = pine_ref_to_bool(mem::replace(&mut param[0], None));
        self.calc_tr(ctx, handle_na.unwrap_or(false))
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        Box::new(self.clone())
    }
}

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(CallableEvaluate::new(Box::new(TrVal::new()), || {
        Callable::new(
            None,
            Some(Box::new(ParamCollectCall::new_with_caller(Box::new(
                TrVal::new(),
            )))),
        )
    }));

    // plot(series, title, color, linewidth, style, trackprice, transp, histbase, offset, join, editable, show_last) → plot

    let func_type = FunctionTypes(vec![FunctionType::new((
        vec![("handle_na", SyntaxType::bool())],
        SyntaxType::float_series(),
    ))]);
    let syntax_type =
        SyntaxType::ValFunction(Box::new(SyntaxType::float_series()), Rc::new(func_type));
    VarResult::new(value, syntax_type, "tr")
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
                ("high", SyntaxType::float_series()),
                ("low", SyntaxType::float_series()),
            ],
        );
        let src = "m = tr\nm2 = tr(true)\n";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![
                    (
                        "close",
                        AnySeries::from_float_vec(vec![Some(10f64), Some(20f64)]),
                    ),
                    (
                        "high",
                        AnySeries::from_float_vec(vec![Some(15f64), Some(22f64)]),
                    ),
                    (
                        "low",
                        AnySeries::from_float_vec(vec![Some(1f64), Some(2f64)]),
                    ),
                ],
                None,
            )
            .unwrap();

        assert_eq!(
            runner.get_context().get_var(VarIndex::new(4, 0)),
            &Some(PineRef::new(Series::from_vec(vec![
                Some(14f64),
                Some(20f64)
            ])))
        );
    }
}

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
    downcast_pf_ref, int2float, Arithmetic, Callable, CallableFactory, Float, Int, PineRef,
    RefData, RuntimeErr, Series, SeriesCall,
};
use std::mem;
use std::rc::Rc;

fn true_range(
    close: &Option<RefData<Series<Float>>>,
    high: &Option<RefData<Series<Float>>>,
    low: &Option<RefData<Series<Float>>>,
) -> Float {
    //  max(high - low, abs(high - close[1]), abs(low - close[1]))
    let v1 = series_index(high, 0).minus(series_index(low, 0));
    let v2 = float_abs(series_index(high, 0).minus(series_index(close, 1)));
    let v3 = float_abs(series_index(low, 0).minus(series_index(close, 1)));
    float_max(v1, v2, v3)
}

#[derive(Debug, Clone, PartialEq)]
struct AtrVal {
    close_index: VarIndex,
    low_index: VarIndex,
    high_index: VarIndex,
    val_history: Vec<Float>,
    prev_val: Float,
}

impl AtrVal {
    pub fn new() -> AtrVal {
        AtrVal {
            close_index: VarIndex::new(0, 0),
            low_index: VarIndex::new(0, 0),
            high_index: VarIndex::new(0, 0),
            val_history: vec![],
            prev_val: Some(0f64),
        }
    }
}

impl<'a> SeriesCall<'a> for AtrVal {
    fn step(
        &mut self,
        ctx: &mut dyn Ctx<'a>,
        mut param: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        ensure_srcs(ctx, vec!["close", "low", "high"], |indexs| {
            self.close_index = indexs[0];
            self.low_index = indexs[1];
            self.high_index = indexs[2];
        });

        let length = require_param("length", pine_ref_to_i64(mem::replace(&mut param[0], None)))?;

        let close = pine_ref_to_f64_series(ctx.get_var(self.close_index).clone());
        let low = pine_ref_to_f64_series(ctx.get_var(self.low_index).clone());
        let high = pine_ref_to_f64_series(ctx.get_var(self.high_index).clone());

        let result = rma_func(true_range(&close, &high, &low), length, self.prev_val)?;
        self.prev_val = result;
        self.val_history.push(result);
        Ok(PineRef::new(Series::from(result)))
    }

    fn back(&mut self, _context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        self.val_history.pop();
        self.prev_val = *self.val_history.last().unwrap();
        Ok(())
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        Box::new(self.clone())
    }
}
pub const VAR_NAME: &'static str = "atr";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(CallableFactory::new(|| {
        Callable::new(None, Some(Box::new(AtrVal::new())))
    }));

    // plot(series, title, color, linewidth, style, trackprice, transp, histbase, offset, join, editable, show_last) → plot

    let func_type = FunctionTypes(vec![FunctionType::new((
        vec![("length", SyntaxType::int())],
        SyntaxType::float_series(),
    ))]);
    let syntax_type = SyntaxType::Function(Rc::new(func_type));
    VarResult::new(value, syntax_type, VAR_NAME)
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
        let src = "m = atr(2)";
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

        let val1 = rma_func(Some(14f64), 2, None).unwrap();
        let val2 = rma_func(Some(20f64), 2, val1).unwrap();
        assert_eq!(
            runner.get_context().get_var(VarIndex::new(4, 0)),
            &Some(PineRef::new(Series::from_vec(vec![val1, val2])))
        );
    }
}

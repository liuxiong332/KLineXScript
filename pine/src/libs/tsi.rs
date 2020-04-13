use super::ema::ema_func;
use super::ema::rma_func;
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
    downcast_pf_ref, int2float, Arithmetic, Callable, CallableCreator, CallableFactory, Evaluate,
    EvaluateVal, Float, Int, ParamCollectCall, PineRef, RefData, RuntimeErr, Series, SeriesCall,
    Tuple,
};
use std::mem;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct KcVal<'a> {
    closes: Series<'a, Float>,
    smooth1: Series<'a, Float>,
    smooth2: Series<'a, Float>,
    asmooth1: Series<'a, Float>,
    asmooth2: Series<'a, Float>,
}

impl<'a> KcVal<'a> {
    pub fn new() -> KcVal<'a> {
        KcVal {
            closes: Series::new(),
            smooth1: Series::new(),
            smooth2: Series::new(),
            asmooth1: Series::new(),
            asmooth2: Series::new(),
        }
    }

    fn process_tsi(
        &mut self,
        _ctx: &mut dyn Ctx<'a>,
        mut param: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<Float, RuntimeErr> {
        move_tuplet!((source, short, long) = param);

        let short = require_param("short", pine_ref_to_i64(short))?;
        let long = require_param("long", pine_ref_to_i64(long))?;

        // Double Smoothed PC
        // ------------------
        // PC = Current Price minus Prior Price
        // First Smoothing = 25-period EMA of PC
        // Second Smoothing = 13-period EMA of 25-period EMA of PC
        let close = pine_ref_to_f64(source);
        let pc = close.minus(self.closes.index_value(1)?);
        let s1val = ema_func(pc, long, self.smooth1.index_value(1)?)?;
        let s2val = ema_func(s1val, short, self.smooth2.index_value(1)?)?;

        // Double Smoothed Absolute PC
        // ---------------------------
        // Absolute Price Change |PC| = Absolute Value of Current Price minus Prior Price
        // First Smoothing = 25-period EMA of |PC|
        // Second Smoothing = 13-period EMA of 25-period EMA of |PC|
        let apc = float_abs(close.minus(self.closes.index_value(1)?));
        let as1val = ema_func(apc, long, self.asmooth1.index_value(1)?)?;
        let as2val = ema_func(as1val, short, self.asmooth2.index_value(1)?)?;

        // TSI = 100 x (Double Smoothed PC / Double Smoothed Absolute PC)
        let tsi = Some(100f64).mul(s2val).div(as2val);
        self.closes.update_commit(close);
        self.smooth1.update_commit(s1val);
        self.smooth2.update_commit(s2val);
        self.asmooth1.update_commit(as1val);
        self.asmooth2.update_commit(as2val);
        Ok(tsi)
    }
}

impl<'a> SeriesCall<'a> for KcVal<'a> {
    fn step(
        &mut self,
        _ctx: &mut dyn Ctx<'a>,
        param: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        let res = self.process_tsi(_ctx, param, _func_type)?;
        Ok(PineRef::new_rc(Series::from(res)))
    }

    fn back(&mut self, _context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        self.closes.roll_back();
        self.smooth1.roll_back();
        self.smooth2.roll_back();
        self.asmooth1.roll_back();
        self.asmooth2.roll_back();
        Ok(())
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
            ("source", SyntaxType::float_series()),
            ("short_length", SyntaxType::int()),
            ("long_length", SyntaxType::int()),
        ],
        SyntaxType::float_series(),
    ))]);
    let syntax_type = SyntaxType::Function(Rc::new(func_type));
    VarResult::new(value, syntax_type, "tsi")
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
    fn rsi_int_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::float_series())],
        );
        let src = "m = tsi(close, 2, 2)\n";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![(
                    "close",
                    AnySeries::from_float_vec(vec![Some(20f64), Some(10f64)]),
                )],
                None,
            )
            .unwrap();

        assert_eq!(
            runner.get_context().move_var(VarIndex::new(0, 0)),
            Some(PineRef::new(Series::from_vec(vec![None, Some(-100.0)])))
        );
    }
}

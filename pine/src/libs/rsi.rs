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
pub struct KcVal {
    prev_upward: Float,
    prev_downward: Float,
}

impl KcVal {
    pub fn new() -> KcVal {
        KcVal {
            prev_upward: None,
            prev_downward: None,
        }
    }

    fn process_rsi<'a>(
        &mut self,
        _ctx: &mut dyn Ctx<'a>,
        mut param: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<Float, RuntimeErr> {
        move_tuplet!((x, y) = param);

        let series = require_param("x", pine_ref_to_f64_series(x))?;
        let length = require_param("y", pine_ref_to_i64(y))?;

        let s0 = series.index_value(0).unwrap();
        let s1 = series.index_value(1).unwrap();
        let upward = float_max2(s0.minus(s1), Some(0f64));
        let downward = float_max2(s1.minus(s0), Some(0f64));

        let rma1 = rma_func(upward, length, self.prev_upward)?;
        let rma2 = rma_func(downward, length, self.prev_downward)?;
        let rs = rma1.div(rma2);

        let res = Some(100f64).minus(Some(100f64).div(rs.add(Some(1f64))));

        self.prev_downward = downward;
        self.prev_upward = upward;
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
        vec![("x", SyntaxType::float_series()), ("y", SyntaxType::int())],
        SyntaxType::float_series(),
    ))]);
    let syntax_type = SyntaxType::Function(Rc::new(func_type));
    VarResult::new(value, syntax_type, "rsi")
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
    fn alma_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::float_series())],
        );
        let src = "m = rsi(close, 2)\n";
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
            runner.get_context().move_var(VarIndex::new(2, 0)),
            Some(PineRef::new(Series::from_vec(vec![None, Some(0.0)])))
        );
    }
}

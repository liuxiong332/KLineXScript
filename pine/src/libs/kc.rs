use super::ema::ema_func;
use super::sma::{declare_ma_var, wma_func};
use super::tr::tr_func;
use super::VarResult;
use crate::ast::stat_expr_types::VarIndex;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::err_msgs::*;
use crate::helper::str_replace;
use crate::helper::{
    ensure_srcs, move_element, pine_ref_to_bool, pine_ref_to_f64, pine_ref_to_f64_series,
    pine_ref_to_i64, require_param,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::InputSrc;
use crate::types::{
    downcast_pf_ref, int2float, Arithmetic, Callable, CallableCreator, CallableFactory, Evaluate,
    EvaluateVal, Float, Int, ParamCollectCall, PineRef, RefData, RuntimeErr, Series, SeriesCall,
    Tuple,
};
use std::f64;
use std::mem;
use std::rc::Rc;

pub type ValGenerator<'a> = fn((Float, Float, Float)) -> PineRef<'a>;

fn kc_generator<'a>(vals: (Float, Float, Float)) -> PineRef<'a> {
    let (basis, ema1, ema2) = vals;
    PineRef::new(Tuple(vec![
        PineRef::new_rc(Series::from(basis)),
        PineRef::new_rc(Series::from(ema1)),
        PineRef::new_rc(Series::from(ema2)),
    ]))
}

#[derive(Debug, Clone, PartialEq)]
pub struct KcVal {
    close_index: VarIndex,
    low_index: VarIndex,
    high_index: VarIndex,
    prev_basis: Float,
    prev_range_ema: Float,
    val_gen: *mut (),
}

impl KcVal {
    pub fn new(val_gen: *mut ()) -> KcVal {
        KcVal {
            close_index: VarIndex::new(0, 0),
            low_index: VarIndex::new(0, 0),
            high_index: VarIndex::new(0, 0),
            prev_basis: None,
            prev_range_ema: None,
            val_gen: val_gen,
        }
    }

    fn handle_index<'a>(&mut self, ctx: &mut dyn Ctx<'a>) {
        ensure_srcs(ctx, vec!["close", "low", "high"], |indexs| {
            self.close_index = indexs[0];
            self.low_index = indexs[1];
            self.high_index = indexs[2];
        });
    }

    fn process_kc<'a>(
        &mut self,
        _ctx: &mut dyn Ctx<'a>,
        mut param: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<(Float, Float, Float), RuntimeErr> {
        self.handle_index(_ctx);

        move_tuplet!((series, length, multi, use_true_range) = param);

        let sval = pine_ref_to_f64(series);
        let length = require_param("length", pine_ref_to_i64(length))?;
        let multi = require_param("multi", pine_ref_to_f64(multi))?;
        let use_true_range = pine_ref_to_bool(use_true_range).unwrap_or(false);

        let basis = ema_func(sval, length, self.prev_basis)?;

        let high = pine_ref_to_f64(_ctx.get_var(self.high_index).clone());
        let low = pine_ref_to_f64(_ctx.get_var(self.low_index).clone());

        let range = if use_true_range {
            let close = pine_ref_to_f64_series(_ctx.get_var(self.close_index).clone()).unwrap();
            let preclose = close.index_value(1).unwrap();
            tr_func(high, low, preclose)
        } else {
            high.minus(low)
        };

        let range_ema = ema_func(range, length, self.prev_range_ema)?;

        self.prev_basis = basis;
        self.prev_range_ema = range_ema;

        let multi_ema = range_ema.mul(Some(multi));
        Ok((basis, basis.add(multi_ema), basis.minus(multi_ema)))
    }
}

impl<'a> SeriesCall<'a> for KcVal {
    fn step(
        &mut self,
        _ctx: &mut dyn Ctx<'a>,
        param: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        let func = unsafe { mem::transmute::<_, ValGenerator<'a>>(self.val_gen) };
        Ok(func(self.process_kc(_ctx, param, _func_type)?))
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        Box::new(self.clone())
    }
}

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(CallableFactory::new(|| {
        Callable::new(
            None,
            Some(Box::new(ParamCollectCall::new_with_caller(Box::new(
                KcVal::new(kc_generator as *mut ()),
            )))),
        )
    }));

    let func_type = FunctionTypes(vec![FunctionType::new((
        vec![
            ("series", SyntaxType::float_series()),
            ("length", SyntaxType::int()),
            ("mult", SyntaxType::float()),
            ("useTrueRange", SyntaxType::bool()),
        ],
        SyntaxType::Tuple(Rc::new(vec![
            SyntaxType::float_series(),
            SyntaxType::float_series(),
            SyntaxType::float_series(),
        ])),
    ))]);
    let syntax_type = SyntaxType::Function(Rc::new(func_type));
    VarResult::new(value, syntax_type, "kc")
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
            vec![
                ("close", SyntaxType::float_series()),
                ("high", SyntaxType::float_series()),
                ("low", SyntaxType::float_series()),
            ],
        );
        let src = "[m1, m2, m3] = kc(close, 3, 1)\n";
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
                        AnySeries::from_float_vec(vec![Some(6f64), Some(20f64)]),
                    ),
                    (
                        "low",
                        AnySeries::from_float_vec(vec![Some(6f64), Some(20f64)]),
                    ),
                ],
                None,
            )
            .unwrap();

        assert_eq!(
            runner.get_context().move_var(VarIndex::new(4, 0)),
            Some(PineRef::new(Series::from_vec(vec![
                Some(5f64),
                Some(12.5f64)
            ])))
        );
    }
}

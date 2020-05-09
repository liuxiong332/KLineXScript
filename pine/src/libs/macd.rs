use super::ema::series_ema;
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
use std::mem;
use std::rc::Rc;

pub type ValGenerator<'a> = fn((Float, Float, Float)) -> PineRef<'a>;

fn val_generator<'a>(vals: (Float, Float, Float)) -> PineRef<'a> {
    let (basis, ema1, ema2) = vals;
    PineRef::new(Tuple(vec![
        PineRef::new_rc(Series::from(basis)),
        PineRef::new_rc(Series::from(ema1)),
        PineRef::new_rc(Series::from(ema2)),
    ]))
}

#[derive(Debug, Clone, PartialEq)]
pub struct KcVal<'a> {
    ema1s: Series<'a, Float>,
    ema2s: Series<'a, Float>,
    dems: Series<'a, Float>,
}

impl<'a> KcVal<'a> {
    pub fn new() -> KcVal<'a> {
        KcVal {
            ema1s: Series::new(),
            ema2s: Series::new(),
            dems: Series::new(),
        }
    }

    fn process_macd(
        &mut self,
        _ctx: &mut dyn Ctx<'a>,
        mut param: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<(Float, Float, Float), RuntimeErr> {
        move_tuplet!((source, fastlen, slowlen, siglen) = param);

        let close = pine_ref_to_f64(source);
        let fastlen = require_param("fastlen", pine_ref_to_i64(fastlen))?;
        let slowlen = require_param("slowlen", pine_ref_to_i64(slowlen))?;
        let siglen = require_param("siglen", pine_ref_to_i64(siglen))?;

        let ema1 = series_ema(close, fastlen, &mut self.ema1s)?;
        let ema2 = series_ema(close, slowlen, &mut self.ema2s)?;
        let dif = ema1.minus(ema2);

        let dem = series_ema(dif, siglen, &mut self.dems)?;
        let osc = dif.minus(dem);

        self.ema1s.commit();
        self.ema2s.commit();
        self.dems.commit();

        Ok((dif, dem, osc))
    }
}

impl<'a> SeriesCall<'a> for KcVal<'a> {
    fn step(
        &mut self,
        _ctx: &mut dyn Ctx<'a>,
        param: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        Ok(val_generator(self.process_macd(_ctx, param, _func_type)?))
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
                KcVal::new(),
            )))),
        )
    }));

    let func_type = FunctionTypes(vec![FunctionType::new((
        vec![
            ("source", SyntaxType::float_series()),
            ("fastlen", SyntaxType::int()),
            ("slowlen", SyntaxType::int()),
            ("siglen", SyntaxType::int()),
        ],
        SyntaxType::Tuple(Rc::new(vec![
            SyntaxType::float_series(),
            SyntaxType::float_series(),
            SyntaxType::float_series(),
        ])),
    ))]);
    let syntax_type = SyntaxType::Function(Rc::new(func_type));
    VarResult::new(value, syntax_type, "macd")
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
        let src = "[m1, m2, m3] = macd(close, 12, 26, 9)\n";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![(
                    "close",
                    AnySeries::from_float_vec(vec![Some(10f64), Some(20f64)]),
                )],
                None,
            )
            .unwrap();

        // assert_eq!(
        //     runner.get_context().move_var(VarIndex::new(2, 0)),
        //     Some(PineRef::new(Series::from_vec(vec![
        //         Some(5f64),
        //         Some(12.5f64)
        //     ])))
        // );
    }
}

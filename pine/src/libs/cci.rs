use super::ema::rma_func;
use super::sma::{series_dev, series_sma};
use super::VarResult;
use crate::ast::stat_expr_types::VarIndex;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::err_msgs::*;
use crate::helper::str_replace;
use crate::helper::{
    ensure_srcs, move_element, pine_ref_to_bool, pine_ref_to_f64, pine_ref_to_f64_series,
    pine_ref_to_i64, require_param, series_index,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::InputSrc;
use crate::types::{
    downcast_pf_ref, int2float, Arithmetic, Callable, CallableFactory, Float, Int,
    ParamCollectCall, PineRef, RefData, RuntimeErr, Series, SeriesCall,
};
use std::cmp;
use std::mem;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
struct CciVal {
    // close_index: VarIndex,
    ma_history: Vec<Float>,
    tp_history: Vec<Float>,
}

impl CciVal {
    pub fn new() -> CciVal {
        CciVal {
            ma_history: vec![],
            tp_history: vec![],
        }
    }
}

// cci = (src - sma(src, length)) / (0.015 * dev(src, length))
impl<'a> SeriesCall<'a> for CciVal {
    fn step(
        &mut self,
        ctx: &mut dyn Ctx<'a>,
        mut param: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        let series = require_param(
            "source",
            pine_ref_to_f64_series(mem::replace(&mut param[0], None)),
        )?;

        let length = require_param("length", pine_ref_to_i64(mem::replace(&mut param[1], None)))?;
        if length < 1i64 {
            return Err(RuntimeErr::InvalidParameters(str_replace(
                GE_1,
                vec![String::from("length")],
            )));
        }

        let cci_val = series
            .at(0)
            .minus(series_sma(&series, length)?)
            .div(Some(0.015f64).mul(series_dev(&series, length)?));
        Ok(PineRef::new(Series::from(cci_val)))
    }

    fn back(&mut self, _context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        self.ma_history.pop();
        self.tp_history.pop();
        Ok(())
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        Box::new(self.clone())
    }
}
pub const VAR_NAME: &'static str = "cci";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(CallableFactory::new(|| {
        Callable::new(
            None,
            Some(Box::new(ParamCollectCall::new_with_caller(Box::new(
                CciVal::new(),
            )))),
        )
    }));

    let func_type = FunctionTypes(vec![FunctionType::new((
        vec![
            ("source", SyntaxType::float_series()),
            ("length", SyntaxType::int()),
        ],
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
    use std::f64;

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
        let src = "m = cci(close, 2)";
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
                        AnySeries::from_float_vec(vec![Some(2f64), Some(0f64)]),
                    ),
                ],
                None,
            )
            .unwrap();
        assert_eq!(
            runner.get_context().get_var(VarIndex::new(0, 0)),
            &Some(PineRef::new(Series::from_vec(vec![
                None,
                Some(5f64 / (5f64 * 0.015f64))
            ])))
        );
    }
}

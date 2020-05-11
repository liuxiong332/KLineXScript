use super::sma::{declare_ma_var, series_wma};
use super::VarResult;
use crate::ast::stat_expr_types::VarIndex;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::err_msgs::*;
use crate::helper::str_replace;
use crate::helper::{
    ge1_param_i64, move_element, pine_ref_to_bool, pine_ref_to_f64, pine_ref_to_f64_series,
    pine_ref_to_i64, require_param,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::InputSrc;
use crate::types::{
    downcast_pf_ref, int2float, Arithmetic, Callable, CallableCreator, CallableFactory, Evaluate,
    EvaluateVal, Float, Int, ParamCollectCall, PineRef, RefData, RuntimeErr, Series, SeriesCall,
};
use std::f64;
use std::rc::Rc;

// X=2*WMA(C,ROUND(N/2))-WMA(C,N);
// HULLMA=WMA(X,ROUND(SQRT(N)));
fn calc_x<'a>(source: &Series<Float>, length: i64) -> Result<Float, RuntimeErr> {
    let val1 = series_wma(source, (length as f64 / 2f64).floor() as i64)?;
    let val2 = series_wma(source, length)?;
    let xval = val1.mul(Some(2f64)).minus(val2);
    Ok(xval)
}

fn calc_hullma(srcs: &Series<Float>, length: i64) -> Result<Float, RuntimeErr> {
    let sqrt_n = (length as f64).sqrt().round() as i64;
    series_wma(srcs, sqrt_n)
}

#[derive(Debug, Clone, PartialEq)]
struct HmaVal<'a> {
    val_history: Series<'a, Float>,
}

impl<'a> HmaVal<'a> {
    pub fn new() -> HmaVal<'a> {
        HmaVal {
            val_history: Series::new(),
        }
    }
}

impl<'a> SeriesCall<'a> for HmaVal<'a> {
    fn step(
        &mut self,
        _ctx: &mut dyn Ctx<'a>,
        mut param: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        move_tuplet!((source, length) = param);

        let source = require_param("source", pine_ref_to_f64_series(source))?;
        let length = ge1_param_i64("length", pine_ref_to_i64(length))?;
        let val = calc_x(&*source, length)?;

        self.val_history.update(val);
        let hullma = calc_hullma(&self.val_history, length)?;

        self.val_history.commit();
        Ok(PineRef::new(Series::from(hullma)))
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
                HmaVal::new(),
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
    VarResult::new(value, syntax_type, "hma")
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
        let src = "m = hma(close, 2)\n";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![(
                    "close",
                    AnySeries::from_float_vec(vec![
                        Some(6f64),
                        Some(12f64),
                        Some(6f64),
                        Some(12f64),
                    ]),
                )],
                None,
            )
            .unwrap();

        assert_eq!(
            runner.get_context().move_var(VarIndex::new(0, 0)),
            Some(PineRef::new(Series::from_vec(vec![
                None,
                Some(14f64),
                Some(4f64),
                Some(14f64)
            ])))
        );
    }
}

// 6 12 6 12

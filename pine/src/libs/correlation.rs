use super::sma::{series_sma, series_stdev};
use super::VarResult;
use crate::ast::stat_expr_types::VarIndex;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::err_msgs::*;
use crate::helper::str_replace;
use crate::helper::{
    ge1_param_i64, move_element, pine_ref_to_bool, pine_ref_to_f64, pine_ref_to_f64_series,
    pine_ref_to_i64, require_param, series_mul,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::InputSrc;
use crate::types::{
    downcast_pf_ref, int2float, Arithmetic, Callable, CallableCreator, CallableFactory, Evaluate,
    EvaluateVal, Float, Int, ParamCollectCall, PineRef, RefData, RuntimeErr, Series, SeriesCall,
    NA,
};
use std::f64;
use std::rc::Rc;

// conv = sma(close * open, 2) - sma(close, 2) * sma(open, 2)
// cor = conv / (stdev(close, 2) * stdev(open, 2))
pub fn series_correlation<'a>(
    source_a: &Series<Float>,
    source_b: &Series<Float>,
    length: i64,
    abmul: &mut Series<Float>,
) -> Result<Float, RuntimeErr> {
    series_mul(source_a.at(0), source_b.at(0), abmul);
    let conv = series_sma(abmul, length)?
        .minus(series_sma(source_a, length)?.mul(series_sma(source_b, length)?));
    let cor = conv.div(series_stdev(source_a, length)?.mul(series_stdev(source_b, length)?));
    Ok(cor)
}

#[derive(Debug, Clone, PartialEq)]
struct CorrelationVal<'a> {
    abmul: Series<'a, Float>,
}

impl<'a> CorrelationVal<'a> {
    fn new() -> CorrelationVal<'a> {
        CorrelationVal {
            abmul: Series::new(),
        }
    }
}

impl<'a> SeriesCall<'a> for CorrelationVal<'a> {
    fn step(
        &mut self,
        _ctx: &mut dyn Ctx<'a>,
        mut param: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        move_tuplet!((source_a, source_b, length) = param);

        let source_a = require_param("source_a", pine_ref_to_f64_series(source_a))?;
        let source_b = require_param("source_b", pine_ref_to_f64_series(source_b))?;
        let length = ge1_param_i64("length", pine_ref_to_i64(length))?;

        let cor_val = series_correlation(&*source_a, &*source_b, length, &mut self.abmul)?;
        self.abmul.commit();
        Ok(PineRef::new(Series::from(cor_val)))
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
                CorrelationVal::new(),
            )))),
        )
    }));

    let func_type = FunctionTypes(vec![FunctionType::new((
        vec![
            ("source_a", SyntaxType::float_series()),
            ("source_b", SyntaxType::float_series()),
            ("length", SyntaxType::int()),
        ],
        SyntaxType::float_series(),
    ))]);
    let syntax_type = SyntaxType::Function(Rc::new(func_type));
    VarResult::new(value, syntax_type, "correlation")
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
    fn correlation_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![
                ("close", SyntaxType::float_series()),
                ("open", SyntaxType::float_series()),
            ],
        );
        let src = "m = correlation(close, open, 2)";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![
                    (
                        "close",
                        AnySeries::from_float_vec(vec![Some(1f64), Some(2f64)]),
                    ),
                    (
                        "open",
                        AnySeries::from_float_vec(vec![Some(2f64), Some(4f64)]),
                    ),
                ],
                None,
            )
            .unwrap();

        // assert_eq!(
        //     runner.get_context().move_var(VarIndex::new(3, 0)),
        //     Some(PineRef::new(Series::from_vec(vec![Some(0f64), Some(9f64)])))
        // );
    }

    #[test]
    fn correlation_len_err_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![
                ("close", SyntaxType::float_series()),
                ("open", SyntaxType::float_series()),
            ],
        );
        let src = "m = correlation(close, open, -2)";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        assert!(runner
            .run(
                &vec![
                    (
                        "close",
                        AnySeries::from_float_vec(vec![Some(1f64), Some(2f64)]),
                    ),
                    (
                        "open",
                        AnySeries::from_float_vec(vec![Some(2f64), Some(4f64)]),
                    ),
                ],
                None,
            )
            .is_err());
    }
}

use super::VarResult;
use crate::ast::stat_expr_types::VarIndex;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::err_msgs::*;
use crate::helper::str_replace;
use crate::helper::{
    move_element, pine_ref_to_bool, pine_ref_to_f64, pine_ref_to_f64_series, pine_ref_to_i64,
    require_param,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::InputSrc;
use crate::types::{
    downcast_pf_ref, int2float, Arithmetic, Callable, CallableFactory, Evaluate, EvaluateVal,
    Float, Int, ParamCollectCall, PineRef, RuntimeErr, Series, SeriesCall, NA,
};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
struct AlmaVal;

impl<'a> SeriesCall<'a> for AlmaVal {
    fn step(
        &mut self,
        _ctx: &mut dyn Ctx<'a>,
        mut param: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        move_tuplet!((series, length, offset, sigma) = param);

        let series = require_param("series", pine_ref_to_f64_series(series))?;
        let length = require_param("length", pine_ref_to_f64(length))?;
        let offset = require_param("offset", pine_ref_to_f64(offset))?;
        let sigma = require_param("sigma", pine_ref_to_f64(sigma))?;

        if length < 1f64 {
            return Err(RuntimeErr::InvalidParameters(str_replace(
                GE_1,
                vec![String::from("length")],
            )));
        }
        // m = floor(offset * (windowsize - 1))
        let m = (offset * (length - 1f64)).floor();

        // s = windowsize / sigma
        let s = length / sigma;

        // norm = 0.0
        // sum = 0.0
        // for i = 0 to windowsize - 1
        //     weight = exp(-1 * pow(i - m, 2) / (2 * pow(s, 2)))
        //     norm := norm + weight
        //     sum := sum + series[windowsize - i - 1] * weight
        // sum / norm
        let mut norm = 0f64;
        let mut sum = 0f64;
        for i in 0..length as usize {
            let weight = ((-1f64 * (i as f64 - m).powi(2)) / (2f64 * s.powi(2))).exp();
            norm += weight;
            match series.index_value(length as usize - i - 1)? {
                Some(val) => {
                    sum += val * weight;
                }
                None => {
                    return Ok(PineRef::new_rc(Series::from(Float::from(None))));
                }
            }
        }
        Ok(PineRef::new(Series::from(Some(sum / norm))))
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        Box::new(self.clone())
    }
}

pub const VAR_NAME: &'static str = "alma";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(CallableFactory::new(|| {
        Callable::new(
            None,
            Some(Box::new(ParamCollectCall::new_with_caller(Box::new(
                AlmaVal,
            )))),
        )
    }));

    let func_type = FunctionTypes(vec![FunctionType::new((
        vec![
            ("series", SyntaxType::float_series()),
            ("length", SyntaxType::float()),
            ("offset", SyntaxType::float()),
            ("sigma", SyntaxType::float()),
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
    // use crate::libs::{floor, exp, };

    #[test]
    fn alma_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::float_series())],
        );
        let src = "alma(close, 9, 0.85, 6)";
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
    }

    #[test]
    fn alma_len_err_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::float_series())],
        );
        let src = "alma(close, -9, 0.85, 6)";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        assert!(runner
            .run(
                &vec![(
                    "close",
                    AnySeries::from_float_vec(vec![Some(10f64), Some(20f64)]),
                )],
                None,
            )
            .is_err());
    }
}

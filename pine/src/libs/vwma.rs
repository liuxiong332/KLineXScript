use super::VarResult;
use crate::ast::stat_expr_types::VarIndex;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::err_msgs::*;
use crate::helper::str_replace;
use crate::helper::{
    ensure_srcs, move_element, pine_ref_to_bool, pine_ref_to_f64, pine_ref_to_f64_series,
    pine_ref_to_i64, pine_ref_to_i64_series, require_param,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::InputSrc;
use crate::types::{
    downcast_pf_ref, int2float, Arithmetic, Callable, CallableFactory, Evaluate, EvaluateVal,
    Float, Int, PineRef, RefData, RuntimeErr, Series, SeriesCall, NA,
};
use std::rc::Rc;

pub fn swma_func<'a>(
    source: RefData<Series<Float>>,
    volume: RefData<Series<Int>>,
    length: i64,
) -> Result<Float, RuntimeErr> {
    let mut sum1 = 0f64;
    let mut sum2 = 0f64;
    for i in 0..length as usize {
        match (source.index_value(i)?, volume.index_value(i)?) {
            (Some(val), Some(vol)) => {
                sum1 += val * vol as f64 / length as f64;
                sum2 += vol as f64 / length as f64;
            }
            _ => {
                return Ok(Float::from(None));
            }
        }
    }
    Ok(Some(sum1 / sum2))
}

#[derive(Debug, Clone, PartialEq)]
struct EmaVal {
    volume_index: VarIndex,
}

impl EmaVal {
    pub fn new() -> EmaVal {
        EmaVal {
            volume_index: VarIndex::new(0, 0),
        }
    }
}

impl<'a> SeriesCall<'a> for EmaVal {
    fn step(
        &mut self,
        _ctx: &mut dyn Ctx<'a>,
        mut param: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        ensure_srcs(_ctx, vec!["volume"], |indexs| {
            self.volume_index = indexs[0];
        });
        move_tuplet!((source, length) = param);

        let source = require_param("source", pine_ref_to_f64_series(source))?;
        let length = require_param("length", pine_ref_to_i64(length))?;
        let volume = pine_ref_to_i64_series(_ctx.get_var(self.volume_index).clone()).unwrap();
        let result = swma_func(source, volume, length)?;
        Ok(PineRef::new(Series::from(result)))
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        Box::new(self.clone())
    }
}

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(CallableFactory::new(|| {
        Callable::new(None, Some(Box::new(EmaVal::new())))
    }));

    let func_type = FunctionTypes(vec![FunctionType::new((
        vec![
            ("source", SyntaxType::float_series()),
            ("length", SyntaxType::int()),
        ],
        SyntaxType::float_series(),
    ))]);
    let syntax_type = SyntaxType::Function(Rc::new(func_type));
    VarResult::new(value, syntax_type, "vwma")
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
    fn vwma_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![
                ("close", SyntaxType::float_series()),
                ("volume", SyntaxType::int_series()),
            ],
        );
        let src = "m1 = vwma(close, 2)";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![
                    (
                        "close",
                        AnySeries::from_float_vec(vec![
                            Some(6f64),
                            Some(12f64),
                            Some(12f64),
                            Some(6f64),
                        ]),
                    ),
                    (
                        "volume",
                        AnySeries::from_int_vec(vec![
                            Some(1i64),
                            Some(1i64),
                            Some(1i64),
                            Some(1i64),
                        ]),
                    ),
                ],
                None,
            )
            .unwrap();

        assert_eq!(
            runner.get_context().move_var(VarIndex::new(0, 0)),
            Some(PineRef::new(Series::from_vec(vec![
                None,
                Some(9f64),
                Some(12f64),
                Some(9f64)
            ])))
        );
    }
}

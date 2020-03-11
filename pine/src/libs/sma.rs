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
    downcast_pf_ref, int2float, Arithmetic, Callable, Evaluate, EvaluateVal, Float, Int, PineRef,
    RefData, RuntimeErr, Series, SeriesCall, NA,
};
use std::mem;
use std::rc::Rc;

fn sma_func<'a>(source: RefData<Series<Float>>, length: i64) -> Result<PineRef<'a>, RuntimeErr> {
    let mut sum = 0f64;
    for i in 0..length as usize {
        match source.index_value(i)? {
            Some(val) => {
                sum += val / length as f64;
            }
            None => {
                return Ok(PineRef::new_rc(Series::from(Float::from(None))));
            }
        }
    }
    Ok(PineRef::new(Series::from(Some(sum))))
}

fn wma_func<'a>(source: RefData<Series<Float>>, length: i64) -> Result<PineRef<'a>, RuntimeErr> {
    let mut norm = 0f64;
    let mut sum = 0f64;
    for i in 0..length as usize {
        let weight = ((length - i as i64) * length) as f64;
        norm += weight;
        match source.index_value(i)? {
            Some(val) => {
                sum += val * weight as f64;
            }
            None => {
                return Ok(PineRef::new_rc(Series::from(Float::from(None))));
            }
        }
    }
    Ok(PineRef::new(Series::from(Some(sum / norm))))
}

type HandleFunc<'a> = fn(RefData<Series<Float>>, i64) -> Result<PineRef<'a>, RuntimeErr>;

#[derive(Debug, Clone, PartialEq)]
struct SmaVal {
    ma_func: *mut (),
}

impl SmaVal {
    pub fn new(ma_func: *mut ()) -> SmaVal {
        SmaVal { ma_func }
    }
}

impl<'a> SeriesCall<'a> for SmaVal {
    fn step(
        &mut self,
        _ctx: &mut dyn Ctx<'a>,
        mut param: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        move_tuplet!((source, length) = param);

        let func = unsafe { mem::transmute::<_, HandleFunc<'a>>(self.ma_func) };
        let source = require_param("source", pine_ref_to_f64_series(source))?;
        let length = require_param("length", pine_ref_to_i64(length))?;
        func(source, length)
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        Box::new(self.clone())
    }
}

fn declare_ma_var<'a>(name: &'static str, handle: HandleFunc<'a>) -> VarResult<'a> {
    let value = PineRef::new(Callable::new(
        None,
        Some(Box::new(SmaVal::new(handle as *mut ()))),
    ));

    let func_type = FunctionTypes(vec![FunctionType::new((
        vec![
            ("source", SyntaxType::float_series()),
            ("length", SyntaxType::int()),
        ],
        SyntaxType::float_series(),
    ))]);
    let syntax_type = SyntaxType::Function(Rc::new(func_type));
    VarResult::new(value, syntax_type, name)
}

pub fn declare_sma_var<'a>() -> VarResult<'a> {
    declare_ma_var("sma", sma_func)
}

pub fn declare_wma_var<'a>() -> VarResult<'a> {
    declare_ma_var("wma", wma_func)
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
            vec![declare_sma_var(), declare_wma_var()],
            vec![("close", SyntaxType::float_series())],
        );
        let src = "m = sma(close, 2)\nm2 = wma(close, 2)";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![(
                    "close",
                    AnySeries::from_float_vec(vec![Some(6f64), Some(12f64)]),
                )],
                None,
            )
            .unwrap();

        assert_eq!(
            runner.get_context().move_var(VarIndex::new(3, 0)),
            Some(PineRef::new(Series::from_vec(vec![None, Some(9f64)])))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(4, 0)),
            Some(PineRef::new(Series::from_vec(vec![None, Some(10f64)])))
        );
    }
}
// 4 * 12 + 2 * 6  4 + 2  5 * 12 / 6 =

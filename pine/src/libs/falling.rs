use super::ema::rma_func;
use super::VarResult;
use crate::ast::stat_expr_types::VarIndex;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::{
    float_abs, float_max, move_element, pine_ref_to_bool, pine_ref_to_f64, pine_ref_to_f64_series,
    pine_ref_to_i64, require_param, series_index,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::InputSrc;
use crate::types::{
    downcast_pf_ref, int2float, Arithmetic, Callable, CallableFactory, Float, Int, PineRef,
    RefData, RuntimeErr, Series, SeriesCall,
};
use std::mem;
use std::rc::Rc;

type CheckHandler<'a> = fn(RefData<Series<Float>>, i64) -> Result<PineRef<'a>, RuntimeErr>;
fn check_less<'a>(source: RefData<Series<Float>>, length: i64) -> Result<PineRef<'a>, RuntimeErr> {
    let cur_val = source.index_value(0).unwrap();
    for i in 1..=length as usize {
        if source.index_value(i).unwrap() < cur_val {
            return Ok(PineRef::new_rc(Series::from(false)));
        }
    }
    return Ok(PineRef::new_rc(Series::from(true)));
}

#[derive(Debug, Clone, PartialEq)]
struct AtrVal {
    check_func: *mut (),
}

impl AtrVal {
    pub fn new(check_func: *mut ()) -> AtrVal {
        AtrVal { check_func }
    }
}

impl<'a> SeriesCall<'a> for AtrVal {
    fn step(
        &mut self,
        _ctx: &mut dyn Ctx<'a>,
        mut param: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        move_tuplet!((source, length) = param);

        let source = require_param("source", pine_ref_to_f64_series(source))?;
        let length = require_param("length", pine_ref_to_i64(length))?;
        let check_func = unsafe { mem::transmute::<_, CheckHandler<'a>>(self.check_func) };
        check_func(source, length)
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        Box::new(self.clone())
    }
}
pub const VAR_NAME: &'static str = "falling";

pub fn declare_s_var<'a>(name: &'static str, check_func: CheckHandler<'a>) -> VarResult<'a> {
    let value = PineRef::new(Callable::new(
        None,
        Some(Box::new(AtrVal::new(check_func as *mut ()))),
    ));

    let func_type = FunctionTypes(vec![FunctionType::new((
        vec![
            ("source", SyntaxType::float_series()),
            ("length", SyntaxType::int()),
        ],
        SyntaxType::bool_series(),
    ))]);
    let syntax_type = SyntaxType::Function(Rc::new(func_type));
    VarResult::new(value, syntax_type, name)
}

pub fn declare_var<'a>() -> VarResult<'a> {
    declare_s_var(VAR_NAME, check_less)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::syntax_type::SyntaxType;
    use crate::runtime::VarOperate;
    use crate::runtime::{AnySeries, NoneCallback};
    use crate::types::Series;
    use crate::{LibInfo, PineParser, PineRunner};

    #[test]
    fn accdist_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::float_series())],
        );
        let src = "m = falling(close, 2)";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![(
                    "close",
                    AnySeries::from_float_vec(vec![Some(10f64), Some(20f64), Some(5f64)]),
                )],
                None,
            )
            .unwrap();
        assert_eq!(
            runner.get_context().get_var(VarIndex::new(0, 0)),
            &Some(PineRef::new(Series::from_vec(vec![false, false, true])))
        );
    }
}

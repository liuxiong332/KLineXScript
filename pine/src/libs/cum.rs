use super::sma::declare_ma_var;
use super::VarResult;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::{
    move_element, pine_ref_to_bool, pine_ref_to_f64, pine_ref_to_f64_series, pine_ref_to_i64,
    require_param, series_index,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::types::{
    downcast_pf_ref, int2float, Arithmetic, Callable, CallableFactory, Evaluate, EvaluateVal,
    Float, Int, PineRef, RefData, RuntimeErr, Series, SeriesCall, NA,
};
use std::mem;
use std::rc::Rc;

fn sum_func<'a>(source: RefData<Series<Float>>, length: i64) -> Result<Float, RuntimeErr> {
    let mut sum_val = Some(0f64);
    for i in 0..length {
        let val = source.index_value(i as usize).unwrap();
        sum_val = sum_val.add(val);
    }
    Ok(sum_val)
}

#[derive(Debug, Clone, PartialEq)]
struct CumVal {
    prev_sum: Float,
}

impl CumVal {
    pub fn new() -> CumVal {
        CumVal {
            prev_sum: Some(0f64),
        }
    }
}

impl<'a> SeriesCall<'a> for CumVal {
    fn step(
        &mut self,
        _ctx: &mut dyn Ctx<'a>,
        mut param: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        let source = mem::replace(&mut param[0], None);

        let source = pine_ref_to_f64(source);
        self.prev_sum = self.prev_sum.add(source);
        Ok(PineRef::new(Series::from(self.prev_sum)))
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        Box::new(self.clone())
    }
}

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(CallableFactory::new(|| {
        Callable::new(None, Some(Box::new(CumVal::new())))
    }));

    let func_type = FunctionTypes(vec![FunctionType::new((
        vec![("x", SyntaxType::float_series())],
        SyntaxType::float_series(),
    ))]);
    let syntax_type = SyntaxType::Function(Rc::new(func_type));
    VarResult::new(value, syntax_type, "cum")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::stat_expr_types::VarIndex;
    use crate::ast::syntax_type::SyntaxType;
    use crate::runtime::VarOperate;
    use crate::runtime::{AnySeries, NoneCallback};
    use crate::types::Series;
    use crate::{LibInfo, PineParser, PineRunner};
    // use crate::libs::{floor, exp, };

    #[test]
    fn cum_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::float_series())],
        );
        let src = "m = cum(close)";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![(
                    "close",
                    AnySeries::from_float_vec(vec![Some(12f64), Some(6f64)]),
                )],
                None,
            )
            .unwrap();

        assert_eq!(
            runner.get_context().move_var(VarIndex::new(2, 0)),
            Some(PineRef::new(Series::from_vec(vec![
                Some(12f64),
                Some(18f64),
            ])))
        );
    }
}

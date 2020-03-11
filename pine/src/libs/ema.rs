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
    Float, Int, PineRef, RefData, RuntimeErr, Series, SeriesCall, NA,
};
use std::mem;
use std::rc::Rc;

fn ema_func<'a>(source: Float, length: i64, prev_val: Float) -> Result<Float, RuntimeErr> {
    let mut sum = 0f64;
    let alpha = 2f64 / (length + 1) as f64;
    match source {
        Some(val) => {
            sum = alpha * val + (1f64 - alpha) * prev_val.unwrap_or(0f64);
        }
        None => {
            return Ok(None);
        }
    }
    Ok(Some(sum))
}

#[derive(Debug, Clone, PartialEq)]
struct EmaVal {
    prev_val: Float,
}

impl EmaVal {
    pub fn new() -> EmaVal {
        EmaVal { prev_val: None }
    }
}

impl<'a> SeriesCall<'a> for EmaVal {
    fn step(
        &mut self,
        _ctx: &mut dyn Ctx<'a>,
        mut param: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        move_tuplet!((source, length) = param);

        let source = pine_ref_to_f64(source);
        let length = require_param("length", pine_ref_to_i64(length))?;
        let val = ema_func(source, length, mem::replace(&mut self.prev_val, None))?;
        self.prev_val = val;
        Ok(PineRef::new(Series::from(val)))
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        Box::new(self.clone())
    }
}

pub const VAR_NAME: &'static str = "ema";

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
        let src = "m = ema(close, 3)";
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

        assert_eq!(
            runner.get_context().move_var(VarIndex::new(2, 0)),
            Some(PineRef::new(Series::from_vec(vec![
                Some(5f64),
                Some(12.5f64)
            ])))
        );
    }
}

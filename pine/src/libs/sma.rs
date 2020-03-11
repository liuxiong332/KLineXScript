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
    RuntimeErr, Series, SeriesCall, NA,
};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
struct SmaVal;

impl<'a> SeriesCall<'a> for SmaVal {
    fn step(
        &mut self,
        _ctx: &mut dyn Ctx<'a>,
        mut param: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        move_tuplet!((source, length) = param);

        let source = require_param("source", pine_ref_to_f64_series(source))?;
        let length = require_param("length", pine_ref_to_i64(length))?;
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

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        Box::new(self.clone())
    }
}

pub const VAR_NAME: &'static str = "sma";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(Callable::new(None, Some(Box::new(SmaVal))));

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
        let src = "m = sma(close, 2)";
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
            Some(PineRef::new(Series::from_vec(vec![None, Some(15f64)])))
        );
    }
}

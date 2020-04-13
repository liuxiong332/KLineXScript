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

#[derive(Debug, PartialEq, Clone)]
enum NNVal {
    Int(i64),
    Float(f64),
    Color(String),
}

impl NNVal {
    fn get_int(&self) -> i64 {
        match self {
            NNVal::Int(v) => v.clone(),
            _ => unreachable!(),
        }
    }

    fn get_float(&self) -> f64 {
        match self {
            NNVal::Float(v) => v.clone(),
            _ => unreachable!(),
        }
    }

    fn get_color(&self) -> String {
        match self {
            NNVal::Color(v) => v.clone(),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct AtrVal {
    prev_val: Option<NNVal>,
}

impl AtrVal {
    pub fn new() -> AtrVal {
        AtrVal { prev_val: None }
    }
}

impl<'a> SeriesCall<'a> for AtrVal {
    fn step(
        &mut self,
        ctx: &mut dyn Ctx<'a>,
        mut param: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        let series = mem::replace(&mut param[0], None);
        let prev_val = self.prev_val.as_ref();

        match _func_type.get_type(0) {
            Some(&SyntaxType::Series(SimpleSyntaxType::Int)) => {
                let val = match pine_ref_to_i64(series) {
                    Some(s) => s,
                    None => prev_val.unwrap_or(&NNVal::Int(0i64)).get_int(),
                };
                self.prev_val = Some(NNVal::Int(val));
                Ok(PineRef::new_rc(Series::from(Some(val))))
            }
            Some(&SyntaxType::Series(SimpleSyntaxType::Float)) => {
                let val = match pine_ref_to_f64(series) {
                    Some(s) => s,
                    None => prev_val.unwrap_or(&NNVal::Float(0f64)).get_float(),
                };
                self.prev_val = Some(NNVal::Float(val));
                Ok(PineRef::new_rc(Series::from(Some(val))))
            }
            _ => unreachable!(),
        }
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        Box::new(self.clone())
    }
}

pub const VAR_NAME: &'static str = "fixnan";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(CallableFactory::new(|| {
        Callable::new(None, Some(Box::new(AtrVal::new())))
    }));

    // plot(series, title, color, linewidth, style, trackprice, transp, histbase, offset, join, editable, show_last) → plot

    let func_type = FunctionTypes(vec![
        FunctionType::new((
            vec![("x", SyntaxType::float_series())],
            SyntaxType::float_series(),
        )),
        FunctionType::new((
            vec![("x", SyntaxType::int_series())],
            SyntaxType::int_series(),
        )),
        // FunctionType::new((
        //     vec![("x", SyntaxType::bool_series())],
        //     SyntaxType::bool_series(),
        // )),
        // FunctionType::new((
        //     vec![("x", SyntaxType::Series(SimpleSyntaxType::Color))],
        //     SyntaxType::Series(SimpleSyntaxType::Color),
        // )),
    ]);
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

    #[test]
    fn accdist_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::float_series())],
        );
        let src = "m = fixnan(close)";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![(
                    "close",
                    AnySeries::from_float_vec(vec![None, Some(10f64), None]),
                )],
                None,
            )
            .unwrap();
        assert_eq!(
            runner.get_context().get_var(VarIndex::new(0, 0)),
            &Some(PineRef::new(Series::from_vec(vec![
                Some(0f64),
                Some(10f64),
                Some(10f64)
            ])))
        );
    }
}

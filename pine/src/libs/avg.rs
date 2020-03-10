use super::VarResult;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::{
    move_element, pine_ref_to_bool, pine_ref_to_color, pine_ref_to_f64, pine_ref_to_i64,
    pine_ref_to_string,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::types::{Callable, Float, Int, PineFrom, PineRef, RuntimeErr, Series, SeriesCall, NA};
use std::cmp;
use std::mem::transmute;
use std::rc::Rc;

fn float_avg<'a>(vals: Vec<Option<PineRef<'a>>>) -> Float {
    let iter = vals.into_iter().filter_map(|v| pine_ref_to_f64(v));
    Some(iter.clone().sum::<f64>() / iter.count() as f64)
}

#[derive(Debug, Clone, PartialEq)]
struct AvgCallVal {}

impl AvgCallVal {
    pub fn new() -> AvgCallVal {
        AvgCallVal {}
    }
}

impl<'a> SeriesCall<'a> for AvgCallVal {
    fn step(
        &mut self,
        _context: &mut dyn Ctx<'a>,
        mut param: Vec<Option<PineRef<'a>>>,
        func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        move_tuplet!((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = param);
        let input_vals = vec![x1, x2, x3, x4, x5, x6, x7, x8, x9, x10];

        match func_type.get_type(0).unwrap() {
            SyntaxType::Simple(SimpleSyntaxType::Float) => {
                let res = float_avg(input_vals);
                Ok(PineRef::new_box(res))
            }
            SyntaxType::Series(SimpleSyntaxType::Float) => {
                let res = float_avg(input_vals);
                Ok(PineRef::new_rc(Series::from(res)))
            }
            _ => unreachable!(),
        }
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        Box::new(self.clone())
    }
}

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(Callable::new(None, Some(Box::new(AvgCallVal::new()))));

    let func_type = FunctionTypes(vec![
        FunctionType::new((
            vec![
                ("x1", SyntaxType::float()),
                ("x2", SyntaxType::float()),
                ("x3", SyntaxType::float()),
                ("x4", SyntaxType::float()),
                ("x5", SyntaxType::float()),
                ("x6", SyntaxType::float()),
                ("x7", SyntaxType::float()),
                ("x8", SyntaxType::float()),
                ("x9", SyntaxType::float()),
                ("x10", SyntaxType::float()),
            ],
            SyntaxType::float(),
        )),
        FunctionType::new((
            vec![
                ("x1", SyntaxType::float_series()),
                ("x2", SyntaxType::float_series()),
                ("x3", SyntaxType::float_series()),
                ("x4", SyntaxType::float_series()),
                ("x5", SyntaxType::float_series()),
                ("x6", SyntaxType::float_series()),
                ("x7", SyntaxType::float_series()),
                ("x8", SyntaxType::float_series()),
                ("x9", SyntaxType::float_series()),
                ("x10", SyntaxType::float_series()),
            ],
            SyntaxType::float_series(),
        )),
    ]);
    let syntax_type = SyntaxType::Function(Rc::new(func_type));
    VarResult::new(value, syntax_type, "avg")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::stat_expr_types::VarIndex;
    use crate::ast::syntax_type::SimpleSyntaxType;
    use crate::runtime::{AnySeries, NoneCallback, VarOperate};
    use crate::{LibInfo, PineParser, PineRunner};

    #[test]
    fn na_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = "
        m2 = avg(close, close + 1, close + 2)
        ";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![("close", AnySeries::from_float_vec(vec![Some(2f64)]))],
                None,
            )
            .unwrap();
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(2, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(3f64)])))
        );
    }
}

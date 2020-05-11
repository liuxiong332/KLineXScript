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

#[derive(Debug, Clone, PartialEq)]
struct MinMaxCallVal {
    int_func: *mut (),
    float_func: *mut (),
}

impl MinMaxCallVal {
    pub fn new(int_func: *mut (), float_func: *mut ()) -> MinMaxCallVal {
        MinMaxCallVal {
            int_func,
            float_func,
        }
    }
}

impl<'a> SeriesCall<'a> for MinMaxCallVal {
    fn step(
        &mut self,
        _context: &mut dyn Ctx<'a>,
        mut param: Vec<Option<PineRef<'a>>>,
        func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        move_tuplet!((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = param);
        let input_vals = vec![x1, x2, x3, x4, x5, x6, x7, x8, x9, x10];

        let int_func =
            unsafe { transmute::<_, fn(Vec<Option<PineRef<'a>>>) -> Int>(self.int_func) };
        let float_func =
            unsafe { transmute::<_, fn(Vec<Option<PineRef<'a>>>) -> Float>(self.float_func) };

        match func_type.get_type(0).unwrap() {
            SyntaxType::Simple(SimpleSyntaxType::Int) => {
                let res = int_func(input_vals);
                Ok(PineRef::new_box(res))
            }
            SyntaxType::Series(SimpleSyntaxType::Int) => {
                let res = int_func(input_vals);
                Ok(PineRef::new_rc(Series::from(res)))
            }
            SyntaxType::Simple(SimpleSyntaxType::Float) => {
                let res = float_func(input_vals);
                Ok(PineRef::new_box(res))
            }
            SyntaxType::Series(SimpleSyntaxType::Float) => {
                let res = float_func(input_vals);
                Ok(PineRef::new_rc(Series::from(res)))
            }
            _ => unreachable!(),
        }
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        Box::new(self.clone())
    }
}

pub fn declare_minmax_var<'a>(
    name: &'static str,
    int_func: fn(Vec<Option<PineRef<'a>>>) -> Int,
    float_func: fn(Vec<Option<PineRef<'a>>>) -> Float,
) -> VarResult<'a> {
    let value = PineRef::new(Callable::new(
        None,
        Some(Box::new(MinMaxCallVal::new(
            int_func as *mut (),
            float_func as *mut (),
        ))),
    ));

    let func_type = FunctionTypes(vec![
        FunctionType::new((
            vec![
                ("x1", SyntaxType::int()),
                ("x2", SyntaxType::int()),
                ("x3", SyntaxType::int()),
                ("x4", SyntaxType::int()),
                ("x5", SyntaxType::int()),
                ("x6", SyntaxType::int()),
                ("x7", SyntaxType::int()),
                ("x8", SyntaxType::int()),
                ("x9", SyntaxType::int()),
                ("x10", SyntaxType::int()),
            ],
            SyntaxType::int(),
        )),
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
                ("x1", SyntaxType::int_series()),
                ("x2", SyntaxType::int_series()),
                ("x3", SyntaxType::int_series()),
                ("x4", SyntaxType::int_series()),
                ("x5", SyntaxType::int_series()),
                ("x6", SyntaxType::int_series()),
                ("x7", SyntaxType::int_series()),
                ("x8", SyntaxType::int_series()),
                ("x9", SyntaxType::int_series()),
                ("x10", SyntaxType::int_series()),
            ],
            SyntaxType::int_series(),
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
    VarResult::new(value, syntax_type, name)
}

fn int_max<'a>(vals: Vec<Option<PineRef<'a>>>) -> Int {
    vals.into_iter().filter_map(|v| pine_ref_to_i64(v)).max()
}

fn float_max<'a>(vals: Vec<Option<PineRef<'a>>>) -> Float {
    vals.into_iter()
        .filter_map(|v| pine_ref_to_f64(v))
        .max_by(|x1, x2| x1.partial_cmp(x2).unwrap())
}

pub fn declare_max_var<'a>() -> VarResult<'a> {
    declare_minmax_var("max", int_max, float_max)
}

fn int_min<'a>(vals: Vec<Option<PineRef<'a>>>) -> Int {
    vals.into_iter().filter_map(|v| pine_ref_to_i64(v)).min()
}

fn float_min<'a>(vals: Vec<Option<PineRef<'a>>>) -> Float {
    vals.into_iter()
        .filter_map(|v| pine_ref_to_f64(v))
        .min_by(|x1, x2| x1.partial_cmp(x2).unwrap())
}

pub fn declare_min_var<'a>() -> VarResult<'a> {
    declare_minmax_var("min", int_min, float_min)
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
            vec![declare_max_var(), declare_min_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = "
        m1 = max(1, 2, 3, 4, 5)
        m2 = max(close, close + 1, close + 2)
        m3 = min(1, 2, 3, 4, 5)
        m4 = min(close, close + 1, close + 2)
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
            runner.get_context().move_var(VarIndex::new(0, 0)),
            Some(PineRef::new_box(Some(5i64)))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(1, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(4f64)])))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(2, 0)),
            Some(PineRef::new_box(Some(1i64)))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(3, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(2f64)])))
        );
    }
}

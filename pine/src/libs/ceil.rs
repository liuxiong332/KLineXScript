use super::VarResult;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::{pine_ref_to_f64, pine_ref_to_i64};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::types::{Callable, Float, Int, PineFrom, PineRef, RuntimeErr, Series, SeriesCall, NA};
use std::mem;
use std::mem::transmute;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
struct MathCallVal {
    func: *mut (),
}

impl MathCallVal {
    pub fn new(func: *mut ()) -> MathCallVal {
        MathCallVal { func }
    }
}

impl<'a> SeriesCall<'a> for MathCallVal {
    fn step(
        &mut self,
        _ctx: &mut dyn Ctx<'a>,
        mut param: Vec<Option<PineRef<'a>>>,
        func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        let xval = mem::replace(&mut param[0], None);

        let handler = unsafe { transmute::<_, fn(Option<PineRef<'a>>) -> Int>(self.func) };
        match ((func_type.signature.0)[0]).1 {
            SyntaxType::Simple(SimpleSyntaxType::Float) => {
                let res = handler(xval);
                Ok(PineRef::new_box(res))
            }
            SyntaxType::Series(SimpleSyntaxType::Float) => {
                let res = handler(xval);
                Ok(PineRef::new_rc(Series::from(res)))
            }
            _ => unreachable!(),
        }
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        Box::new(self.clone())
    }
}

pub fn declare_math_var<'a>(
    varname: &'static str,
    func: fn(Option<PineRef<'a>>) -> Int,
) -> VarResult<'a> {
    let value = PineRef::new(Callable::new(
        None,
        Some(Box::new(MathCallVal::new(func as *mut ()))),
    ));

    let func_type = FunctionTypes(vec![
        FunctionType::new((vec![("x", SyntaxType::float())], SyntaxType::int())),
        FunctionType::new((
            vec![("x", SyntaxType::float_series())],
            SyntaxType::int_series(),
        )),
    ]);
    let syntax_type = SyntaxType::Function(Rc::new(func_type));
    VarResult::new(value, syntax_type, varname)
}

fn float_ceil<'a>(xval: Option<PineRef<'a>>) -> Int {
    match pine_ref_to_f64(xval) {
        None => None,
        Some(v) => Some(v.ceil() as i64),
    }
}

pub fn declare_ceil_var<'a>() -> VarResult<'a> {
    declare_math_var("ceil", float_ceil)
}

fn float_round<'a>(xval: Option<PineRef<'a>>) -> Int {
    match pine_ref_to_f64(xval) {
        None => None,
        Some(v) => Some(v.round() as i64),
    }
}

pub fn declare_round_var<'a>() -> VarResult<'a> {
    declare_math_var("round", float_round)
}

fn float_floor<'a>(xval: Option<PineRef<'a>>) -> Int {
    match pine_ref_to_f64(xval) {
        None => None,
        Some(v) => Some(v.floor() as i64),
    }
}

pub fn declare_floor_var<'a>() -> VarResult<'a> {
    declare_math_var("floor", float_floor)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::stat_expr_types::VarIndex;
    use crate::ast::syntax_type::SimpleSyntaxType;
    use crate::runtime::{AnySeries, NoneCallback, VarOperate};
    use crate::{LibInfo, PineParser, PineRunner};

    #[test]
    fn ceil_test() {
        let lib_info = LibInfo::new(
            vec![declare_ceil_var(), declare_floor_var(), declare_round_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = "
        m1 = ceil(12.3)\nm2 = round(12.5)\nm3 = floor(12.5)
        ";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![("close", AnySeries::from_float_vec(vec![Some(-2f64)]))],
                None,
            )
            .unwrap();

        let starti = 0;
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(starti, 0)),
            Some(PineRef::new(Some(13i64)))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(starti + 1, 0)),
            Some(PineRef::new(Some(13i64)))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(starti + 2, 0)),
            Some(PineRef::new(Some(12i64)))
        );
    }
}

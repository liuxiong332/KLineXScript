use super::VarResult;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::{move_element, pine_ref_to_f64, pine_ref_to_i64};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::types::{Callable, Float, Int, PineFrom, PineRef, RuntimeErr, Series, SeriesCall, NA};
use std::rc::Rc;

fn float_pow<'a>(base: Option<PineRef<'a>>, exponent: Option<PineRef<'a>>) -> Float {
    match (pine_ref_to_f64(base), pine_ref_to_f64(exponent)) {
        (Some(v), Some(e)) => Some(v.powf(e)),
        _ => None,
    }
}

#[derive(Debug, Clone, PartialEq)]
struct MathCallVal;

impl<'a> SeriesCall<'a> for MathCallVal {
    fn step(
        &mut self,
        _ctx: &mut dyn Ctx<'a>,
        mut param: Vec<Option<PineRef<'a>>>,
        func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        move_tuplet!((base, exponent) = param);
        println!("base exponent {:?} {:?}", base, exponent);
        match func_type.get_type(0).unwrap() {
            SyntaxType::Simple(_) => Ok(PineRef::new_box(float_pow(base, exponent))),
            SyntaxType::Series(_) => Ok(PineRef::new_rc(Series::from(float_pow(base, exponent)))),
            _ => unreachable!(),
        }
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        Box::new(self.clone())
    }
}

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(Callable::new(None, Some(Box::new(MathCallVal))));

    let func_type = FunctionTypes(vec![
        FunctionType::new((
            vec![
                ("base", SyntaxType::float()),
                ("exponent", SyntaxType::float()),
            ],
            SyntaxType::float(),
        )),
        FunctionType::new((
            vec![
                ("base", SyntaxType::float_series()),
                ("exponent", SyntaxType::float()),
            ],
            SyntaxType::float_series(),
        )),
    ]);
    let syntax_type = SyntaxType::Function(Rc::new(func_type));
    VarResult::new(value, syntax_type, "pow")
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
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = "
        m1 = pow(3, 2)
        ";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![("close", AnySeries::from_float_vec(vec![Some(-2f64)]))],
                None,
            )
            .unwrap();

        assert_eq!(
            runner.get_context().move_var(VarIndex::new(2, 0)),
            Some(PineRef::new(Some(9f64)))
        );
    }
}

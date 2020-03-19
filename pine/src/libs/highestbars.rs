use super::ema::rma_func;
use super::VarResult;
use crate::ast::stat_expr_types::VarIndex;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::{
    ensure_srcs, float_abs, float_max, move_element, pine_ref_to_bool, pine_ref_to_f64,
    pine_ref_to_f64_series, pine_ref_to_i64, require_param, series_index,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::InputSrc;
use crate::types::{
    downcast_pf_ref, int2float, Arithmetic, Callable, CallableCreator, CallableFactory, Float, Int,
    PineRef, RefData, RuntimeErr, Series, SeriesCall,
};
use std::mem;
use std::rc::Rc;

type GenIndexFunc<'a> = fn(&mut dyn Ctx<'a>) -> VarIndex;
type GetValFunc = fn(&Option<RefData<Series<Float>>>, i64) -> Int;

fn get_max_val<'a>(source: &Option<RefData<Series<Float>>>, length: i64) -> Int {
    let mut max_val = Some(0f64);
    let mut max_i = Some(0);
    for i in 0..length as usize {
        let cur_val = series_index(source, i);
        if cur_val > max_val {
            max_i = Some(i as i64);
            max_val = cur_val;
        }
    }
    max_i
}

#[derive(Debug, Clone, PartialEq)]
struct AtrVal {
    src_name: &'static str,
    run_func: *mut (),
    dest_index: VarIndex,
}

impl AtrVal {
    pub fn new(src_name: &'static str, run_func: *mut ()) -> AtrVal {
        AtrVal {
            src_name,
            run_func,
            dest_index: VarIndex::new(0, 0),
        }
    }
}

impl<'a> SeriesCall<'a> for AtrVal {
    fn step(
        &mut self,
        ctx: &mut dyn Ctx<'a>,
        mut param: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        let source;
        let length;

        let runner = unsafe { mem::transmute::<_, GetValFunc>(self.run_func) };

        if _func_type.signature.0.len() == 1 {
            ensure_srcs(ctx, vec![self.src_name], |indexs| {
                self.dest_index = indexs[0];
            });

            source = pine_ref_to_f64_series(ctx.get_var(self.dest_index).clone());
            length = require_param("length", pine_ref_to_i64(mem::replace(&mut param[0], None)))?;
        } else {
            source = pine_ref_to_f64_series(mem::replace(&mut param[0], None));
            length = require_param("length", pine_ref_to_i64(mem::replace(&mut param[1], None)))?;
        }
        let max_val = runner(&source, length);
        Ok(PineRef::new_rc(Series::from(max_val)))
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        Box::new(self.clone())
    }
}

#[derive(Debug, Clone, PartialEq)]
struct SmaCreator {
    src_name: &'static str,
    handle: *mut (),
}

impl SmaCreator {
    pub fn new(src_name: &'static str, handle: *mut ()) -> SmaCreator {
        SmaCreator { src_name, handle }
    }
}

impl<'a> CallableCreator<'a> for SmaCreator {
    fn create(&self) -> Callable<'a> {
        Callable::new(
            None,
            Some(Box::new(AtrVal::new(self.src_name, self.handle))),
        )
    }

    fn copy(&self) -> Box<dyn CallableCreator<'a>> {
        Box::new(self.clone())
    }
}

pub fn declare_s_var<'a>(
    name: &'static str,
    src_name: &'static str,
    run_func: GetValFunc,
) -> VarResult<'a> {
    let value = PineRef::new(CallableFactory::new_with_creator(Box::new(
        SmaCreator::new(src_name, run_func as *mut ()),
    )));

    let func_type = FunctionTypes(vec![
        FunctionType::new((
            vec![("length", SyntaxType::int())],
            SyntaxType::int_series(),
        )),
        FunctionType::new((
            vec![
                ("source", SyntaxType::float_series()),
                ("length", SyntaxType::int()),
            ],
            SyntaxType::int_series(),
        )),
    ]);
    let syntax_type = SyntaxType::Function(Rc::new(func_type));
    VarResult::new(value, syntax_type, name)
}

pub fn declare_var<'a>() -> VarResult<'a> {
    declare_s_var("highestbars", "high", get_max_val)
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
            vec![
                ("close", SyntaxType::float_series()),
                ("high", SyntaxType::float_series()),
            ],
        );
        let src = "m1 = highestbars(2)\nm2 = highestbars(close, 2)";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![
                    (
                        "close",
                        AnySeries::from_float_vec(vec![Some(10f64), Some(20f64), Some(5f64)]),
                    ),
                    (
                        "high",
                        AnySeries::from_float_vec(vec![Some(19f64), Some(25f64), Some(10f64)]),
                    ),
                ],
                None,
            )
            .unwrap();
        assert_eq!(
            runner.get_context().get_var(VarIndex::new(3, 0)),
            &Some(PineRef::new(Series::from_vec(vec![
                Some(0i64),
                Some(0i64),
                Some(1i64)
            ])))
        );
    }
}

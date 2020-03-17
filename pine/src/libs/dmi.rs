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
    RefData, RuntimeErr, Series, SeriesCall, Tuple,
};
use std::mem;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
struct DmiVal {
    close_index: VarIndex,
    low_index: VarIndex,
    high_index: VarIndex,
    val_history: Vec<Float>,
    prev_val: Float,
}

impl DmiVal {
    pub fn new() -> DmiVal {
        DmiVal {
            close_index: VarIndex::new(0, 0),
            low_index: VarIndex::new(0, 0),
            high_index: VarIndex::new(0, 0),
            val_history: vec![],
            prev_val: Some(0f64),
        }
    }
}

impl<'a> SeriesCall<'a> for DmiVal {
    fn step(
        &mut self,
        ctx: &mut dyn Ctx<'a>,
        mut param: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        if !downcast_ctx(ctx).check_is_input_info_ready() {
            downcast_ctx(ctx).add_input_src(InputSrc::new(
                None,
                vec![
                    String::from("close"),
                    String::from("low"),
                    String::from("high"),
                ],
            ));

            self.close_index = VarIndex::new(*ctx.get_varname_index("close").unwrap(), 0);
            self.low_index = VarIndex::new(*ctx.get_varname_index("low").unwrap(), 0);
            self.high_index = VarIndex::new(*ctx.get_varname_index("high").unwrap(), 0);
        }

        let di_len = require_param(
            "diLength",
            pine_ref_to_i64(mem::replace(&mut param[0], None)),
        )?;
        let adx_len = require_param(
            "adxSmoothing",
            pine_ref_to_i64(mem::replace(&mut param[1], None)),
        )?;

        let close = pine_ref_to_f64_series(ctx.get_var(self.close_index).clone());
        let low = pine_ref_to_f64_series(ctx.get_var(self.low_index).clone());
        let high = pine_ref_to_f64_series(ctx.get_var(self.high_index).clone());

        let mut tr = Some(0f64);
        let mut dm_ps = Some(0f64);
        let mut dm_ms = Some(0f64);
        for i in 0..di_len as usize {
            let tr_val = float_max(
                float_abs(series_index(&high, i).minus(series_index(&low, i))),
                float_abs(series_index(&high, i).minus(series_index(&close, i + 1))),
                float_abs(series_index(&low, i).minus(series_index(&close, i + 1))),
            );

            let dm_plus1 = series_index(&high, i).minus(series_index(&high, i + 1));
            let dm_minus1 = series_index(&low, i).minus(series_index(&low, i + 1));
            let dm_plus = if dm_plus1 < dm_minus1 {
                Some(0f64)
            } else {
                dm_plus1
            };
            let dm_minus = if dm_minus1 < dm_plus1 {
                Some(0f64)
            } else {
                dm_minus1
            };
            tr = tr.add(tr_val);
            dm_ps = dm_ps.add(dm_plus);
            dm_ms = dm_ms.add(dm_minus);
        }

        println!("tr val {:?} {:?} {:?}", tr, dm_ps, dm_ms);

        let di_plus = dm_ps.div(tr).mul(Some(100f64));
        let di_minus = dm_ms.div(tr).mul(Some(100f64));

        let dx = (di_plus.minus(di_minus))
            .div(di_plus.add(di_minus))
            .mul(Some(100f64));
        self.val_history.push(dx);

        let mut adx = Some(0f64);
        for i in 0..adx_len as usize {
            if i < self.val_history.len() {
                adx = adx.add(self.val_history[i]);
            }
        }
        Ok(PineRef::new(Tuple(vec![
            PineRef::new_rc(Series::from(di_plus)),
            PineRef::new_rc(Series::from(di_minus)),
            PineRef::new_rc(Series::from(adx)),
        ])))
    }

    fn back(&mut self, _context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        self.val_history.pop();
        Ok(())
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        Box::new(self.clone())
    }
}
pub const VAR_NAME: &'static str = "dmi";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(CallableFactory::new(|| {
        Callable::new(None, Some(Box::new(DmiVal::new())))
    }));

    // plot(series, title, color, linewidth, style, trackprice, transp, histbase, offset, join, editable, show_last) → plot

    let func_type = FunctionTypes(vec![FunctionType::new((
        vec![
            ("diLength", SyntaxType::int()),
            ("adxSmoothing", SyntaxType::int()),
        ],
        SyntaxType::Tuple(Rc::new(vec![
            SyntaxType::float_series(),
            SyntaxType::float_series(),
            SyntaxType::float_series(),
        ])),
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

    #[test]
    fn accdist_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![
                ("close", SyntaxType::float_series()),
                ("high", SyntaxType::float_series()),
                ("low", SyntaxType::float_series()),
            ],
        );
        let src = "[m1, m2, m3] = dmi(2, 2)";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![
                    (
                        "close",
                        AnySeries::from_float_vec(vec![Some(10f64), Some(20f64), Some(10f64)]),
                    ),
                    (
                        "high",
                        AnySeries::from_float_vec(vec![Some(15f64), Some(22f64), Some(15f64)]),
                    ),
                    (
                        "low",
                        AnySeries::from_float_vec(vec![Some(1f64), Some(2f64), Some(1f64)]),
                    ),
                ],
                None,
            )
            .unwrap();
        // assert_eq!(
        //     runner.get_context().get_var(VarIndex::new(4, 0)),
        //     &Some(PineRef::new(Series::from_vec(vec![
        //         Some(0f64),
        //         Some(0f64),
        //         Some(0f64)
        //     ])))
        // );
    }
}

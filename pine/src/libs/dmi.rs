use super::ema::rma_func;
use super::VarResult;
use crate::ast::stat_expr_types::VarIndex;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::{
    ensure_srcs, float_abs, float_max, move_element, pine_ref_to_bool, pine_ref_to_f64,
    pine_ref_to_f64_series, pine_ref_to_i64, require_param, series_index, series_index2,
};
use crate::libs::change::series_change;
use crate::libs::ema::series_rma;
use crate::libs::tr::series_tr;
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::InputSrc;
use crate::types::{
    downcast_pf_ref, int2float, Arithmetic, Callable, CallableFactory, Comparator, Float, Int,
    Negative, PineRef, RefData, RuntimeErr, Series, SeriesCall, Tuple,
};
use std::mem;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
struct DirmovProps<'a> {
    trs: Series<'a, Float>,
    dm1s: Series<'a, Float>,
    dm2s: Series<'a, Float>,
}

impl<'a> DirmovProps<'a> {
    fn new() -> DirmovProps<'a> {
        DirmovProps {
            trs: Series::new(),
            dm1s: Series::new(),
            dm2s: Series::new(),
        }
    }

    fn commit(&mut self) {
        self.trs.commit();
        self.dm1s.commit();
        self.dm2s.commit();
    }
}

// dirmov(len) =>
// 	up = change(high)
// 	down = -change(low)
// 	truerange = rma(tr, len)
// 	plus = fixnan(100 * rma(up > down and up > 0 ? up : 0, len) / truerange)
// 	minus = fixnan(100 * rma(down > up and down > 0 ? down : 0, len) / truerange)
//     [plus, minus]
fn dirmov(
    high: &Series<Float>,
    low: &Series<Float>,
    close: &Series<Float>,
    props: &mut DirmovProps,
    len: i64,
) -> Result<(Float, Float), RuntimeErr> {
    let up = series_change(high, 1);
    let down = series_change(low, 1).negative();

    let trv = series_tr(high.at(0), low.at(0), close);
    let truerange = series_rma(trv, len, &mut props.trs)?;

    let dm_plus = if up.gt(down) && up.gt(Some(0f64)) {
        up
    } else {
        Some(0f64)
    };
    let plus = Some(100f64)
        .mul(series_rma(dm_plus, len, &mut props.dm1s)?)
        .div(truerange);

    let dm_minus = if down.gt(up) && down.gt(Some(0f64)) {
        down
    } else {
        Some(0f64)
    };
    let minus = Some(100f64)
        .mul(series_rma(dm_minus, len, &mut props.dm2s)?)
        .div(truerange);

    return Ok((plus, minus));
}

#[derive(Debug, Clone, PartialEq)]
struct DmiVal<'a> {
    close_index: VarIndex,
    low_index: VarIndex,
    high_index: VarIndex,

    dirmov_props: DirmovProps<'a>,
    adxs: Series<'a, Float>,
}

impl<'a> DmiVal<'a> {
    pub fn new() -> DmiVal<'a> {
        DmiVal {
            close_index: VarIndex::new(0, 0),
            low_index: VarIndex::new(0, 0),
            high_index: VarIndex::new(0, 0),
            dirmov_props: DirmovProps::new(),
            adxs: Series::new(),
        }
    }
}

impl<'a> SeriesCall<'a> for DmiVal<'a> {
    fn step(
        &mut self,
        ctx: &mut dyn Ctx<'a>,
        mut param: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        ensure_srcs(ctx, vec!["close", "low", "high"], |indexs| {
            self.close_index = indexs[0];
            self.low_index = indexs[1];
            self.high_index = indexs[2];
        });

        let di_len = require_param(
            "diLength",
            pine_ref_to_i64(mem::replace(&mut param[0], None)),
        )?;
        let adx_len = require_param(
            "adxSmoothing",
            pine_ref_to_i64(mem::replace(&mut param[1], None)),
        )?;

        let close = require_param(
            "close",
            pine_ref_to_f64_series(ctx.get_var(self.close_index).clone()),
        )?;
        let low = require_param(
            "low",
            pine_ref_to_f64_series(ctx.get_var(self.low_index).clone()),
        )?;
        let high = require_param(
            "high",
            pine_ref_to_f64_series(ctx.get_var(self.high_index).clone()),
        )?;

        // myadx(dilen, adxlen) =>
        // [plus, minus] = dirmov(dilen)
        // sum = plus + minus
        // adx = 100 * rma(abs(plus - minus) / (sum == 0 ? 1 : sum), adxlen)
        // [adx, plus, minus]

        let (plus, minus) = dirmov(&*high, &*low, &*close, &mut self.dirmov_props, di_len)?;
        let sum = plus.add(minus);
        let aval =
            float_abs(plus.minus(minus)).div(if sum == Some(0f64) { Some(1f64) } else { sum });
        let adx = series_rma(aval, adx_len, &mut self.adxs)?.mul(Some(100f64));

        self.dirmov_props.commit();
        self.adxs.commit();

        Ok(PineRef::new(Tuple(vec![
            PineRef::new_rc(Series::from(plus)),
            PineRef::new_rc(Series::from(minus)),
            PineRef::new_rc(Series::from(adx)),
        ])))
    }

    fn back(&mut self, _context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
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

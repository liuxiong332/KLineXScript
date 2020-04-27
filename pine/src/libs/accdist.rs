use super::VarResult;
use crate::ast::stat_expr_types::VarIndex;
use crate::ast::syntax_type::SyntaxType;
use crate::helper::{ensure_srcs, float_add};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::InputSrc;
use crate::types::{
    downcast_pf_ref, int2float, Arithmetic, Evaluate, EvaluateFactory, EvaluateVal, Float, Int,
    PineRef, RuntimeErr, Series,
};

#[derive(Debug, Clone, PartialEq)]
struct AccDistVal {
    close_index: VarIndex,
    low_index: VarIndex,
    high_index: VarIndex,
    volume_index: VarIndex,
    ad_history: Vec<Float>,
    prev_cmfv: Float,
    is_init: bool,
}

impl AccDistVal {
    pub fn new() -> AccDistVal {
        println!("init accdist");
        AccDistVal {
            close_index: VarIndex::new(0, 0),
            low_index: VarIndex::new(0, 0),
            high_index: VarIndex::new(0, 0),
            volume_index: VarIndex::new(0, 0),
            ad_history: vec![],
            prev_cmfv: Some(0f64),
            is_init: false,
        }
    }
}

// ref to https://www.investopedia.com/terms/a/accumulationdistribution.asp
// CMFV=Current money flow volume = ((Pc - Pl) - (Ph - Pc)) / (Ph - Pl) * V
impl<'a> EvaluateVal<'a> for AccDistVal {
    fn custom_name(&self) -> &str {
        "accdist"
    }

    fn call(&mut self, ctx: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, RuntimeErr> {
        ensure_srcs(ctx, vec!["close", "low", "high", "volume"], |indexs| {
            self.close_index = indexs[0];
            self.low_index = indexs[1];
            self.high_index = indexs[2];
            self.volume_index = indexs[3];
        });
        match (
            ctx.get_var(self.close_index),
            ctx.get_var(self.low_index),
            ctx.get_var(self.high_index),
            ctx.get_var(self.volume_index),
        ) {
            (Some(close_val), Some(low_val), Some(high_val), Some(volume_val)) => {
                let close = downcast_pf_ref::<Series<Float>>(close_val)
                    .unwrap()
                    .get_current();
                let low = downcast_pf_ref::<Series<Float>>(low_val)
                    .unwrap()
                    .get_current();
                let high = downcast_pf_ref::<Series<Float>>(high_val)
                    .unwrap()
                    .get_current();
                let volume = downcast_pf_ref::<Series<Int>>(volume_val)
                    .unwrap()
                    .get_current();

                let val1 = (close.minus(low).minus(high.minus(close))).div(high.minus(low));
                let cmfv = float_add(
                    val1.mul(int2float(volume)),
                    Some(self.prev_cmfv.unwrap_or(0f64)),
                );
                self.ad_history.push(cmfv);
                self.prev_cmfv = cmfv;
                Ok(PineRef::new_rc(Series::from(cmfv)))
            }
            _ => Ok(PineRef::new_rc(Series::from(Float::from(None)))),
        }
    }

    fn back(&mut self, _ctx: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        self.ad_history.pop();
        self.prev_cmfv = *self.ad_history.last().unwrap();
        Ok(())
    }

    // fn run(&mut self, _ctx: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
    //     Ok(())
    // }

    fn copy(&self) -> Box<dyn EvaluateVal<'a>> {
        Box::new(self.clone())
    }
}
pub const VAR_NAME: &'static str = "accdist";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(EvaluateFactory::new(|| {
        Evaluate::new(Box::new(AccDistVal::new()))
    }));

    let syntax_type = SyntaxType::Val(Box::new(SyntaxType::float_series()));
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
                ("volume", SyntaxType::int_series()),
            ],
        );
        let src = "m = accdist";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![
                    (
                        "close",
                        AnySeries::from_float_vec(vec![Some(10f64), Some(20f64)]),
                    ),
                    (
                        "high",
                        AnySeries::from_float_vec(vec![Some(15f64), Some(22f64)]),
                    ),
                    (
                        "low",
                        AnySeries::from_float_vec(vec![Some(1f64), Some(2f64)]),
                    ),
                    (
                        "volume",
                        AnySeries::from_int_vec(vec![Some(100i64), Some(200i64)]),
                    ),
                ],
                None,
            )
            .unwrap();

        let val1 = ((10f64 - 1f64) - (15f64 - 10f64)) / (15f64 - 1f64) * 100f64;
        let val2 = ((20f64 - 2f64) - (22f64 - 20f64)) / (22f64 - 2f64) * 200f64 + val1;
        assert_eq!(
            runner.get_context().get_var(VarIndex::new(0, 0)),
            &Some(PineRef::new(Series::from_vec(vec![Some(val1), Some(val2)])))
        );

        runner
            .update(&vec![
                ("close", AnySeries::from_float_vec(vec![Some(20f64)])),
                ("high", AnySeries::from_float_vec(vec![Some(22f64)])),
                ("low", AnySeries::from_float_vec(vec![Some(2f64)])),
                ("volume", AnySeries::from_int_vec(vec![Some(200i64)])),
            ])
            .unwrap();
        assert_eq!(
            runner.get_context().get_var(VarIndex::new(0, 0)),
            &Some(PineRef::new(Series::from_vec(vec![Some(val1), Some(val2)])))
        );
    }
}

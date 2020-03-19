use super::ema::rma_func;
use super::sma::sma_func;
use super::VarResult;
use crate::ast::stat_expr_types::VarIndex;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::{
    ensure_srcs, move_element, pine_ref_to_bool, pine_ref_to_f64, pine_ref_to_f64_series,
    pine_ref_to_i64, require_param, series_index,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::InputSrc;
use crate::types::{
    downcast_pf_ref, int2float, Arithmetic, Callable, CallableFactory, Float, Int, PineRef,
    RefData, RuntimeErr, Series, SeriesCall,
};
use std::cmp;
use std::mem;
use std::rc::Rc;

fn float_max(f1: Float, f2: Float, f3: Float) -> Float {
    vec![f1.unwrap_or(0f64), f2.unwrap_or(0f64), f3.unwrap_or(0f64)]
        .into_iter()
        .max_by(|a, b| a.partial_cmp(b).unwrap())
}

fn float_abs(val: Float) -> Float {
    match val {
        Some(f1) => Some(f1.abs()),
        None => None,
    }
}

fn true_range(
    close: &Option<RefData<Series<Float>>>,
    high: &Option<RefData<Series<Float>>>,
    low: &Option<RefData<Series<Float>>>,
) -> Float {
    //  max(high - low, abs(high - close[1]), abs(low - close[1]))
    let v1 = series_index(high, 0).minus(series_index(low, 0));
    let v2 = float_abs(series_index(high, 0).minus(series_index(close, 1)));
    let v3 = float_abs(series_index(low, 0).minus(series_index(close, 1)));
    float_max(v1, v2, v3)
}

#[derive(Debug, Clone, PartialEq)]
struct CciVal {
    // close_index: VarIndex,
    low_index: VarIndex,
    high_index: VarIndex,
    ma_history: Vec<Float>,
    tp_history: Vec<Float>,
}

impl CciVal {
    pub fn new() -> CciVal {
        CciVal {
            low_index: VarIndex::new(0, 0),
            high_index: VarIndex::new(0, 0),
            ma_history: vec![],
            tp_history: vec![],
        }
    }
}

impl<'a> SeriesCall<'a> for CciVal {
    fn step(
        &mut self,
        ctx: &mut dyn Ctx<'a>,
        mut param: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        ensure_srcs(ctx, vec!["low", "high"], |indexs| {
            self.low_index = indexs[0];
            self.high_index = indexs[1];
        });

        let series = pine_ref_to_f64_series(mem::replace(&mut param[0], None));

        let length = require_param("length", pine_ref_to_i64(mem::replace(&mut param[1], None)))?;

        let low = pine_ref_to_f64(ctx.get_var(self.low_index).clone());
        let high = pine_ref_to_f64(ctx.get_var(self.high_index).clone());

        // TP t ＝ ( 最高價t ＋ 最低價t ＋ 收盤價t  ) ／３
        let tp = series_index(&series, 0).add(high).add(low).div(Some(3f64));
        println!("tp val {:?}", tp);

        let start_i = cmp::max(0, self.ma_history.len() as i64 - length + 1) as usize;

        // MA t ＝(  TPt  +  TPt-1  + ．．． +  TP t-n+1 ) ／ ｎ
        let ma_his = self.tp_history[start_i..]
            .iter()
            .fold(Some(0f64), |sum, v| sum.add(v.clone()));
        let ma = tp.add(ma_his).div(Some(length as f64));
        println!("ma val {:?}", ma);

        // MD t ＝(｜MAt－TPt｜＋｜MAt-1－TPt-1 ｜＋．．．．＋｜MAt-n+1－TPt-n+1｜)／ｎ
        let md_his = self.ma_history[start_i..]
            .iter()
            .zip(self.tp_history[start_i..].iter())
            .fold(Some(0f64), |sum, (v1, v2)| {
                sum.add(float_abs(v1.minus(v2.clone())))
            });
        let md = float_abs(tp.minus(ma)).add(md_his).div(Some(length as f64));
        println!("md val {:?}", md);

        self.tp_history.push(tp);
        self.ma_history.push(ma);

        // CCI t ＝ ( TP t－MA t ) ／ (  0.015 * MD t )
        let cci = tp.minus(ma).div(md.mul(Some(0.015f64)));
        // let result = rma_func(true_range(&close, &high, &low), length, self.prev_val)?;
        // self.prev_val = result;
        // self.val_history.push(result);

        Ok(PineRef::new(Series::from(cci)))
    }

    fn back(&mut self, _context: &mut dyn Ctx<'a>) -> Result<(), RuntimeErr> {
        self.ma_history.pop();
        self.tp_history.pop();
        Ok(())
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        Box::new(self.clone())
    }
}
pub const VAR_NAME: &'static str = "cci";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(CallableFactory::new(|| {
        Callable::new(None, Some(Box::new(CciVal::new())))
    }));

    // plot(series, title, color, linewidth, style, trackprice, transp, histbase, offset, join, editable, show_last) → plot

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
    use std::f64;

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
        let src = "m = cci(close, 2)";
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
                        AnySeries::from_float_vec(vec![Some(2f64), Some(0f64)]),
                    ),
                ],
                None,
            )
            .unwrap();
        // tp 9  ma 4.5 md 2.25  14 11.5  3.5
        assert_eq!(
            runner.get_context().get_var(VarIndex::new(4, 0)),
            &Some(PineRef::new(Series::from_vec(vec![
                Some(4.5f64 / (2.25f64 * 0.015f64)),
                Some(2.5f64 / (3.5f64 * 0.015f64))
            ])))
        );
    }
}

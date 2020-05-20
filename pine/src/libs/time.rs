use super::VarResult;
use crate::ast::stat_expr_types::VarIndex;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SyntaxType};
use crate::helper::{move_element, pine_ref_to_i64, pine_ref_to_string, Resolution, Session};
use crate::runtime::{downcast_ctx, Ctx};
use crate::types::{
    Callable, CallableEvaluate, Evaluate, EvaluateVal, Float, Int, PineRef, RuntimeErr, Series,
    SeriesCall,
};
use chrono_tz::Tz;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
struct TimeVal {
    time_index: Option<VarIndex>,
}

impl TimeVal {
    pub fn new() -> TimeVal {
        TimeVal { time_index: None }
    }
}

impl<'a> EvaluateVal<'a> for TimeVal {
    fn custom_name(&self) -> &str {
        "time"
    }

    fn call(&mut self, ctx: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, RuntimeErr> {
        if self.time_index.is_none() {
            self.time_index = downcast_ctx(ctx).get_rel_varname_index("_time");
        }
        match ctx.get_var(self.time_index.unwrap()) {
            Some(val) => Ok(val.clone()),
            _ => Ok(PineRef::new_rc(Series::from(Float::from(None)))),
        }
    }

    fn copy(&self) -> Box<dyn EvaluateVal<'a>> {
        Box::new(self.clone())
    }
}

#[derive(Debug, Clone, PartialEq)]
struct TimeCallVal {
    time_index: RefCell<Option<VarIndex>>,
    tz: RefCell<Option<Tz>>,
}

impl TimeCallVal {
    pub fn new() -> TimeCallVal {
        TimeCallVal {
            time_index: RefCell::new(None),
            tz: RefCell::new(None),
        }
    }
}

impl<'a> SeriesCall<'a> for TimeCallVal {
    fn step(
        &mut self,
        ctx: &mut dyn Ctx<'a>,
        _p: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        if self.time_index.borrow().is_none() {
            let i = downcast_ctx(ctx).get_rel_varname_index("_time").unwrap();
            self.time_index.replace(Some(i));
            match downcast_ctx(ctx).get_syminfo() {
                Some(syminfo) => {
                    self.tz.replace(Some(syminfo.timezone.parse().unwrap()));
                }
                _ => {
                    self.tz.replace(Some(Tz::America__New_York));
                }
            }
        }
        let index = self.time_index.borrow().unwrap();
        process_time(
            _p,
            pine_ref_to_i64(ctx.get_var(index).clone()),
            self.tz.borrow().as_ref().unwrap(),
        )
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        Box::new(self.clone())
    }
}

fn process_time<'a>(
    mut param: Vec<Option<PineRef<'a>>>,
    timeval: Option<i64>,
    tz: &Tz,
) -> Result<PineRef<'a>, RuntimeErr> {
    match timeval {
        Some(timeval) => {
            if param.len() == 2 {
                move_tuplet!((res, session) = param);
                let res = pine_ref_to_string(res).unwrap_or(String::from(""));
                let session = pine_ref_to_string(session).unwrap_or(String::from(""));
                if !Session::parse(&session)?.is_in(timeval, tz) {
                    Ok(PineRef::new_rc(Series::from(Int::from(None))))
                } else {
                    Ok(PineRef::new_rc(Series::from(Some(
                        Resolution::parse(&res)?.get_restime(timeval, tz),
                    ))))
                }
            } else {
                let res =
                    pine_ref_to_string(move_element(&mut param, 0)).unwrap_or(String::from(""));
                Ok(PineRef::new_rc(Series::from(Some(
                    Resolution::parse(&res)?.get_restime(timeval, tz),
                ))))
            }
        }
        _ => Ok(PineRef::new_rc(Series::from(Int::from(None)))),
    }
}

pub const VAR_NAME: &'static str = "time";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(CallableEvaluate::new(
        || Evaluate::new(Box::new(TimeVal::new())),
        || Callable::new(None, Some(Box::new(TimeCallVal::new()))),
    ));

    let func_type = FunctionTypes(vec![FunctionType::new((
        vec![
            ("resolution", SyntaxType::string()),
            ("session", SyntaxType::string()),
        ],
        SyntaxType::int_series(),
    ))]);
    let syntax_type =
        SyntaxType::ValFunction(Box::new(SyntaxType::int_series()), Rc::new(func_type));
    VarResult::new(value, syntax_type, VAR_NAME)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::stat_expr_types::VarIndex;
    use crate::ast::syntax_type::SimpleSyntaxType;
    use crate::runtime::{AnySeries, NoneCallback, SymbolInfo, VarOperate};
    use crate::{LibInfo, PineParser, PineRunner};
    use chrono::TimeZone;

    #[test]
    fn time_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![
                ("close", SyntaxType::Series(SimpleSyntaxType::Float)),
                ("_time", SyntaxType::Series(SimpleSyntaxType::Int)),
            ],
        );
        let src = "m = time";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![
                    ("close", AnySeries::from_float_vec(vec![Some(2f64)])),
                    ("_time", AnySeries::from_int_vec(vec![Some(5i64)])),
                ],
                None,
            )
            .unwrap();
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(0, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(5i64)])))
        );
    }

    #[test]
    fn time_func_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![
                ("close", SyntaxType::Series(SimpleSyntaxType::Float)),
                ("_time", SyntaxType::Series(SimpleSyntaxType::Int)),
            ],
        );
        let src = "m = time('5', '0900-1700')";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());
        let ts = Tz::Asia__Shanghai
            .ymd(2020, 2, 17)
            .and_hms(16, 12, 0)
            .timestamp()
            * 1000;
        runner
            .run(
                &vec![
                    ("close", AnySeries::from_float_vec(vec![Some(2f64)])),
                    ("_time", AnySeries::from_int_vec(vec![Some(ts)])),
                ],
                Some(Rc::new(SymbolInfo {
                    symbol_type: String::from(""),
                    timezone: String::from("Asia/Shanghai"),
                    ticker: String::from(""),
                    session: String::from(""),
                    trade_start: String::from(""),
                    trade_end: String::from(""),
                    root: None,
                    currency: String::from(""),
                    description: String::from(""),
                    mintick: 1f64,
                })),
            )
            .unwrap();

        let res_ts = Tz::Asia__Shanghai
            .ymd(2020, 2, 17)
            .and_hms(16, 10, 0)
            .timestamp()
            * 1000;
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(0, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(res_ts)])))
        );
    }
}

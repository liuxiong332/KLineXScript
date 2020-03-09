use super::VarResult;
use crate::ast::stat_expr_types::VarIndex;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SyntaxType};
use crate::helper::err_msgs::*;
use crate::helper::str_replace;
use crate::helper::{move_element, pine_ref_to_i64, pine_ref_to_string, Resolution, Session};
use crate::runtime::{downcast_ctx, Ctx};
use crate::types::{
    CallObjEval, Callable, CallableEvaluate, EvaluateVal, Float, Int, PineClass, PineFrom, PineRef,
    RefData, RuntimeErr, Series, SeriesCall,
};
use chrono::Datelike;
use chrono::TimeZone;
use chrono::Timelike;
use chrono_tz::Tz;
use std::collections::BTreeMap;
use std::mem::transmute;
use std::rc::Rc;

pub fn parse_time_from_ctx<'a>(ctx: &mut dyn Ctx<'a>) -> (VarIndex, Tz) {
    let i = downcast_ctx(ctx).get_varname_index("_time").unwrap();
    let time_index = VarIndex::new(*i, 0);
    (time_index, parse_tz_from_ctx(ctx))
}

pub fn parse_tz_from_ctx<'a>(ctx: &mut dyn Ctx<'a>) -> Tz {
    let tz: Tz = match downcast_ctx(ctx).get_syminfo() {
        Some(syminfo) => syminfo.timezone.parse().unwrap(),
        None => Tz::America__New_York,
    };
    tz
}

// fn get_year_from_ts(tz: &Tz, cur_time: Int) -> Int {
//     let year = tz.timestamp_millis(cur_time.unwrap()).year() as i64;
//     Some(year)
// }

#[derive(Debug, Clone, PartialEq)]
struct TimeVal {
    processor: *mut (),
    time_index: Option<VarIndex>,
    tz: Option<Tz>,
}

impl TimeVal {
    pub fn new(processor: *mut ()) -> TimeVal {
        TimeVal {
            processor,
            time_index: None,
            tz: None,
        }
    }
}

impl<'a> EvaluateVal<'a> for TimeVal {
    fn custom_name(&self) -> &str {
        "time"
    }

    fn call(&mut self, ctx: &mut dyn Ctx<'a>) -> Result<PineRef<'a>, RuntimeErr> {
        if self.time_index.is_none() {
            let (time_index, tz) = parse_time_from_ctx(ctx);
            self.time_index = Some(time_index);
            self.tz = Some(tz);
        }
        let processor =
            unsafe { transmute::<_, fn(Option<PineRef<'a>>, &Tz) -> PineRef<'a>>(self.processor) };
        Ok(processor(
            ctx.get_var(self.time_index.unwrap()).clone(),
            self.tz.as_ref().unwrap(),
        ))
    }

    fn copy(&self) -> Box<dyn EvaluateVal<'a>> {
        Box::new(self.clone())
    }
}

#[derive(Debug, Clone, PartialEq)]
struct TimeCallVal {
    processor: *mut (),
    tz: Option<Tz>,
}

impl TimeCallVal {
    pub fn new(processor: *mut ()) -> TimeCallVal {
        TimeCallVal {
            processor,
            tz: None,
        }
    }
}

impl<'a> SeriesCall<'a> for TimeCallVal {
    fn step(
        &mut self,
        ctx: &mut dyn Ctx<'a>,
        mut param: Vec<Option<PineRef<'a>>>,
        _func_type: FunctionType<'a>,
    ) -> Result<PineRef<'a>, RuntimeErr> {
        if self.tz.is_none() {
            self.tz = Some(parse_tz_from_ctx(ctx));
        }
        let processor =
            unsafe { transmute::<_, fn(Option<PineRef<'a>>, &Tz) -> PineRef<'a>>(self.processor) };
        Ok(processor(
            move_element(&mut param, 0),
            self.tz.as_ref().unwrap(),
        ))
    }

    fn copy(&self) -> Box<dyn SeriesCall<'a> + 'a> {
        Box::new(self.clone())
    }
}

pub fn declare_time_var<'a>(
    name: &'static str,
    processor: fn(Option<PineRef<'a>>, &Tz) -> PineRef<'a>,
    func: fn() -> Callable<'a>,
) -> VarResult<'a> {
    let value = PineRef::new(CallableEvaluate::new(
        Box::new(TimeVal::new(processor as *mut ())),
        func,
    ));

    // plot(series, title, color, linewidth, style, trackprice, transp, histbase, offset, join, editable, show_last) → plot

    let func_type = FunctionTypes(vec![FunctionType::new((
        vec![("time", SyntaxType::int_series())],
        SyntaxType::int_series(),
    ))]);
    let syntax_type =
        SyntaxType::ValFunction(Box::new(SyntaxType::int_series()), Rc::new(func_type));
    VarResult::new(value, syntax_type, name)
}

fn get_year<'a>(time: Option<PineRef<'a>>, tz: &Tz) -> PineRef<'a> {
    let val: Int = match pine_ref_to_i64(time) {
        Some(v) => {
            let year = tz.timestamp_millis(v).year() as i64;
            Some(year)
        }
        None => None,
    };
    PineRef::new(Series::from(val))
}

fn get_month<'a>(time: Option<PineRef<'a>>, tz: &Tz) -> PineRef<'a> {
    let val: Int = match pine_ref_to_i64(time) {
        Some(v) => {
            let month = tz.timestamp_millis(v).month() as i64;
            Some(month)
        }
        None => None,
    };
    PineRef::new(Series::from(val))
}

fn get_weekofyear<'a>(time: Option<PineRef<'a>>, tz: &Tz) -> PineRef<'a> {
    let val: Int = match pine_ref_to_i64(time) {
        Some(v) => {
            let week = tz.timestamp_millis(v).iso_week().week() as i64;
            Some(week)
        }
        None => None,
    };
    PineRef::new(Series::from(val))
}

fn get_dayofmonth<'a>(time: Option<PineRef<'a>>, tz: &Tz) -> PineRef<'a> {
    let val: Int = match pine_ref_to_i64(time) {
        Some(v) => {
            let week = tz.timestamp_millis(v).day() as i64;
            Some(week)
        }
        None => None,
    };
    PineRef::new(Series::from(val))
}

fn get_dayofweek<'a>(time: Option<PineRef<'a>>, tz: &Tz) -> PineRef<'a> {
    let val: Int = match pine_ref_to_i64(time) {
        Some(v) => {
            let week = tz.timestamp_millis(v).weekday().number_from_sunday() as i64;
            Some(week)
        }
        None => None,
    };
    PineRef::new(Series::from(val))
}

fn get_hour<'a>(time: Option<PineRef<'a>>, tz: &Tz) -> PineRef<'a> {
    let val: Int = match pine_ref_to_i64(time) {
        Some(v) => {
            let week = tz.timestamp_millis(v).hour() as i64;
            Some(week)
        }
        None => None,
    };
    PineRef::new(Series::from(val))
}

fn get_minute<'a>(time: Option<PineRef<'a>>, tz: &Tz) -> PineRef<'a> {
    let val: Int = match pine_ref_to_i64(time) {
        Some(v) => {
            let week = tz.timestamp_millis(v).minute() as i64;
            Some(week)
        }
        None => None,
    };
    PineRef::new(Series::from(val))
}

fn get_second<'a>(time: Option<PineRef<'a>>, tz: &Tz) -> PineRef<'a> {
    let val: Int = match pine_ref_to_i64(time) {
        Some(v) => {
            let week = tz.timestamp_millis(v).second() as i64;
            Some(week)
        }
        None => None,
    };
    PineRef::new(Series::from(val))
}

pub fn declare_year_var<'a>() -> VarResult<'a> {
    declare_time_var("year", get_year, || {
        Callable::new(None, Some(Box::new(TimeCallVal::new(get_year as *mut ()))))
    })
}

pub fn declare_month_var<'a>() -> VarResult<'a> {
    declare_time_var("month", get_month, || {
        Callable::new(None, Some(Box::new(TimeCallVal::new(get_month as *mut ()))))
    })
}

pub fn declare_weekofyear_var<'a>() -> VarResult<'a> {
    declare_time_var("weekofyear", get_weekofyear, || {
        Callable::new(
            None,
            Some(Box::new(TimeCallVal::new(get_weekofyear as *mut ()))),
        )
    })
}

pub fn declare_dayofmonth_var<'a>() -> VarResult<'a> {
    declare_time_var("dayofmonth", get_dayofmonth, || {
        Callable::new(
            None,
            Some(Box::new(TimeCallVal::new(get_dayofmonth as *mut ()))),
        )
    })
}

struct DayOfWeekProps;

impl<'a> PineClass<'a> for DayOfWeekProps {
    fn custom_type(&self) -> &str {
        "dayofweek"
    }

    fn get(&self, _ctx: &mut dyn Ctx<'a>, name: &str) -> Result<PineRef<'a>, RuntimeErr> {
        match name {
            "sunday" => Ok(PineRef::new_box(Some(1i64))),
            "monday" => Ok(PineRef::new_box(Some(2i64))),
            "tuesday" => Ok(PineRef::new_box(Some(3i64))),
            "wednesday" => Ok(PineRef::new_box(Some(4i64))),
            "thursday" => Ok(PineRef::new_box(Some(5i64))),
            "friday" => Ok(PineRef::new_box(Some(6i64))),
            "saturday" => Ok(PineRef::new_box(Some(7i64))),
            _ => Err(RuntimeErr::NotImplement(str_replace(
                NO_FIELD_IN_OBJECT,
                vec![String::from(name), String::from("dayofweek")],
            ))),
        }
    }

    fn copy(&self) -> Box<dyn PineClass<'a> + 'a> {
        Box::new(DayOfWeekProps)
    }
}

pub fn declare_dayofweek_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(CallObjEval::new(
        Box::new(TimeVal::new(get_dayofweek as *mut ())),
        Box::new(DayOfWeekProps),
        || {
            Callable::new(
                None,
                Some(Box::new(TimeCallVal::new(get_dayofweek as *mut ()))),
            )
        },
    ));

    // plot(series, title, color, linewidth, style, trackprice, transp, histbase, offset, join, editable, show_last) → plot

    let func_type = FunctionTypes(vec![FunctionType::new((
        vec![("time", SyntaxType::int_series())],
        SyntaxType::int_series(),
    ))]);
    let mut obj_type = BTreeMap::new();
    obj_type.insert("sunday", SyntaxType::int());
    obj_type.insert("monday", SyntaxType::int());
    obj_type.insert("tuesday", SyntaxType::int());
    obj_type.insert("wednesday", SyntaxType::int());
    obj_type.insert("thursday", SyntaxType::int());
    obj_type.insert("friday", SyntaxType::int());
    obj_type.insert("saturday", SyntaxType::int());
    let syntax_type = SyntaxType::ValObjectFunction(
        Box::new(SyntaxType::int_series()),
        Rc::new(obj_type),
        Rc::new(func_type),
    );
    VarResult::new(value, syntax_type, "dayofweek")
}

pub fn declare_hour_var<'a>() -> VarResult<'a> {
    declare_time_var("hour", get_hour, || {
        Callable::new(None, Some(Box::new(TimeCallVal::new(get_hour as *mut ()))))
    })
}

pub fn declare_minute_var<'a>() -> VarResult<'a> {
    declare_time_var("minute", get_minute, || {
        Callable::new(
            None,
            Some(Box::new(TimeCallVal::new(get_minute as *mut ()))),
        )
    })
}

pub fn declare_second_var<'a>() -> VarResult<'a> {
    declare_time_var("second", get_second, || {
        Callable::new(
            None,
            Some(Box::new(TimeCallVal::new(get_second as *mut ()))),
        )
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::stat_expr_types::VarIndex;
    use crate::ast::syntax_type::SimpleSyntaxType;
    use crate::runtime::{AnySeries, NoneCallback, SymbolInfo, VarOperate};
    use crate::{LibInfo, PineParser, PineRunner};
    use chrono::TimeZone;

    fn get_syminfo() -> SymbolInfo {
        SymbolInfo {
            symbol_type: String::from("future"),
            timezone: String::from("Asia/Shanghai"),
            ticker: String::from("BATS:MSFT"),
            session: String::from("regular"),
            trade_start: String::from(""),
            trade_end: String::from(""),
            root: Some(String::from("le")),
            currency: String::from("USD"),
            description: String::from("des"),
            mintick: 1f64,
        }
    }

    #[test]
    fn time_test() {
        let time_param = Tz::Asia__Shanghai
            .ymd(2021, 10, 12)
            .and_hms(1, 2, 3)
            .timestamp_millis();

        let src = format!(
            "y1 = year\ny2=year({:?})\n
            m1=month\nm2=month({:?})\n
            w1=weekofyear\nw2=weekofyear({:?})\n
            d1=dayofmonth\nd2=dayofmonth({:?})\n
            d3=dayofweek\nd4=dayofweek({:?})\n
            h1=hour\nh2=hour({:?})\n
            mi1=minute\nmi2=minute({:?})\n
            s1=second\ns2=second({:?})\n",
            time_param,
            time_param,
            time_param,
            time_param,
            time_param,
            time_param,
            time_param,
            time_param
        );
        let lib_info = LibInfo::new(
            vec![
                declare_year_var(),
                declare_month_var(),
                declare_weekofyear_var(),
                declare_dayofmonth_var(),
                declare_dayofweek_var(),
                declare_hour_var(),
                declare_minute_var(),
                declare_second_var(),
            ],
            vec![("_time", SyntaxType::Series(SimpleSyntaxType::Int))],
        );
        let blk = PineParser::new(&src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        let time = Tz::Asia__Shanghai
            .ymd(2020, 1, 20)
            .and_hms(10, 11, 12)
            .timestamp_millis();
        runner
            .run(
                &vec![("_time", AnySeries::from_int_vec(vec![Some(time)]))],
                Some(Rc::new(get_syminfo())),
            )
            .unwrap();

        let stari = 9;

        // year
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(stari, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(2020)])))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(stari + 1, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(2021)])))
        );

        // month
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(stari + 2, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(1)])))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(stari + 3, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(10)])))
        );

        // weekofyear
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(stari + 4, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(4)])))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(stari + 5, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(41)])))
        );

        // dayofmonth
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(stari + 6, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(20)])))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(stari + 7, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(12)])))
        );

        // dayofweek
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(stari + 8, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(2)])))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(stari + 9, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(3)])))
        );

        // hour
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(stari + 10, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(10)])))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(stari + 11, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(1)])))
        );

        // minute
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(stari + 12, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(11)])))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(stari + 13, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(2)])))
        );

        // second
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(stari + 14, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(12)])))
        );
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(stari + 15, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(3)])))
        );
    }

    #[test]
    fn dayofweek_test() {
        use crate::types::{downcast_pf, Tuple};

        let lib_info = LibInfo::new(
            vec![declare_dayofweek_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = "m = [
            dayofweek.sunday, dayofweek.monday, dayofweek.tuesday, dayofweek.wednesday, 
            dayofweek.thursday, dayofweek.friday, dayofweek.saturday
        ]";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![("close", AnySeries::from_float_vec(vec![Some(1f64)]))],
                None,
            )
            .unwrap();
        let tuple_res =
            downcast_pf::<Tuple>(runner.get_context().move_var(VarIndex::new(2, 0)).unwrap());
        let tuple_vec = tuple_res.unwrap().into_inner().0;

        assert_eq!(
            tuple_vec,
            vec![
                PineRef::new(Some(1i64)),
                PineRef::new(Some(2i64)),
                PineRef::new(Some(3i64)),
                PineRef::new(Some(4i64)),
                PineRef::new(Some(5i64)),
                PineRef::new(Some(6i64)),
                PineRef::new(Some(7i64))
            ]
        );
    }
}

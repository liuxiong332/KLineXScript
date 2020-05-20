use super::VarResult;
use crate::ast::stat_expr_types::VarIndex;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SyntaxType};
use crate::helper::err_msgs::*;
use crate::helper::str_replace;
use crate::helper::{
    ensure_srcs, move_element, pine_ref_to_i64, pine_ref_to_string, Resolution, Session,
};
use crate::runtime::{downcast_ctx, Ctx};
use crate::types::{
    CallObjEval, Callable, CallableEvaluate, Evaluate, EvaluateVal, Float, Int, PineClass,
    PineFrom, PineRef, RefData, RuntimeErr, Series, SeriesCall,
};
use chrono::{DateTime, Datelike, Local, TimeZone, Timelike};
use chrono_tz::Tz;
use std::collections::BTreeMap;
use std::mem::transmute;
use std::rc::Rc;

// pub fn parse_time_from_ctx<'a>(ctx: &mut dyn Ctx<'a>) -> (VarIndex, Tz) {
//     let time_index;
//     ensure_srcs(ctx, vec!["_time"], |indexs| {
//         time_index = indexs[0];
//     });
//     (time_index, parse_tz_from_ctx(ctx))
// }

#[derive(Debug, Clone, PartialEq)]
pub enum MyTz {
    Tz(Tz),
    Local,
}

impl MyTz {
    fn year(&self, millis: i64) -> i64 {
        match self {
            MyTz::Tz(tz) => tz.timestamp_millis(millis).year() as i64,
            MyTz::Local => Local.timestamp_millis(millis).year() as i64,
        }
    }

    fn month(&self, millis: i64) -> i64 {
        match self {
            MyTz::Tz(tz) => tz.timestamp_millis(millis).month() as i64,
            MyTz::Local => Local.timestamp_millis(millis).month() as i64,
        }
    }
    fn weekofyear(&self, millis: i64) -> i64 {
        match self {
            MyTz::Tz(tz) => tz.timestamp_millis(millis).iso_week().week() as i64,
            MyTz::Local => Local.timestamp_millis(millis).iso_week().week() as i64,
        }
    }

    fn dayofmonth(&self, millis: i64) -> i64 {
        match self {
            MyTz::Tz(tz) => tz.timestamp_millis(millis).day() as i64,
            MyTz::Local => Local.timestamp_millis(millis).day() as i64,
        }
    }

    fn dayofweek(&self, millis: i64) -> i64 {
        match self {
            MyTz::Tz(tz) => tz.timestamp_millis(millis).weekday().number_from_sunday() as i64,
            MyTz::Local => Local
                .timestamp_millis(millis)
                .weekday()
                .number_from_sunday() as i64,
        }
    }

    fn hour(&self, millis: i64) -> i64 {
        match self {
            MyTz::Tz(tz) => tz.timestamp_millis(millis).hour() as i64,
            MyTz::Local => Local.timestamp_millis(millis).hour() as i64,
        }
    }

    fn minute(&self, millis: i64) -> i64 {
        match self {
            MyTz::Tz(tz) => tz.timestamp_millis(millis).minute() as i64,
            MyTz::Local => Local.timestamp_millis(millis).minute() as i64,
        }
    }

    fn second(&self, millis: i64) -> i64 {
        match self {
            MyTz::Tz(tz) => tz.timestamp_millis(millis).second() as i64,
            MyTz::Local => Local.timestamp_millis(millis).second() as i64,
        }
    }
}

pub fn parse_tz_from_ctx<'a>(ctx: &mut dyn Ctx<'a>) -> MyTz {
    match downcast_ctx(ctx).get_syminfo() {
        Some(syminfo) => match syminfo.timezone.parse() {
            Ok(tz) => MyTz::Tz(tz),
            Err(_) => MyTz::Local,
        },
        None => MyTz::Local,
    }
}

// fn get_year_from_ts(tz: &Tz, cur_time: Int) -> Int {
//     let year = tz.timestamp_millis(cur_time.unwrap()).year() as i64;
//     Some(year)
// }

#[derive(Debug, Clone, PartialEq)]
struct TimeVal {
    processor: *mut (),
    time_index: Option<VarIndex>,
    tz: Option<MyTz>,
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
            ensure_srcs(ctx, vec!["_time"], |indexs| {
                self.time_index = Some(indexs[0]);
            });
            self.tz = Some(parse_tz_from_ctx(ctx));
        }
        let processor = unsafe {
            transmute::<_, fn(Option<PineRef<'a>>, &MyTz) -> PineRef<'a>>(self.processor)
        };
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
    tz: Option<MyTz>,
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
        let processor = unsafe {
            transmute::<_, fn(Option<PineRef<'a>>, &MyTz) -> PineRef<'a>>(self.processor)
        };
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
    processor: fn() -> Evaluate<'a>,
    func: fn() -> Callable<'a>,
) -> VarResult<'a> {
    let value = PineRef::new(CallableEvaluate::new(processor, func));

    let func_type = FunctionTypes(vec![FunctionType::new((
        vec![("time", SyntaxType::int_series())],
        SyntaxType::int_series(),
    ))]);
    let syntax_type =
        SyntaxType::ValFunction(Box::new(SyntaxType::int_series()), Rc::new(func_type));
    VarResult::new(value, syntax_type, name)
}

fn get_year<'a>(time: Option<PineRef<'a>>, tz: &MyTz) -> PineRef<'a> {
    let val: Int = match pine_ref_to_i64(time) {
        Some(v) => {
            let year = tz.year(v);
            Some(year)
        }
        None => None,
    };
    PineRef::new(Series::from(val))
}

fn get_month<'a>(time: Option<PineRef<'a>>, tz: &MyTz) -> PineRef<'a> {
    let val: Int = match pine_ref_to_i64(time) {
        Some(v) => {
            let month = tz.month(v);
            Some(month)
        }
        None => None,
    };
    PineRef::new(Series::from(val))
}

fn get_weekofyear<'a>(time: Option<PineRef<'a>>, tz: &MyTz) -> PineRef<'a> {
    let val: Int = match pine_ref_to_i64(time) {
        Some(v) => {
            let week = tz.weekofyear(v);
            Some(week)
        }
        None => None,
    };
    PineRef::new(Series::from(val))
}

fn get_dayofmonth<'a>(time: Option<PineRef<'a>>, tz: &MyTz) -> PineRef<'a> {
    let val: Int = match pine_ref_to_i64(time) {
        Some(v) => {
            let week = tz.dayofmonth(v);
            Some(week)
        }
        None => None,
    };
    PineRef::new(Series::from(val))
}

fn get_dayofweek<'a>(time: Option<PineRef<'a>>, tz: &MyTz) -> PineRef<'a> {
    let val: Int = match pine_ref_to_i64(time) {
        Some(v) => {
            let week = tz.dayofweek(v);
            Some(week)
        }
        None => None,
    };
    PineRef::new(Series::from(val))
}

fn get_hour<'a>(time: Option<PineRef<'a>>, tz: &MyTz) -> PineRef<'a> {
    let val: Int = match pine_ref_to_i64(time) {
        Some(v) => {
            let week = tz.hour(v);
            Some(week)
        }
        None => None,
    };
    PineRef::new(Series::from(val))
}

fn get_minute<'a>(time: Option<PineRef<'a>>, tz: &MyTz) -> PineRef<'a> {
    let val: Int = match pine_ref_to_i64(time) {
        Some(v) => {
            let week = tz.minute(v);
            Some(week)
        }
        None => None,
    };
    PineRef::new(Series::from(val))
}

fn get_second<'a>(time: Option<PineRef<'a>>, tz: &MyTz) -> PineRef<'a> {
    let val: Int = match pine_ref_to_i64(time) {
        Some(v) => {
            let week = tz.second(v);
            Some(week)
        }
        None => None,
    };
    PineRef::new(Series::from(val))
}

pub fn declare_year_var<'a>() -> VarResult<'a> {
    declare_time_var(
        "year",
        || Evaluate::new(Box::new(TimeVal::new(get_year as *mut ()))),
        || Callable::new(None, Some(Box::new(TimeCallVal::new(get_year as *mut ())))),
    )
}

pub fn declare_month_var<'a>() -> VarResult<'a> {
    declare_time_var(
        "month",
        || Evaluate::new(Box::new(TimeVal::new(get_month as *mut ()))),
        || Callable::new(None, Some(Box::new(TimeCallVal::new(get_month as *mut ())))),
    )
}

pub fn declare_weekofyear_var<'a>() -> VarResult<'a> {
    declare_time_var(
        "weekofyear",
        || Evaluate::new(Box::new(TimeVal::new(get_weekofyear as *mut ()))),
        || {
            Callable::new(
                None,
                Some(Box::new(TimeCallVal::new(get_weekofyear as *mut ()))),
            )
        },
    )
}

pub fn declare_dayofmonth_var<'a>() -> VarResult<'a> {
    declare_time_var(
        "dayofmonth",
        || Evaluate::new(Box::new(TimeVal::new(get_dayofmonth as *mut ()))),
        || {
            Callable::new(
                None,
                Some(Box::new(TimeCallVal::new(get_dayofmonth as *mut ()))),
            )
        },
    )
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
        Box::new(DayOfWeekProps),
        || Evaluate::new(Box::new(TimeVal::new(get_dayofweek as *mut ()))),
        || {
            Callable::new(
                None,
                Some(Box::new(TimeCallVal::new(get_dayofweek as *mut ()))),
            )
        },
    ));

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
    declare_time_var(
        "hour",
        || Evaluate::new(Box::new(TimeVal::new(get_hour as *mut ()))),
        || Callable::new(None, Some(Box::new(TimeCallVal::new(get_hour as *mut ())))),
    )
}

pub fn declare_minute_var<'a>() -> VarResult<'a> {
    declare_time_var(
        "minute",
        || Evaluate::new(Box::new(TimeVal::new(get_minute as *mut ()))),
        || {
            Callable::new(
                None,
                Some(Box::new(TimeCallVal::new(get_minute as *mut ()))),
            )
        },
    )
}

pub fn declare_second_var<'a>() -> VarResult<'a> {
    declare_time_var(
        "second",
        || Evaluate::new(Box::new(TimeVal::new(get_second as *mut ()))),
        || {
            Callable::new(
                None,
                Some(Box::new(TimeCallVal::new(get_second as *mut ()))),
            )
        },
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::stat_expr_types::VarIndex;
    use crate::ast::syntax_type::SimpleSyntaxType;
    use crate::runtime::output::InputSrc;
    use crate::runtime::{AnySeries, NoneCallback, SymbolInfo, VarOperate};
    use crate::{LibInfo, PineParser, PineRunner};
    use chrono::TimeZone;

    fn get_syminfo(timezone: String) -> SymbolInfo {
        SymbolInfo {
            symbol_type: String::from("future"),
            timezone: timezone,
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
                Some(Rc::new(get_syminfo(String::from("Asia/Shanghai")))),
            )
            .unwrap();

        let stari = 0;

        // year
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(stari, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(2020)])))
        );
        assert_eq!(
            runner.get_io_info().get_input_srcs(),
            &vec![InputSrc::new(None, vec![String::from("time")])]
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
            vec![("_time", SyntaxType::Series(SimpleSyntaxType::Int))],
        );
        let src = "m = [
            dayofweek.sunday, dayofweek.monday, dayofweek.tuesday, dayofweek.wednesday, 
            dayofweek.thursday, dayofweek.friday, dayofweek.saturday
        ]";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![("_time", AnySeries::from_int_vec(vec![Some(100i64)]))],
                None,
            )
            .unwrap();
        let tuple_res =
            downcast_pf::<Tuple>(runner.get_context().move_var(VarIndex::new(0, 0)).unwrap());
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

    #[test]
    fn timezone_test() {
        let lib_info = LibInfo::new(
            vec![declare_dayofweek_var()],
            vec![("_time", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = "m = dayofweek";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());

        runner
            .run(
                &vec![("_time", AnySeries::from_int_vec(vec![Some(100i64)]))],
                Some(Rc::new(get_syminfo(String::from("Asia/Shanghai")))),
            )
            .unwrap();
        assert_eq!(
            runner.get_context().move_var(VarIndex::new(0, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(5i64)])))
        );
        runner
            .run(
                &vec![("_time", AnySeries::from_int_vec(vec![Some(100i64)]))],
                Some(Rc::new(get_syminfo(String::from("America/New_York")))),
            )
            .unwrap();

        assert_eq!(
            runner.get_context().move_var(VarIndex::new(0, 0)),
            Some(PineRef::new_rc(Series::from_vec(vec![Some(4i64)])))
        );
    }
}

use super::VarResult;
use crate::ast::stat_expr_types::VarIndex;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SimpleSyntaxType, SyntaxType};
use crate::helper::err_msgs::*;
use crate::helper::session::TradeTimeSpan;
use crate::helper::str_replace;
use crate::helper::{
    move_element, pine_ref_to_bool, pine_ref_to_color, pine_ref_to_f64, pine_ref_to_i64,
    pine_ref_to_string,
};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::output::{OutputData, OutputInfo, PlotInfo};
use crate::types::{
    Bool, Callable, CallableFactory, DataType, Float, Int, Object, ParamCollectCall, PineClass,
    PineFrom, PineRef, PineType, RefData, RuntimeErr, SecondType, Series, NA,
};
use chrono_tz::Tz;
use std::cell::{Cell, RefCell};
use std::collections::BTreeMap;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
struct BarStateProps {
    barindex_index: Cell<VarIndex>,
    time_index: Cell<VarIndex>,
    data_ranges: RefCell<Vec<(i32, i32)>>,
}

impl BarStateProps {
    pub fn new() -> BarStateProps {
        BarStateProps {
            barindex_index: Cell::new(VarIndex::new(0, 0)),
            time_index: Cell::new(VarIndex::new(0, 0)),
            data_ranges: RefCell::new(vec![(0, 0)]),
        }
    }

    fn get_varindex<'a>(&self, ctx: &mut dyn Ctx<'a>) -> i64 {
        let barindex = ctx.get_var(self.barindex_index.get()).clone();
        pine_ref_to_i64(barindex).unwrap()
    }

    fn is_last<'a>(&self, ctx: &mut dyn Ctx<'a>) -> bool {
        let index = self.get_varindex(ctx);
        let (_, end) = downcast_ctx(ctx.get_main_ctx()).get_data_range();
        index == (end.unwrap() - 1) as i64
    }

    fn is_in_trade<'a>(&self, ctx: &mut dyn Ctx<'a>) -> bool {
        match downcast_ctx(ctx.get_main_ctx()).get_syminfo() {
            Some(syminfo) => {
                let tz = syminfo.timezone.parse().unwrap();
                let timespan = TradeTimeSpan::parse_str(&syminfo.trade_start, &syminfo.trade_end);
                let time_index = self.time_index.get();
                let cur_time = pine_ref_to_i64(ctx.get_var(time_index).clone()).unwrap();
                if timespan.is_in(cur_time, &tz) {
                    true
                } else {
                    false
                }
            }
            _ => true,
        }
    }
}

impl<'a> PineClass<'a> for BarStateProps {
    fn custom_type(&self) -> &str {
        "barstate"
    }

    fn get(&self, _ctx: &mut dyn Ctx<'a>, name: &str) -> Result<PineRef<'a>, RuntimeErr> {
        if self.barindex_index.get() == VarIndex::new(0, 0) {
            let index = _ctx.get_top_varname_index("bar_index").unwrap();
            self.barindex_index.set(index);
            let index = _ctx.get_top_varname_index("_time").unwrap();
            self.time_index.set(index);
        }
        let (start, end) = downcast_ctx(_ctx.get_main_ctx()).get_data_range();
        if self.data_ranges.borrow().last() != Some(&(start.unwrap(), end.unwrap())) {
            self.data_ranges
                .borrow_mut()
                .push((start.unwrap(), end.unwrap()));
        }
        match name {
            "isfirst" => {
                let index = self.get_varindex(_ctx);
                if index == 0 {
                    Ok(PineRef::new_rc(Series::from(true)))
                } else {
                    Ok(PineRef::new_rc(Series::from(false)))
                }
            }
            "islast" => {
                if self.is_last(_ctx) {
                    Ok(PineRef::new_rc(Series::from(true)))
                } else {
                    Ok(PineRef::new_rc(Series::from(false)))
                }
            }
            "ishistory" => {
                if !self.is_last(_ctx) {
                    Ok(PineRef::new_rc(Series::from(true)))
                } else {
                    // The point is the last point and in trade time.
                    Ok(PineRef::new_rc(Series::from(!self.is_in_trade(_ctx))))
                }
            }
            "isrealtime" => {
                if !self.is_last(_ctx) {
                    Ok(PineRef::new_rc(Series::from(false)))
                } else {
                    // The point is the last point and in trade time.
                    Ok(PineRef::new_rc(Series::from(self.is_in_trade(_ctx))))
                }
            }
            "isnew" => {
                let index = self.get_varindex(_ctx) as i32;
                let appear_count = self
                    .data_ranges
                    .borrow()
                    .iter()
                    .filter(|range| index >= range.0 && index < range.1)
                    .count();
                if appear_count > 1 {
                    Ok(PineRef::new_rc(Series::from(false)))
                } else {
                    Ok(PineRef::new_rc(Series::from(true)))
                }
            }
            "isconfirmed" => {
                let index = self.get_varindex(_ctx) as i32;
                let appear_count = self
                    .data_ranges
                    .borrow()
                    .iter()
                    .filter(|range| index >= range.0 && index < range.1)
                    .count();
                if appear_count > 1 {
                    let end_index = self.data_ranges.borrow().last().unwrap().1;
                    // appearing not only once but not the last on is considered as onfirmed.
                    Ok(PineRef::new_rc(Series::from(index != end_index - 1)))
                } else {
                    Ok(PineRef::new_rc(Series::from(false)))
                }
            }
            _ => Err(RuntimeErr::NotImplement(str_replace(
                NO_FIELD_IN_OBJECT,
                vec![String::from(name), String::from("plot")],
            ))),
        }
    }

    fn copy(&self) -> Box<dyn PineClass<'a> + 'a> {
        Box::new(self.clone())
    }
}

pub const VAR_NAME: &'static str = "barstate";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(Object::new(Box::new(BarStateProps::new())));

    let mut obj_type = BTreeMap::new();
    obj_type.insert("isfirst", SyntaxType::bool());
    obj_type.insert("islast", SyntaxType::bool());
    obj_type.insert("ishistory", SyntaxType::bool());
    obj_type.insert("isrealtime", SyntaxType::bool());
    obj_type.insert("isnew", SyntaxType::bool());
    obj_type.insert("isconfirmed", SyntaxType::bool());
    let syntax_type = SyntaxType::Object(Rc::new(obj_type));
    VarResult::new(value, syntax_type, VAR_NAME)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::context::VarOperate;
    use crate::runtime::{AnySeries, NoneCallback, SymbolInfo};
    use crate::{LibInfo, PineParser, PineRunner};
    use chrono::offset::TimeZone;
    use std::mem;

    fn gen_ts(h: u32, m: u32) -> i64 {
        Tz::Asia__Shanghai
            .ymd(2020, 2, 17)
            .and_hms(h, m, 0)
            .timestamp()
            * 1000
    }

    #[test]
    fn plot_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![
                ("close", SyntaxType::Series(SimpleSyntaxType::Float)),
                ("_time", SyntaxType::Series(SimpleSyntaxType::Int)),
                ("bar_index", SyntaxType::Series(SimpleSyntaxType::Int)),
            ],
        );

        // [
        //     , barstate.islast, barstate.ishistory,
        //     barstate.isrealtime, barstate.isnew, barstate.isconfirmed
        let src = "m1=barstate.isfirst
            m2=barstate.islast
            m3=barstate.ishistory
            m4=barstate.isrealtime
            m5=barstate.isnew
            m6=barstate.isconfirmed
        ";
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());
        downcast_ctx(runner.get_context()).update_data_range((Some(0), Some(2)));
        runner
            .run(
                &vec![
                    (
                        "close",
                        AnySeries::from_float_vec(vec![Some(1f64), Some(2f64)]),
                    ),
                    (
                        "_time",
                        AnySeries::from_int_vec(vec![Some(gen_ts(10, 0)), Some(gen_ts(14, 0))]),
                    ),
                ],
                Some(Rc::new(SymbolInfo {
                    symbol_type: String::from(""),
                    timezone: String::from("Asia/Shanghai"),
                    ticker: String::from(""),
                    session: String::from(""),
                    trade_start: String::from("9:30"),
                    trade_end: String::from("15:00"),
                    root: None,
                    currency: String::from(""),
                    description: String::from(""),
                    mintick: 1f64,
                })),
            )
            .unwrap();

        assert_eq!(
            runner.get_context().get_var(VarIndex::new(0, 0)),
            &Some(PineRef::new(Series::from_vec(vec![true, false])))
        );
        assert_eq!(
            runner.get_context().get_var(VarIndex::new(1, 0)),
            &Some(PineRef::new(Series::from_vec(vec![false, true])))
        );
        assert_eq!(
            runner.get_context().get_var(VarIndex::new(2, 0)),
            &Some(PineRef::new(Series::from_vec(vec![true, false])))
        );
        assert_eq!(
            runner.get_context().get_var(VarIndex::new(3, 0)),
            &Some(PineRef::new(Series::from_vec(vec![false, true])))
        );
        assert_eq!(
            runner.get_context().get_var(VarIndex::new(4, 0)),
            &Some(PineRef::new(Series::from_vec(vec![true, true])))
        );
        assert_eq!(
            runner.get_context().get_var(VarIndex::new(5, 0)),
            &Some(PineRef::new(Series::from_vec(vec![false, false])))
        );

        runner
            .update(&vec![
                (
                    "close",
                    AnySeries::from_float_vec(vec![Some(1f64), Some(2f64)]),
                ),
                (
                    "_time",
                    AnySeries::from_int_vec(vec![Some(gen_ts(14, 0)), Some(gen_ts(15, 0))]),
                ),
            ])
            .unwrap();

        assert_eq!(
            runner.get_context().get_var(VarIndex::new(4, 0)),
            &Some(PineRef::new(Series::from_vec(vec![true, false, true])))
        );
        assert_eq!(
            runner.get_context().get_var(VarIndex::new(5, 0)),
            &Some(PineRef::new(Series::from_vec(vec![false, true, false])))
        );
    }

    #[test]
    fn barstate_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![
                ("close", SyntaxType::Series(SimpleSyntaxType::Float)),
                ("_time", SyntaxType::Series(SimpleSyntaxType::Int)),
                ("bar_index", SyntaxType::Series(SimpleSyntaxType::Int)),
            ],
        );

        let src = r#"
            float a1 = barstate.isconfirmed ? close :na
            float a2 = barstate.isfirst ? close :na
            float a3 = barstate.ishistory? close :na
            float a4 = barstate.islast ? close :na
            float a5 = barstate.isnew ? close :na
            float a6 = barstate.isrealtime ? close :na
        "#;
        let blk = PineParser::new(src, &lib_info).parse_blk().unwrap();
        let mut runner = PineRunner::new(&lib_info, &blk, &NoneCallback());
        downcast_ctx(runner.get_context()).update_data_range((Some(0), Some(2)));
        runner
            .run(
                &vec![
                    (
                        "close",
                        AnySeries::from_float_vec(vec![Some(1f64), Some(2f64)]),
                    ),
                    (
                        "_time",
                        AnySeries::from_int_vec(vec![Some(gen_ts(10, 0)), Some(gen_ts(14, 0))]),
                    ),
                ],
                Some(Rc::new(SymbolInfo {
                    symbol_type: String::from(""),
                    timezone: String::from("Asia/Shanghai"),
                    ticker: String::from(""),
                    session: String::from(""),
                    trade_start: String::from("9:30"),
                    trade_end: String::from("15:00"),
                    root: None,
                    currency: String::from(""),
                    description: String::from(""),
                    mintick: 1f64,
                })),
            )
            .unwrap();
    }
}

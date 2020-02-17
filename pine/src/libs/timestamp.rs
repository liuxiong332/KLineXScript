use super::VarResult;
use crate::ast::syntax_type::{FunctionType, FunctionTypes, SyntaxType};
use crate::helper::err_msgs::*;
use crate::helper::str_replace;
use crate::helper::{move_element, pine_ref_to_bool, pine_ref_to_i64, pine_ref_to_string};
use crate::runtime::context::{downcast_ctx, Ctx};
use crate::runtime::{ScriptPurpose, StudyScript};
use crate::types::{Callable, CallableFactory, Int, PineFrom, PineRef, RuntimeErr, Series, NA};
use chrono::TimeZone;
use chrono_tz::Tz;
use regex::Regex;
use std::rc::Rc;

pub fn pine_ref_to_i64_or<'a>(val: Option<PineRef<'a>>, defval: i64) -> i64 {
    if val.is_none() {
        return defval;
    }
    match Int::implicity_from(val.unwrap()) {
        Ok(res) => res.into_inner().unwrap(),
        Err(_) => defval,
    }
}

pub fn get_ymd_dm_require<'a>(
    year: Option<PineRef<'a>>,
    month: Option<PineRef<'a>>,
    day: Option<PineRef<'a>>,
    hour: Option<PineRef<'a>>,
    minute: Option<PineRef<'a>>,
) -> Result<(i64, i64, i64, i64, i64), RuntimeErr> {
    match (
        pine_ref_to_i64(year),
        pine_ref_to_i64(month),
        pine_ref_to_i64(day),
        pine_ref_to_i64(hour),
        pine_ref_to_i64(minute),
    ) {
        (Some(y), Some(m), Some(d), Some(h), Some(mi)) => Ok((y, m, d, h, mi)),
        (y, m, d, h, mi) => {
            let mut miss_params = vec![];
            if y.is_none() {
                miss_params.push(String::from("year"));
            }
            if m.is_none() {
                miss_params.push(String::from("month"));
            }
            if d.is_none() {
                miss_params.push(String::from("day"));
            }
            if h.is_none() {
                miss_params.push(String::from("hour"));
            }
            if mi.is_none() {
                miss_params.push(String::from("minute"));
            }
            Err(RuntimeErr::MissingParameters(str_replace(
                REQUIRED_PARAMETERS,
                miss_params,
            )))
        }
    }
}

pub fn get_timezone(tz: String) -> Result<Tz, RuntimeErr> {
    match tz.parse() {
        Ok(tz) => Ok(tz),
        _ => {
            let re = Regex::new(r"GMT([+-])(\d+)").unwrap();
            match re.captures(&tz) {
                Some(caps) => {
                    match (caps.get(1).unwrap().as_str(), caps.get(2).unwrap().as_str()) {
                        ("-", "0") => Ok(Tz::Etc__GMTPlus0),
                        ("-", "1") => Ok(Tz::Etc__GMTPlus1),
                        ("-", "2") => Ok(Tz::Etc__GMTPlus2),
                        ("-", "3") => Ok(Tz::Etc__GMTPlus3),
                        ("-", "4") => Ok(Tz::Etc__GMTPlus4),
                        ("-", "5") => Ok(Tz::Etc__GMTPlus5),
                        ("-", "6") => Ok(Tz::Etc__GMTPlus6),
                        ("-", "7") => Ok(Tz::Etc__GMTPlus7),
                        ("-", "8") => Ok(Tz::Etc__GMTPlus8),
                        ("-", "9") => Ok(Tz::Etc__GMTPlus9),
                        ("-", "10") => Ok(Tz::Etc__GMTPlus10),
                        ("-", "11") => Ok(Tz::Etc__GMTPlus11),
                        ("-", "12") => Ok(Tz::Etc__GMTPlus12),
                        ("+", "0") => Ok(Tz::Etc__GMTMinus0),
                        ("+", "1") => Ok(Tz::Etc__GMTMinus1),
                        ("+", "2") => Ok(Tz::Etc__GMTMinus2),
                        ("+", "3") => Ok(Tz::Etc__GMTMinus3),
                        ("+", "4") => Ok(Tz::Etc__GMTMinus4),
                        ("+", "5") => Ok(Tz::Etc__GMTMinus5),
                        ("+", "6") => Ok(Tz::Etc__GMTMinus6),
                        ("+", "7") => Ok(Tz::Etc__GMTMinus7),
                        ("+", "8") => Ok(Tz::Etc__GMTMinus8),
                        ("+", "9") => Ok(Tz::Etc__GMTMinus9),
                        ("+", "10") => Ok(Tz::Etc__GMTMinus10),
                        ("+", "11") => Ok(Tz::Etc__GMTMinus11),
                        ("+", "12") => Ok(Tz::Etc__GMTMinus12),
                        _ => Err(RuntimeErr::InvalidParameters(str_replace(
                            UNRECONGNIZED_TZ,
                            vec![String::from("timezone")],
                        ))),
                    }
                }
                _ => Err(RuntimeErr::InvalidParameters(str_replace(
                    UNRECONGNIZED_TZ,
                    vec![String::from("timezone")],
                ))),
            }
        }
    }
}

fn timestamp<'a>(
    _context: &mut dyn Ctx<'a>,
    mut param: Vec<Option<PineRef<'a>>>,
    _func_type: FunctionType<'a>,
) -> Result<PineRef<'a>, RuntimeErr> {
    let mut is_series = false;
    let result = if param.len() == 7 {
        if _func_type == gen_sig4_type() {
            is_series = true;
        }
        move_tuplet!((timezone, year, month, day, hour, minute, second) = param);
        let res = get_ymd_dm_require(year, month, day, hour, minute)?;
        (
            pine_ref_to_string(timezone).unwrap(),
            res.0,
            res.1,
            res.2,
            res.3,
            res.4,
            pine_ref_to_i64_or(second, 0),
        )
    } else {
        if _func_type == gen_sig2_type() {
            is_series = true;
        }
        move_tuplet!((year, month, day, hour, minute, second) = param);
        let res = get_ymd_dm_require(year, month, day, hour, minute)?;
        (
            String::from("Asia/Shanghai"),
            res.0,
            res.1,
            res.2,
            res.3,
            res.4,
            pine_ref_to_i64_or(second, 0),
        )
    };

    let tz: Tz = get_timezone(result.0)?;
    let dt = tz
        .ymd(result.1 as i32, result.2 as u32, result.3 as u32)
        .and_hms(result.4 as u32, result.5 as u32, result.6 as u32);

    let ts = dt.timestamp() * 1000;
    if is_series {
        Ok(PineRef::new(Series::from(Some(ts))))
    } else {
        Ok(PineRef::new(Some(ts)))
    }
}

fn gen_sig1_type<'a>() -> FunctionType<'a> {
    FunctionType::new((
        vec![
            ("year", SyntaxType::int()),
            ("month", SyntaxType::int()),
            ("day", SyntaxType::int()),
            ("hour", SyntaxType::int()),
            ("minute", SyntaxType::int()),
            ("second", SyntaxType::int()),
        ],
        SyntaxType::Void,
    ))
}

fn gen_sig2_type<'a>() -> FunctionType<'a> {
    FunctionType::new((
        vec![
            ("year", SyntaxType::int_series()),
            ("month", SyntaxType::int_series()),
            ("day", SyntaxType::int_series()),
            ("hour", SyntaxType::int_series()),
            ("minute", SyntaxType::int_series()),
            ("second", SyntaxType::int_series()),
        ],
        SyntaxType::Void,
    ))
}

fn gen_sig3_type<'a>() -> FunctionType<'a> {
    FunctionType::new((
        vec![
            ("timezone", SyntaxType::string()),
            ("year", SyntaxType::int()),
            ("month", SyntaxType::int()),
            ("day", SyntaxType::int()),
            ("hour", SyntaxType::int()),
            ("minute", SyntaxType::int()),
            ("second", SyntaxType::int()),
        ],
        SyntaxType::int(),
    ))
}

fn gen_sig4_type<'a>() -> FunctionType<'a> {
    FunctionType::new((
        vec![
            ("timezone", SyntaxType::string()),
            ("year", SyntaxType::int_series()),
            ("month", SyntaxType::int_series()),
            ("day", SyntaxType::int_series()),
            ("hour", SyntaxType::int_series()),
            ("minute", SyntaxType::int_series()),
            ("second", SyntaxType::int_series()),
        ],
        SyntaxType::int(),
    ))
}

pub const VAR_NAME: &'static str = "timestamp";

pub fn declare_var<'a>() -> VarResult<'a> {
    let value = PineRef::new(CallableFactory::new(|| {
        Callable::new(Some(timestamp), None)
    }));

    // plot(series, title, color, linewidth, style, trackprice, transp, histbase, offset, join, editable, show_last) → plot

    let func_type = FunctionTypes(vec![
        gen_sig1_type(),
        gen_sig2_type(),
        gen_sig3_type(),
        gen_sig4_type(),
    ]);
    let syntax_type = SyntaxType::Function(Rc::new(func_type));
    VarResult::new(value, syntax_type, VAR_NAME)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::stat_expr_types::VarIndex;
    use crate::ast::syntax_type::SimpleSyntaxType;
    use crate::runtime::{AnySeries, NoneCallback, VarOperate};
    use crate::{LibInfo, PineParser, PineRunner};

    #[test]
    fn timestamp_test() {
        let lib_info = LibInfo::new(
            vec![declare_var()],
            vec![("close", SyntaxType::Series(SimpleSyntaxType::Float))],
        );
        let src = "month = 2\nm = timestamp(2020, 02, 12, 0, 0)";
        let src2 = "month=2\nm = timestamp('Asia/Shanghai', 2020, 02, 12, 0, 0)";
        let src3 = "month=true ? 2 : 1\nm = timestamp(2020, month, 12, 0, 0)";
        let src4 = "month=true ? 2 : 1\nm = timestamp('Asia/Shanghai', 2020, month, 12, 0, 0)";
        let src5 = "month=2\nm = timestamp('GMT+8', 2020, 02, 12, 0, 0)";

        fn run_src<'a>(src: &'a str, lib_info: &LibInfo<'a>, res: PineRef<'a>) {
            let blk = PineParser::new(src, lib_info).parse_blk().unwrap();
            let mut runner = PineRunner::new(lib_info, &blk, &NoneCallback());

            runner
                .run(
                    &vec![("close", AnySeries::from_float_vec(vec![Some(2f64)]))],
                    None,
                )
                .unwrap();
            assert_eq!(
                runner.get_context().move_var(VarIndex::new(3, 0)),
                Some(res)
            );
        }
        run_src(src, &lib_info, PineRef::new_box(Some(1581436800000)));
        run_src(src2, &lib_info, PineRef::new_box(Some(1581436800000)));
        run_src(
            src3,
            &lib_info,
            PineRef::new_rc(Series::from_vec(vec![Some(1581436800000)])),
        );
        run_src(
            src4,
            &lib_info,
            PineRef::new_rc(Series::from_vec(vec![Some(1581436800000)])),
        );
        run_src(src5, &lib_info, PineRef::new_box(Some(1581436800000)));
    }
}

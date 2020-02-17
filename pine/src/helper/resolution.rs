use super::err_msgs::*;
use super::session::{DayTime, TradeTimeSpan};
use super::str_replace::*;
use crate::types::RuntimeErr;
use chrono::Datelike;
use chrono::Duration;
use chrono::TimeZone;
use chrono::Timelike;
use chrono_tz::Tz;
use regex::Regex;
use std::str::FromStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ResolutionType {
    Minute,
    Daily,
    Weekly,
    Monthly,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Resolution {
    count: i32,
    restype: ResolutionType,
}

impl Resolution {
    pub fn new(count: i32, restype: ResolutionType) -> Resolution {
        Resolution { count, restype }
    }

    pub fn parse(resstr: &str) -> Result<Resolution, RuntimeErr> {
        let re = Regex::new(r"(\d*)([DWM]?)").unwrap();
        match re.captures(resstr) {
            Some(caps) => {
                let count = match &caps[1] {
                    "" => 1,
                    val => i32::from_str(val).unwrap(),
                };
                let restype = match &caps[2] {
                    "D" => ResolutionType::Daily,
                    "W" => ResolutionType::Weekly,
                    "M" => ResolutionType::Monthly,
                    _ => ResolutionType::Minute,
                };
                Ok(Resolution::new(count, restype))
            }
            _ => Err(RuntimeErr::InvalidParameters(str_replace(
                UNRECONGNIZED_RES,
                vec![String::from(resstr)],
            ))),
        }
    }

    pub fn get_restime(&self, millseconds: i64, tz: &Tz) -> i64 {
        let dt = tz.timestamp(millseconds / 1000, 0);
        match self.restype {
            ResolutionType::Minute => {
                let time = dt.time();
                let minute = (time.hour() * 60 + time.minute()) as i32;
                let new_dt =
                    dt - Duration::minutes((minute - minute / self.count * self.count) as i64);
                new_dt.timestamp() * 1000
            }
            ResolutionType::Daily => {
                let time = dt.time();
                let minute = (time.hour() * 60 + time.minute()) as i64;

                let date = dt.date();
                let day = date.day0() as i32;
                let new_dt = dt
                    - Duration::days((day - day / self.count * self.count) as i64)
                    - Duration::minutes(minute);
                new_dt.timestamp() * 1000
            }
            ResolutionType::Weekly => {
                let time = dt.time();
                let minute = (time.hour() * 60 + time.minute()) as i64;

                let date = dt.date();
                let wk = date.iso_week().week0() as i32;
                let wk_count = self.count;
                let new_dt = dt
                    - Duration::weeks((wk - wk / wk_count * wk_count) as i64)
                    - Duration::minutes(minute); // Wipe the hour and minute

                // Wipe the weekday from sunday
                let new_dt =
                    new_dt - Duration::days(new_dt.weekday().num_days_from_sunday() as i64);
                new_dt.timestamp() * 1000
            }

            ResolutionType::Monthly => {
                let time = dt.time();
                let minute = (time.hour() * 60 + time.minute()) as i64;

                let date = dt.date();
                let month = date.month0();
                let count = self.count as u32;
                let new_month = month / count * count;
                let new_dt = dt.with_month0(new_month).unwrap() - Duration::minutes(minute); // Wipe the hour and minute

                // Wipe the month day.
                let new_dt = new_dt - Duration::days(new_dt.day0() as i64);
                new_dt.timestamp() * 1000
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn res_test() {
        assert_eq!(
            Resolution::parse("12"),
            Ok(Resolution::new(12, ResolutionType::Minute))
        );
        assert_eq!(
            Resolution::parse("D"),
            Ok(Resolution::new(1, ResolutionType::Daily))
        );
        assert_eq!(
            Resolution::parse("W"),
            Ok(Resolution::new(1, ResolutionType::Weekly))
        );
        assert_eq!(
            Resolution::parse("M"),
            Ok(Resolution::new(1, ResolutionType::Monthly))
        );
        assert_eq!(
            Resolution::parse("5D"),
            Ok(Resolution::new(5, ResolutionType::Daily))
        );
        assert_eq!(
            Resolution::parse("12M"),
            Ok(Resolution::new(12, ResolutionType::Monthly))
        );
    }

    #[test]
    fn restime_test() {
        let tz = Tz::Asia__Shanghai;
        assert_eq!(
            Resolution::new(4, ResolutionType::Minute).get_restime(
                tz.ymd(2020, 2, 17).and_hms(11, 25, 0).timestamp() * 1000,
                &tz
            ),
            tz.ymd(2020, 2, 17).and_hms(11, 24, 0).timestamp() * 1000
        );
        assert_eq!(
            Resolution::new(60, ResolutionType::Minute).get_restime(
                tz.ymd(2020, 2, 17).and_hms(11, 25, 0).timestamp() * 1000,
                &tz
            ),
            tz.ymd(2020, 2, 17).and_hms(11, 0, 0).timestamp() * 1000
        );

        assert_eq!(
            Resolution::new(1, ResolutionType::Daily).get_restime(
                tz.ymd(2020, 2, 17).and_hms(11, 25, 0).timestamp() * 1000,
                &tz
            ),
            tz.ymd(2020, 2, 17).and_hms(0, 0, 0).timestamp() * 1000
        );
        assert_eq!(
            Resolution::new(3, ResolutionType::Daily).get_restime(
                tz.ymd(2020, 2, 17).and_hms(11, 25, 0).timestamp() * 1000,
                &tz
            ),
            tz.ymd(2020, 2, 16).and_hms(0, 0, 0).timestamp() * 1000
        );

        assert_eq!(
            Resolution::new(1, ResolutionType::Weekly).get_restime(
                tz.ymd(2020, 2, 17).and_hms(11, 25, 0).timestamp() * 1000,
                &tz
            ),
            tz.ymd(2020, 2, 16).and_hms(0, 0, 0).timestamp() * 1000
        );
        assert_eq!(
            Resolution::new(2, ResolutionType::Weekly).get_restime(
                tz.ymd(2020, 2, 17).and_hms(11, 25, 0).timestamp() * 1000,
                &tz
            ),
            tz.ymd(2020, 2, 9).and_hms(0, 0, 0).timestamp() * 1000
        );

        assert_eq!(
            Resolution::new(1, ResolutionType::Monthly).get_restime(
                tz.ymd(2020, 2, 17).and_hms(11, 25, 0).timestamp() * 1000,
                &tz
            ),
            tz.ymd(2020, 2, 1).and_hms(0, 0, 0).timestamp() * 1000
        );

        assert_eq!(
            Resolution::new(2, ResolutionType::Monthly).get_restime(
                tz.ymd(2020, 2, 17).and_hms(11, 25, 0).timestamp() * 1000,
                &tz
            ),
            tz.ymd(2020, 1, 1).and_hms(0, 0, 0).timestamp() * 1000
        );
    }
}

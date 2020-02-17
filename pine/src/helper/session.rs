use crate::types::RuntimeErr;
use chrono::Datelike;
use chrono::TimeZone;
use chrono::Timelike;
use chrono::Weekday;
use chrono_tz::Tz;
use regex::Regex;
use std::ops::Sub;
use std::str::FromStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct DayTime {
    pub hour: i32,
    pub minute: i32,
}

impl DayTime {
    pub fn new(hour: i32, minute: i32) -> DayTime {
        DayTime { hour, minute }
    }

    pub fn is_negative(&self) -> bool {
        self.hour < 0
    }

    pub fn sub_hour(mut self, h: i32) -> DayTime {
        self.hour -= h;
        self
    }

    pub fn sub(mut self, h: i32, m: i32) -> DayTime {
        if self.minute < m {
            self.minute = self.minute + 60 - m;
            self.hour -= h + 1;
        } else {
            self.minute = self.minute - m;
            self.hour -= h;
        }
        self
    }

    pub fn from_m(m: i32) -> DayTime {
        DayTime {
            hour: m / 60,
            minute: m % 60,
        }
    }

    pub fn to_m(&self) -> i32 {
        self.hour * 60 + self.minute
    }
}

impl Sub for DayTime {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        self.sub(self.hour, self.minute)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TradeTimeSpan {
    pub start: DayTime,
    pub end: DayTime,
}

impl TradeTimeSpan {
    pub fn new(h1: i32, m1: i32, h2: i32, m2: i32) -> TradeTimeSpan {
        TradeTimeSpan {
            start: DayTime::new(h1, m1),
            end: DayTime::new(h2, m2),
        }
    }

    pub fn parse(h1: i32, m1: i32, h2: i32, m2: i32) -> TradeTimeSpan {
        let mut start = DayTime::new(h1, m1);
        let end = if h2 == 0 && m2 == 0 {
            DayTime::new(24, 0)
        } else {
            DayTime::new(h2, m2)
        };
        if start >= end {
            start = start.sub_hour(24);
        }
        TradeTimeSpan { start, end }
    }

    pub fn is_between(&self, time: &DayTime) -> bool {
        time >= &self.start && time < &self.end
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Session {
    pub spans: Vec<TradeTimeSpan>,
    pub weekdays: Vec<Weekday>,
}

impl Session {
    pub fn parse_wk(wks: &str) -> Result<Vec<Weekday>, RuntimeErr> {
        let mut res: Vec<Weekday> = vec![];
        for c in wks.chars() {
            let v = match c {
                '1' => Weekday::Sun,
                '2' => Weekday::Mon,
                '3' => Weekday::Tue,
                '4' => Weekday::Wed,
                '5' => Weekday::Thu,
                '6' => Weekday::Fri,
                '7' => Weekday::Sat,
                _ => return Err(RuntimeErr::UnrecongnizedSession),
            };
            res.push(v);
        }
        Ok(res)
    }

    fn all_weekdays() -> Vec<Weekday> {
        vec![
            Weekday::Sun,
            Weekday::Mon,
            Weekday::Tue,
            Weekday::Wed,
            Weekday::Thu,
            Weekday::Fri,
            Weekday::Sat,
        ]
    }

    fn def_weekdays() -> Vec<Weekday> {
        vec![
            Weekday::Mon,
            Weekday::Tue,
            Weekday::Wed,
            Weekday::Thu,
            Weekday::Fri,
        ]
    }

    pub fn parse(sstr: &str) -> Result<Session, RuntimeErr> {
        if sstr == "24x7" {
            let session = Session {
                spans: vec![TradeTimeSpan {
                    start: DayTime::new(0, 0),
                    end: DayTime::new(24, 0),
                }],
                weekdays: Session::all_weekdays(),
            };
            return Ok(session);
        }
        let re = Regex::new(r"(\d{2})(\d{2})-(\d{2})(\d{2})").unwrap();
        let spans: Vec<TradeTimeSpan> = re
            .captures_iter(sstr)
            .into_iter()
            .map(|caps| {
                TradeTimeSpan::parse(
                    i32::from_str(&caps[1]).unwrap(),
                    i32::from_str(&caps[2]).unwrap(),
                    i32::from_str(&caps[3]).unwrap(),
                    i32::from_str(&caps[4]).unwrap(),
                )
            })
            .collect();

        let wk_re = Regex::new(r":([1-7]+)").unwrap();
        let wks = match wk_re.captures(&sstr) {
            Some(caps) => Session::parse_wk(&caps[1])?,
            _ => Self::def_weekdays(),
        };
        Ok(Session {
            spans,
            weekdays: wks,
        })
    }

    pub fn is_in(&self, millseconds: i64, tz: &Tz) -> bool {
        let dt = tz.timestamp(millseconds / 1000, 0);
        let mut wk = dt.date().weekday();
        let time = dt.time();
        let mut day_time = DayTime::new(time.hour() as i32, time.minute() as i32);
        // Find in the spans
        let select_span = self.spans.iter().find(|timespan| {
            let mut between = day_time >= timespan.start && day_time < timespan.end;

            if !between && timespan.start.is_negative() {
                let pre_day = day_time.sub_hour(24);
                if pre_day >= timespan.start && pre_day < timespan.end {
                    day_time = pre_day; // This day is belongs to next day.
                    wk = wk.succ();
                    between = true;
                }
            }
            between
        });
        if select_span.is_none() {
            return false;
        }
        match self.weekdays.iter().find(|&&d| d == wk) {
            Some(_) => true,
            None => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn session_test() {
        assert_eq!(
            Session::parse(&String::from("0000-0000")),
            Ok(Session {
                spans: vec![TradeTimeSpan::new(0, 0, 24, 0)],
                weekdays: Session::def_weekdays(),
            })
        );
        assert_eq!(
            Session::parse(&String::from("0900-1600,1700-2000")),
            Ok(Session {
                spans: vec![
                    TradeTimeSpan::new(9, 0, 16, 0),
                    TradeTimeSpan::new(17, 0, 20, 0)
                ],
                weekdays: Session::def_weekdays(),
            })
        );

        assert_eq!(
            Session::parse(&String::from("2000-1630:1234567")),
            Ok(Session {
                spans: vec![TradeTimeSpan::new(-4, 0, 16, 30),],
                weekdays: Session::all_weekdays(),
            })
        );

        assert_eq!(
            Session::parse(&String::from("0930-1700:146")),
            Ok(Session {
                spans: vec![TradeTimeSpan::new(9, 30, 17, 0),],
                weekdays: vec![Weekday::Sun, Weekday::Wed, Weekday::Fri],
            })
        );

        assert_eq!(
            Session::parse(&String::from("24x7")),
            Ok(Session {
                spans: vec![TradeTimeSpan::new(0, 0, 24, 0),],
                weekdays: Session::all_weekdays(),
            })
        );

        assert_eq!(
            Session::parse(&String::from("0000-0000:1234567")),
            Ok(Session {
                spans: vec![TradeTimeSpan::new(0, 0, 24, 0),],
                weekdays: Session::all_weekdays(),
            })
        );

        assert_eq!(
            Session::parse(&String::from("0000-0000:23456")),
            Ok(Session {
                spans: vec![TradeTimeSpan::new(0, 0, 24, 0),],
                weekdays: Session::def_weekdays(),
            })
        );

        assert_eq!(
            Session::parse(&String::from("1700-1700:23456")),
            Ok(Session {
                spans: vec![TradeTimeSpan::new(-7, 0, 17, 0),],
                weekdays: Session::def_weekdays(),
            })
        );

        assert_eq!(
            Session::parse(&String::from("1000-1001:26")),
            Ok(Session {
                spans: vec![TradeTimeSpan::new(10, 0, 10, 01),],
                weekdays: vec![Weekday::Mon, Weekday::Fri],
            })
        );
    }

    #[test]
    fn is_in_test() {
        let tz = Tz::UTC;
        let ts = tz.ymd(2020, 2, 14).and_hms(9, 48, 0).timestamp() * 1000;
        assert_eq!(
            Session::parse(&String::from("0100-1000"))
                .unwrap()
                .is_in(ts, &tz),
            true
        );

        assert_eq!(
            Session::parse(&String::from("0900-0100"))
                .unwrap()
                .is_in(ts, &tz),
            false
        );
        assert_eq!(
            Session::parse(&String::from("0900-0100")).unwrap().is_in(
                tz.ymd(2020, 2, 13).and_hms(9, 48, 0).timestamp() * 1000,
                &tz
            ),
            true
        );
    }
}

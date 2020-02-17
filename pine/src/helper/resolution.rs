use super::err_msgs::*;
use super::str_replace::*;
use crate::types::RuntimeErr;
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
}

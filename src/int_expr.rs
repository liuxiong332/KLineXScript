use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::digit1,
    combinator::{map, opt},
    error::{ErrorKind, ParseError},
    multi::separated_list,
    Err, IResult,
};

use crate::error::PineError;

struct _IntExpr {
    val: i32,
}

pub fn underscore_digit_str(s: &str) -> IResult<&str, String, PineError<&str>> {
    map(separated_list(tag("_"), digit1), |s| s.join(""))(s)
}

pub fn unsigned_int(input: &str) -> IResult<&str, i32, PineError<&str>> {
    let (next_s, num_str) = underscore_digit_str(input).unwrap();
    if let Ok(num) = i32::from_str_radix(&num_str, 10) {
        Ok((next_s, num))
    } else {
        Err(Err::Error(PineError::from_error_kind(
            input,
            ErrorKind::Digit,
        )))
    }
}

pub fn signed_int(s: &str) -> IResult<&str, i32, PineError<&str>> {
    let (s, sign) = opt(alt((tag("+"), tag("-"))))(s)?;
    let (s, num_int) = unsigned_int(s)?;
    match sign {
        Some("+") | None => Ok((s, num_int)),
        Some("-") => Ok((s, -num_int)),
        _ => panic!("internal error: entered unreachable code"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn underscore_digit_str_test() {
        assert_eq!(
            underscore_digit_str("1221_121"),
            Ok(("", String::from("1221121")))
        );

        assert_eq!(
            underscore_digit_str("12_23_34_sss"),
            Ok(("_sss", String::from("122334")))
        );

        assert_eq!(
            underscore_digit_str("1221121"),
            Ok(("", String::from("1221121")))
        );
    }

    #[test]
    fn underscore_digit_test() {
        assert_eq!(unsigned_int("1221_121"), Ok(("", 1221121)));
        assert_eq!(
            unsigned_int(""),
            Err(Err::Error(PineError::from_error_kind("", ErrorKind::Digit)))
        );
    }

    #[test]
    fn signed_int_test() {
        assert_eq!(signed_int("-1221_121"), Ok(("", -1221121)));
        assert_eq!(signed_int("+1221_121"), Ok(("", 1221121)));
        assert_eq!(signed_int("1221_121"), Ok(("", 1221121)));
    }
}

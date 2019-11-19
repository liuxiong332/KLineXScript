use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::digit1,
    combinator::{map, opt, recognize},
    multi::separated_list,
    sequence::{preceded, tuple},
    Err,
};
use std::str::FromStr;

use crate::error::{PineError, PineErrorKind, PineResult};
use crate::utils::skip_ws;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Numeral {
    Float(f64),
    Int(i32),
}

pub fn underscore_digit_str(s: &str) -> PineResult<String> {
    map(separated_list(tag("_"), digit1), |s| s.join(""))(s)
}

// pub fn signed_int(s: &str) -> PineResult<i32> {
//     let (s, sign) = opt(alt((tag("+"), tag("-"))))(s)?;
//     let (s, _) = space0(s)?;
//     let (s, num_int) = decimal(s)?;
//     match sign {
//         Some("+") | None => Ok((s, num_int)),
//         Some("-") => Ok((s, -num_int)),
//         _ => panic!("internal error: entered unreachable code"),
//     }
// }

pub fn decimal(input: &str) -> PineResult<i32> {
    let (next_s, num_str) = underscore_digit_str(input).unwrap();
    // let (next_s, num_str) = digit1(input)?;
    match i32::from_str_radix(&num_str, 10) {
        Ok(num) => Ok((next_s, num)),
        _ => Err(Err::Error(PineError::from_pine_kind(
            input,
            PineErrorKind::InvalidDecimal,
        ))),
    }
}

pub fn float_mag(input: &str) -> PineResult<i32> {
    preceded(alt((tag("e"), tag("E"))), float_sgn_suffix)(input)
}

pub fn float_sgn_suffix(input: &str) -> PineResult<i32> {
    let (input, sign) = opt(alt((tag("+"), tag("-"))))(input)?;
    let (input, num) = decimal(input)?;
    match sign {
        Some("+") | None => Ok((input, num)),
        Some("-") => Ok((input, -num)),
        _ => Ok((input, 0)),
    }
}

pub fn num_lit(input: &str) -> PineResult<Numeral> {
    let (input, out) = recognize(tuple((
        opt(decimal),
        opt(preceded(tag("."), decimal)),
        opt(float_mag),
    )))(input)?;
    if let Ok(n) = i32::from_str_radix(out, 10) {
        Ok((input, Numeral::Int(n)))
    } else if let Ok(f) = f64::from_str(out) {
        Ok((input, Numeral::Float(f)))
    } else {
        Err(Err::Error(PineError::from_pine_kind(
            input,
            PineErrorKind::InvalidDecimal,
        )))
    }
}

// match float or int. e.g. 2.12 2.12e121 .111e11  1221
pub fn num_lit_ws(input: &str) -> PineResult<Numeral> {
    let (input, _) = skip_ws(input)?;
    num_lit(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn signed_int_test() {
        assert_eq!(num_lit_ws("121.1"), Ok(("", Numeral::Float(121.1))));
        assert_eq!(num_lit_ws("121"), Ok(("", Numeral::Int(121))));
        assert_eq!(num_lit_ws("121e1"), Ok(("", Numeral::Float(121e1))));
        assert_eq!(num_lit_ws("121.1e1"), Ok(("", Numeral::Float(121.1e1))));
    }
}

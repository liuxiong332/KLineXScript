use super::input::{Input, Position, StrRange};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::digit1,
    combinator::{map, opt, recognize},
    multi::separated_list,
    sequence::{preceded, terminated, tuple},
    Err,
};
use std::str::FromStr;

use super::error::{PineError, PineErrorKind, PineResult};
use super::utils::skip_ws;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct IntNode {
    pub value: i64,
    pub range: StrRange,
}

impl IntNode {
    pub fn new(value: i64, range: StrRange) -> IntNode {
        IntNode { value, range }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct FloatNode {
    pub value: f64,
    pub range: StrRange,
}

impl FloatNode {
    pub fn new(value: f64, range: StrRange) -> FloatNode {
        FloatNode { value, range }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Numeral {
    Float(FloatNode),
    Int(IntNode),
}

impl Numeral {
    pub fn from_i64(val: i64) -> Numeral {
        Numeral::Int(IntNode::new(
            val,
            StrRange::new(Position::new(0, 0), Position::max()),
        ))
    }

    pub fn from_f64(val: f64) -> Numeral {
        Numeral::Float(FloatNode::new(
            val,
            StrRange::new(Position::new(0, 0), Position::max()),
        ))
    }

    pub fn range(&self) -> StrRange {
        match self {
            Numeral::Float(node) => node.range,
            Numeral::Int(node) => node.range,
        }
    }
}

pub fn underscore_digit_str(s: Input) -> PineResult<String> {
    map(separated_list(tag("_"), digit1), |v: Vec<Input>| {
        v.into_iter().map(|s| s.src).collect::<Vec<&str>>().join("")
    })(s)
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

pub fn decimal(input: Input) -> PineResult<i64> {
    let (next_s, num_str) = underscore_digit_str(input).unwrap();
    // let (next_s, num_str) = digit1(input)?;
    match i64::from_str_radix(&num_str, 10) {
        Ok(num) => Ok((next_s, num)),
        _ => Err(Err::Error(PineError::from_pine_kind(
            input,
            PineErrorKind::InvalidDecimal,
        ))),
    }
}

pub fn int_lit(input: Input) -> PineResult<i64> {
    let (input, (sign, lit)) =
        tuple((opt(terminated(alt((tag("+"), tag("-"))), skip_ws)), decimal))(input)?;
    match sign {
        Some(Input { src: "+", .. }) | None => Ok((input, lit)),
        Some(Input { src: "-", .. }) => Ok((input, -lit)),
        _ => unreachable!(),
    }
}

pub fn int_lit_ws(input: Input) -> PineResult<i64> {
    let (input, _) = skip_ws(input)?;
    int_lit(input)
}

pub fn float_mag(input: Input) -> PineResult<i64> {
    preceded(alt((tag("e"), tag("E"))), float_sgn_suffix)(input)
}

pub fn float_sgn_suffix(input: Input) -> PineResult<i64> {
    let (input, sign) = opt(alt((tag("+"), tag("-"))))(input)?;
    let (input, num) = decimal(input)?;
    match sign {
        Some(Input { src: "+", .. }) | None => Ok((input, num)),
        Some(Input { src: "-", .. }) => Ok((input, -num)),
        _ => Ok((input, 0)),
    }
}

pub fn num_lit(input: Input) -> PineResult<Numeral> {
    let (input, out) = recognize(tuple((
        opt(decimal),
        opt(preceded(tag("."), decimal)),
        opt(float_mag),
    )))(input)?;
    if let Ok(n) = i64::from_str_radix(out.src, 10) {
        Ok((
            input,
            Numeral::Int(IntNode::new(n, StrRange::from_input(&out))),
        ))
    } else if let Ok(f) = f64::from_str(out.src) {
        Ok((
            input,
            Numeral::Float(FloatNode::new(f, StrRange::from_input(&out))),
        ))
    } else {
        Err(Err::Error(PineError::from_pine_kind(
            input,
            PineErrorKind::InvalidDecimal,
        )))
    }
}

// match float or int. e.g. 2.12 2.12e121 .111e11  1221
pub fn num_lit_ws(input: Input) -> PineResult<Numeral> {
    let (input, _) = skip_ws(input)?;
    num_lit(input)
}

#[cfg(test)]
mod tests {
    use super::super::input::Position;
    use super::*;
    use std::convert::TryInto;

    #[test]
    fn signed_int_test() {
        fn test_lit_ws(s: &str, res: f64) {
            let test_input = Input::new_with_str(s);
            let input_len: u32 = test_input.len().try_into().unwrap();
            assert_eq!(
                num_lit_ws(test_input),
                Ok((
                    Input::new("", Position::new(0, input_len), Position::max()),
                    Numeral::Float(FloatNode::new(
                        res,
                        StrRange::new(Position::new(0, 0), Position::new(0, input_len))
                    ))
                ))
            );
        }

        test_lit_ws("121.1", 121.1);
        test_lit_ws("121e1", 121e1);
        test_lit_ws("121.1e1", 121.1e1);
    }

    #[test]
    fn int_test() {
        fn test_int_ws(s: &str, res: i64) {
            let test_input = Input::new_with_str(s);
            let input_len: u32 = test_input.len().try_into().unwrap();
            assert_eq!(
                int_lit_ws(test_input),
                Ok((
                    Input::new("", Position::new(0, input_len), Position::max()),
                    res
                ))
            );
        }

        test_int_ws("121", 121);
        test_int_ws(" + 121", 121i64);
        test_int_ws(" - 121", -121i64);
        test_int_ws(" 121", 121);
    }
}

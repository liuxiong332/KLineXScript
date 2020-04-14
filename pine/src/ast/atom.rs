use super::error::{PineError, PineErrorKind, PineResult};
use super::input::{Input, Position, StrRange};
use super::name::{alpha_or_underscore, is_alphanum_or_underscore};
use super::state::{AstState, PineInputError};
use super::utils::skip_ws;
use nom::Err;
use nom::{bytes::complete::take_while, combinator::recognize, sequence::pair};

pub fn atom_lit<'a>(input: Input<'a>, _state: &AstState) -> PineResult<'a> {
    let (input, _) = skip_ws(input)?;
    recognize(pair(
        alpha_or_underscore,
        take_while(is_alphanum_or_underscore),
    ))(input)
}

pub fn atom_val<'a>(val: &'a str) -> impl Fn(Input<'a>) -> PineResult<'a> {
    move |input: Input<'a>| {
        let (input, out) = recognize(pair(
            alpha_or_underscore,
            take_while(is_alphanum_or_underscore),
        ))(input)?;
        match out.src {
            v if v == val => Ok((input, out)),
            _ => Err(Err::Error(PineError::from_pine_kind(
                input,
                PineErrorKind::Context("Not match value"),
            ))),
        }
    }
}

pub fn atom_vals<'a>(vals: &'a [&'a str]) -> impl Fn(Input<'a>) -> PineResult<'a> {
    move |input: Input<'a>| {
        let (input, out) = recognize(pair(
            alpha_or_underscore,
            take_while(is_alphanum_or_underscore),
        ))(input)?;
        match vals.iter().find(|&s| *s == out.src) {
            Some(_) => Ok((input, out)),
            None => Err(Err::Error(PineError::from_pine_kind(
                input,
                PineErrorKind::Context("Not match value"),
            ))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::input::Position;
    use super::*;

    #[test]
    fn atom_lit_test() {
        assert_eq!(
            atom_lit(Input::new_with_str(" a23456 d"), &AstState::new()),
            Ok((
                Input::new(" d", Position::new(0, 7), Position::max()),
                Input::new("a23456", Position::new(0, 1), Position::new(0, 7))
            ))
        );
    }

    #[test]
    fn atom_val_test() {
        assert_eq!(
            atom_val("if")(Input::new_with_str("if else")),
            Ok((
                Input::new(" else", Position::new(0, 2), Position::max()),
                Input::new("if", Position::new(0, 0), Position::new(0, 2))
            ))
        );
        assert!(atom_val("if")(Input::new_with_str("ifelse")).is_err());
    }

    #[test]
    fn atom_vals_test() {
        assert_eq!(
            atom_vals(&["if", "else"])(Input::new_with_str("if else")),
            Ok((
                Input::new(" else", Position::new(0, 2), Position::max()),
                Input::new("if", Position::new(0, 0), Position::new(0, 2))
            ))
        );
        assert_eq!(
            atom_vals(&["if", "else"])(Input::new_with_str("else")),
            Ok((
                Input::new("", Position::new(0, 4), Position::max()),
                Input::new("else", Position::new(0, 0), Position::new(0, 4))
            ))
        );
        assert!(atom_vals(&["if", "else"])(Input::new_with_str("ifelse")).is_err());
    }
}

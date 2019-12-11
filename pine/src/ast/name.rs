use super::error::{PineError, PineErrorKind, PineResult};
use super::input::Input;
use super::utils::skip_ws;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    combinator::recognize,
    sequence::pair,
    Err, InputTake,
};

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct VarName<'a> {
    pub value: &'a str,
    pub input: Input<'a>,
}

impl<'a> VarName<'a> {
    pub fn new(value: &'a str, input: Input<'a>) -> VarName<'a> {
        VarName { value, input }
    }

    pub fn new_no_input(value: &'a str) -> VarName<'a> {
        VarName {
            value,
            input: Input::new_empty(),
        }
    }
}

fn reserved(input: Input) -> PineResult {
    alt((
        tag("and"),
        tag("or"),
        tag("not"),
        tag("break"),
        tag("continue"),
        tag("else"),
        tag("if"),
        tag("true"),
        tag("false"),
        tag("for"),
        tag("while"),
        tag("return"),
        tag("na"),
    ))(input)
}

fn alpha_or_underscore(input: Input) -> PineResult {
    match input.src.chars().next().map(|t: char| {
        let b = t.is_alphabetic() || t == '_';
        (t, b)
    }) {
        Some((_, true)) => Ok(input.take_split(1)),
        _ => Err(Err::Error(PineError::from_pine_kind(
            input,
            PineErrorKind::InvalidIdentifier("The identifier must start with alphabetic or _"),
        ))),
    }
    // alt((alpha0, tag("_")))(input)
}

fn is_alphanum_or_underscore(input: char) -> bool {
    input.is_alphanumeric() || input == '_'
}

pub fn varname(input: Input) -> PineResult<VarName> {
    let (input, name) = recognize(pair(
        alpha_or_underscore,
        take_while(is_alphanum_or_underscore),
    ))(input)?;
    if let Ok((rest, _)) = reserved(name) {
        if rest.len() == 0 {
            return Err(Err::Error(PineError::from_pine_kind(
                input,
                PineErrorKind::ReservedVarName,
            )));
        }
    }
    Ok((input, VarName::new(name.src, name)))
}

pub fn varname_ws(input: Input) -> PineResult<VarName> {
    let (input, _) = skip_ws(input)?;
    varname(input)
}

#[cfg(test)]
mod tests {
    use super::super::input::Position;
    use super::*;
    use std::convert::TryInto;

    #[test]
    fn name_test() {
        let test_input = Input::new_with_str("na");
        assert_eq!(
            reserved(test_input),
            Ok(test_input.take_split(test_input.len()))
        );

        let test_input = Input::new_with_str("_hello");
        assert_eq!(
            alpha_or_underscore(test_input),
            Ok((
                Input::new("hello", Position::new(0, 1), Position::max()),
                Input::new_u32("_", 0, 0, 0, 1)
            ))
        );

        assert_eq!(
            alpha_or_underscore(Input::new_with_str("hello")),
            Ok((
                Input::new("ello", Position::new(0, 1), Position::max()),
                Input::new_u32("h", 0, 0, 0, 1)
            ))
        );
        assert!(alpha_or_underscore(Input::new_with_str("2hello")).is_err());

        fn test_varname(s: &str, res: &str, col: u32) {
            let test_input = Input::new_with_str(s);
            let input_len: u32 = test_input.len().try_into().unwrap();
            assert_eq!(
                varname_ws(test_input),
                Ok((
                    Input::new("", Position::new(0, input_len), Position::max()),
                    VarName::new(res, Input::new_with_start(res, Position::new(0, col)))
                ))
            );
        }

        test_varname(" hello_world", "hello_world", 1);
        test_varname(" hed_12s", "hed_12s", 1);
        test_varname("myVar", "myVar", 0);
        test_varname("_myVar", "_myVar", 0);
        test_varname("my123Var", "my123Var", 0);
        test_varname("MAX_LEN", "MAX_LEN", 0);
        test_varname("max_len", "max_len", 0);
    }
}
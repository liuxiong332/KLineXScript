use super::error::PineResult;
use super::input::Input;
use super::utils::input_end;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_while};
use nom::combinator::{not, recognize};
use nom::sequence::tuple;

// The comment is like this //... until the end of this line.
pub fn comment(input: Input) -> PineResult {
    let (input, out) = recognize(tuple((
        tag("//"),
        is_not("\n"),
        alt((tag("\n"), input_end)),
    )))(input)?;
    Ok((input, out))
}

#[cfg(test)]
mod tests {
    use super::super::input::Position;
    use super::*;
    use crate::ast::error::PineError;
    use nom::{
        error::{ErrorKind, ParseError},
        Err,
    };
    use std::convert::TryInto;

    #[test]
    fn comment_test() {
        let s = Input::new_with_str("//hello world\nwode");
        assert_eq!(
            comment(s),
            Ok((
                Input::new("wode", Position::new(1, 0), Position::max()),
                Input::new_u32("//hello world\n", 0, 0, 1, 0)
            ))
        );
        assert_eq!(
            comment(Input::new_with_str("hello world")),
            Err(Err::Error(PineError::from_error_kind(
                Input::new_with_str("hello world"),
                ErrorKind::Tag
            )))
        );
        assert_eq!(
            comment(Input::new_with_str("//hello world")),
            Ok((
                Input::new("", Position::new(0, 13), Position::max()),
                Input::new_u32("//hello world", 0, 0, 0, 13)
            ))
        );
    }
}

use super::error::PineResult;
use super::input::Input;
use nom::bytes::complete::{tag, take_until};

// The comment is like this //... until the end of this line.
pub fn comment(input: Input) -> PineResult {
    let (_, _) = tag("//")(input)?;
    let (input, out) = take_until("\n")(input)?;
    Ok((input.forward(1), out))
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
        let content_len: u32 = "//hello world\n".len().try_into().unwrap();
        assert_eq!(
            comment(s),
            Ok((
                Input::new("wode", Position::new(1, 0), Position::max()),
                Input::new_u32("//hello world", 0, 0, 0, content_len - 1)
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
            Err(Err::Error(PineError::from_error_kind(
                Input::new_with_str("//hello world"),
                ErrorKind::TakeUntil
            )))
        );
    }
}

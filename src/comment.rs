use crate::error::{PineError, PineResult};
use nom::{
    bytes::complete::{tag, take_until},
    error::{ErrorKind, ParseError, VerboseError},
    Err, IResult,
};

// The comment is like this //... until the end of this line.
pub fn comment(input: &str) -> PineResult {
    let (_, _) = tag("//")(input)?;
    let (input, out) = take_until("\n")(input)?;
    Ok((&input[1..], out))
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn comment_test() {
        assert_eq!(
            comment("//hello world\nwode"),
            Ok(("wode", "//hello world"))
        );
        assert_eq!(
            comment("hello world"),
            Err(Err::Error(PineError::from_error_kind(
                "hello world",
                ErrorKind::Tag
            )))
        );
        assert_eq!(
            comment("//hello world"),
            Err(Err::Error(PineError::from_error_kind(
                "//hello world",
                ErrorKind::TakeUntil
            )))
        );
    }
}
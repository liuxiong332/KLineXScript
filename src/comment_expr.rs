use nom::{
    bytes::complete::{tag, take_until},
    error::{VerboseError, Error, ErrorKind},
    Err,
    IResult,
};

// The comment is like this //... until the end of this line.
fn comment(input: &str) -> IResult<&str, (), VerboseError<&str>> {
    let (input, _) = tag("//")(input)?;
    let (input, _) = take_until("\n")(input)?;
    (&input[1..], ())
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn comment_test() {
        assert_eq!(comment("//hello world\nwode"), Ok(("wode")));
        assert_eq!(comment("hello world"), Err(Err::Error(Error::from_error_kind("hello world", ErrorKind::Tag)))));
        assert_eq!(comment("//hello world"), Err(Err::Error(Error::from_error_kind("hello world", ErrorKind::TakeUntil)))));
    }
}

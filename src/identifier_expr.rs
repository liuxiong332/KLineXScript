use nom::{
    bytes::complete::{tag, take_till},
    error::{ErrorKind, ParseError},
    Err, IResult,
};

use crate::error::{PineError, PineErrorKind};

#[derive(Debug, PartialEq)]
struct IdentifierExpr<'a>(&'a str);

fn check_is_id_char(c: char) -> bool {
    match c {
        'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => false,
        _ => true,
    }
}

// The comment is like this //... until the end of this line.
fn identifier_expr(input: &str) -> IResult<&str, IdentifierExpr, PineError<&str>> {
    // The first character must be '_' or A-Z, a-z
    if let Some(first_char) = input.chars().next() {
        match first_char {
            'a'..='z' | 'A'..='Z' | '_' => (),
            _ => {
                return Err(Err::Error(PineError::from_pine_kind(
                    input,
                    PineErrorKind::InvalidIdentifier("The identifier must start with _ or alpha."),
                )))
            }
        }
        let (input, out) = take_till(check_is_id_char)(input)?;
        // let out = input
        //     .chars()
        //     .take_while(check_is_id_char)
        //     .collect()
        //     .join("");
        Ok((input, IdentifierExpr(out)))
    } else {
        Err(Err::Error(ParseError::from_error_kind(
            input,
            ErrorKind::Not,
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn comment_test() {
        assert_eq!(
            identifier_expr("_hello"),
            Ok(("", IdentifierExpr("_hello")))
        );
        assert_eq!(
            identifier_expr("hello11"),
            Ok(("", IdentifierExpr("hello11")))
        );
        assert!(identifier_expr("2hello11").is_err());
    }
}

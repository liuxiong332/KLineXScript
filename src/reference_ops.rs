use nom::{
    bytes::complete::{tag, take_till},
    character::complete::space0,
    combinator::opt,
    error::{ErrorKind, ParseError},
    sequence::{delimited, preceded, terminated},
    Err, IResult,
};

use crate::error::{PineError, PineErrorKind};
use crate::int_expr::signed_int;

#[derive(Debug, PartialEq)]
struct IdentifierExpr<'a>(&'a str);

// The comment is like this //... until the end of this line.
fn reference_ops(input: &str) -> IResult<&str, i32, PineError<&str>> {
    delimited(
        terminated(tag("["), space0),
        signed_int,
        preceded(space0, tag("]")),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn comment_test() {
        assert_eq!(reference_ops("[122]"), Ok(("", 122)));
        assert_eq!(reference_ops("[  122  ]"), Ok(("", 122)));
    }
}

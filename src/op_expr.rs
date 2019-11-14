use nom::{
    bytes::complete::tag,
    character::complete::{space0, space1},
    sequence::{delimited, preceded, terminated, tuple},
    IResult,
};

use crate::error::PineError;
use crate::int_expr::{int_expr, IntExpr};

#[derive(Debug, PartialEq)]
pub struct UnaryLogicNot(IntExpr);

#[derive(Debug, PartialEq)]
pub struct UnaryNegative(IntExpr);

fn not_ops(input: &str) -> IResult<&str, UnaryLogicNot, PineError<&str>> {
    let (input, (_, _, expr)) = tuple((tag("not"), space1, int_expr))(input)?;
    Ok((input, UnaryLogicNot(expr)))
}

fn positive_ops(input: &str) -> IResult<&str, IntExpr, PineError<&str>> {
    let (input, (_, _, expr)) = tuple((tag("+"), space0, int_expr))(input)?;
    Ok((input, expr))
}

fn negative_ops(input: &str) -> IResult<&str, UnaryNegative, PineError<&str>> {
    let (input, (_, _, expr)) = tuple((tag("-"), space0, int_expr))(input)?;
    Ok((input, UnaryNegative(expr)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::int_expr::IntExpr;

    #[test]
    fn comment_test() {
        assert_eq!(not_ops("not 123"), Ok(("", UnaryLogicNot(IntExpr(123)))));
    }
}

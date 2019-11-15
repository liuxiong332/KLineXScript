use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map, value},
    multi::{many0, separated_list},
    sequence::{delimited, tuple},
};

use crate::color::color_lit;
use crate::error::PineResult;
use crate::name::{varname, VarName};
use crate::op::*;
use crate::stat_expr_types::*;
use crate::string::string_lit;
use crate::trans::flatexp_from_components;
use crate::utils::eat_sep;

pub fn exp2(input: &str) -> PineResult<Exp2> {
    alt((
        value(Exp2::Na, eat_sep(tag("na"))),
        value(Exp2::Bool(true), eat_sep(tag("true"))),
        value(Exp2::Bool(false), eat_sep(tag("false"))),
        map(string_lit, Exp2::Str),
        map(color_lit, Exp2::Color),
        map(varname, Exp2::VarName),
        map(rettupledef, |varnames| Exp2::RetTuple(Box::new(varnames))),
    ))(input)
}

pub fn unopexp2(input: &str) -> PineResult<(Vec<UnaryOp>, Exp2)> {
    tuple((many0(unary_op), exp2))(input)
}

pub fn flatexp(input: &str) -> PineResult<FlatExp> {
    let (input, head) = unopexp2(input)?;
    let (input, binop_chain) = many0(tuple((binary_op, unopexp2)))(input)?;
    Ok((input, flatexp_from_components(head, binop_chain)))
}

// The left return tuple of expression `[a, b] = [1, 2]` that contain variable name between square brackets
fn rettupledef(input: &str) -> PineResult<Vec<VarName>> {
    eat_sep(delimited(
        eat_sep(tag("[")),
        separated_list(eat_sep(tag(",")), varname),
        eat_sep(tag("]")),
    ))(input)
}

// The right tuple of expression `[a, b] = [1, 2]` that contain expressions splited by dot between square brackets
// fn tupledef(input: &str) -> PineResult<Vec<Exp>> {
//     eat_sep(delimited(
//         eat_sep(tag("[")),
//         separated_list(eat_sep(tag(",")), varname),
//         eat_sep(tag("]")),
//     ))(input)
// }

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn tupledef_test() {
        assert_eq!(
            rettupledef(" [hello, good]"),
            Ok(("", vec![VarName("hello"), VarName("good")]))
        );
        assert_eq!(
            rettupledef(" [hello, good,  my]hello"),
            Ok((
                "hello",
                vec![VarName("hello"), VarName("good"), VarName("my")]
            ))
        );

        assert_eq!(
            rettupledef(" [ hello  , good ]"),
            Ok(("", vec![VarName("hello"), VarName("good")]))
        );
    }
}

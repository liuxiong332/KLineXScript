use super::error::PineResult;
use super::utils::skip_ws;
use nom::{branch::alt, bytes::complete::tag, combinator::value};

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum BinaryOp {
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    Lt,
    Leq,
    Gt,
    Geq,
    Eq,
    Neq,
    BoolAnd,
    BoolOr,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum UnaryOp {
    Plus,
    Minus,
    BoolNot,
}

pub fn unary_op(input: &str) -> PineResult<UnaryOp> {
    let (input, _) = skip_ws(input)?;
    alt((
        value(UnaryOp::Plus, tag("+")),
        value(UnaryOp::Minus, tag("-")),
        value(UnaryOp::BoolNot, tag("not")),
    ))(input)
}

pub fn binary_op(input: &str) -> PineResult<BinaryOp> {
    let (input, _) = skip_ws(input)?;
    alt((
        value(BinaryOp::Plus, tag("+")),
        value(BinaryOp::Minus, tag("-")),
        value(BinaryOp::Mul, tag("*")),
        value(BinaryOp::Div, tag("/")),
        value(BinaryOp::Mod, tag("%")),
        value(BinaryOp::Leq, tag("<=")),
        value(BinaryOp::Geq, tag(">=")),
        value(BinaryOp::Lt, tag("<")),
        value(BinaryOp::Gt, tag(">")),
        value(BinaryOp::Eq, tag("==")),
        value(BinaryOp::Neq, tag("!=")),
        value(BinaryOp::BoolAnd, tag("and")),
        value(BinaryOp::BoolOr, tag("or")),
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn binary_op_test() {
        assert_eq!(binary_op(" +"), Ok(("", BinaryOp::Plus)));
        assert_eq!(binary_op(" -"), Ok(("", BinaryOp::Minus)));
        assert_eq!(binary_op(" *"), Ok(("", BinaryOp::Mul)));
        assert_eq!(binary_op(" /"), Ok(("", BinaryOp::Div)));
        assert_eq!(binary_op(" %"), Ok(("", BinaryOp::Mod)));
        assert_eq!(binary_op(" >"), Ok(("", BinaryOp::Gt)));
        assert_eq!(binary_op(" >="), Ok(("", BinaryOp::Geq)));
        assert_eq!(binary_op(" <"), Ok(("", BinaryOp::Lt)));
        assert_eq!(binary_op(" <="), Ok(("", BinaryOp::Leq)));
        assert_eq!(binary_op(" =="), Ok(("", BinaryOp::Eq)));
        assert_eq!(binary_op(" !="), Ok(("", BinaryOp::Neq)));
        assert_eq!(binary_op(" and"), Ok(("", BinaryOp::BoolAnd)));
        assert_eq!(binary_op(" or"), Ok(("", BinaryOp::BoolOr)));
    }

    #[test]
    fn unary_op_test() {
        assert_eq!(unary_op(" +"), Ok(("", UnaryOp::Plus)));
        assert_eq!(unary_op(" -"), Ok(("", UnaryOp::Minus)));
        assert_eq!(unary_op(" not"), Ok(("", UnaryOp::BoolNot)));
    }
}

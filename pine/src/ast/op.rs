use super::error::PineResult;
use super::input::Input;
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

pub fn unary_op(input: Input) -> PineResult<UnaryOp> {
    let (input, _) = skip_ws(input)?;
    alt((
        value(UnaryOp::Plus, tag("+")),
        value(UnaryOp::Minus, tag("-")),
        value(UnaryOp::BoolNot, tag("not")),
    ))(input)
}

pub fn binary_op(input: Input) -> PineResult<BinaryOp> {
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
    use super::super::input::Position;
    use super::*;
    use std::convert::TryInto;

    fn test_op(s: &str, op: BinaryOp) {
        let test_input = Input::new_with_str(s);
        let input_len: u32 = test_input.len().try_into().unwrap();
        assert_eq!(
            binary_op(test_input),
            Ok((
                Input::new("", Position::new(0, input_len), Position::max()),
                op
            ))
        );
    }

    #[test]
    fn binary_op_test() {
        test_op(" +", BinaryOp::Plus);
        test_op(" -", BinaryOp::Minus);
        test_op(" *", BinaryOp::Mul);
        test_op(" /", BinaryOp::Div);
        test_op(" %", BinaryOp::Mod);
        test_op(" >", BinaryOp::Gt);
        test_op(" >=", BinaryOp::Geq);
        test_op(" <", BinaryOp::Lt);
        test_op(" <=", BinaryOp::Leq);
        test_op(" ==", BinaryOp::Eq);
        test_op(" !=", BinaryOp::Neq);
        test_op(" and", BinaryOp::BoolAnd);
        test_op(" or", BinaryOp::BoolOr);
    }

    fn test_unary_op(s: &str, op: UnaryOp) {
        let test_input = Input::new_with_str(s);
        let input_len: u32 = test_input.len().try_into().unwrap();
        assert_eq!(
            unary_op(test_input),
            Ok((
                Input::new("", Position::new(0, input_len), Position::max()),
                op
            ))
        );
    }

    #[test]
    fn unary_op_test() {
        test_unary_op(" +", UnaryOp::Plus);
        test_unary_op(" -", UnaryOp::Minus);
        test_unary_op(" not", UnaryOp::BoolNot);
    }
}

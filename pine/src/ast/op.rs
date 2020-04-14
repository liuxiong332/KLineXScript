use super::atom::atom_val;
use super::error::PineResult;
use super::input::{Input, StrRange};
use super::utils::skip_ws;
use nom::{branch::alt, bytes::complete::tag, combinator::map};

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

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOpNode {
    pub op: BinaryOp,
    pub range: StrRange,
}

impl BinaryOpNode {
    pub fn new(op: BinaryOp, range: StrRange) -> BinaryOpNode {
        BinaryOpNode { op, range }
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum UnaryOp {
    Plus,
    Minus,
    BoolNot,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOpNode {
    pub op: UnaryOp,
    pub range: StrRange,
}

impl UnaryOpNode {
    pub fn new(op: UnaryOp, range: StrRange) -> UnaryOpNode {
        UnaryOpNode { op, range }
    }
}

pub fn unary_op(input: Input) -> PineResult<UnaryOpNode> {
    let (input, _) = skip_ws(input)?;
    alt((
        map(tag("+"), |s| {
            UnaryOpNode::new(UnaryOp::Plus, StrRange::from_input(&s))
        }),
        map(tag("-"), |s| {
            UnaryOpNode::new(UnaryOp::Minus, StrRange::from_input(&s))
        }),
        map(atom_val("not"), |s| {
            UnaryOpNode::new(UnaryOp::BoolNot, StrRange::from_input(&s))
        }),
    ))(input)
}

pub fn binary_op(input: Input) -> PineResult<BinaryOpNode> {
    let (input, _) = skip_ws(input)?;
    alt((
        map(tag("+"), |s| {
            BinaryOpNode::new(BinaryOp::Plus, StrRange::from_input(&s))
        }),
        map(tag("-"), |s| {
            BinaryOpNode::new(BinaryOp::Minus, StrRange::from_input(&s))
        }),
        map(tag("*"), |s| {
            BinaryOpNode::new(BinaryOp::Mul, StrRange::from_input(&s))
        }),
        map(tag("/"), |s| {
            BinaryOpNode::new(BinaryOp::Div, StrRange::from_input(&s))
        }),
        map(tag("%"), |s| {
            BinaryOpNode::new(BinaryOp::Mod, StrRange::from_input(&s))
        }),
        map(tag("<="), |s| {
            BinaryOpNode::new(BinaryOp::Leq, StrRange::from_input(&s))
        }),
        map(tag(">="), |s| {
            BinaryOpNode::new(BinaryOp::Geq, StrRange::from_input(&s))
        }),
        map(tag("<"), |s| {
            BinaryOpNode::new(BinaryOp::Lt, StrRange::from_input(&s))
        }),
        map(tag(">"), |s| {
            BinaryOpNode::new(BinaryOp::Gt, StrRange::from_input(&s))
        }),
        map(tag("=="), |s| {
            BinaryOpNode::new(BinaryOp::Eq, StrRange::from_input(&s))
        }),
        map(tag("!="), |s| {
            BinaryOpNode::new(BinaryOp::Neq, StrRange::from_input(&s))
        }),
        map(atom_val("and"), |s| {
            BinaryOpNode::new(BinaryOp::BoolAnd, StrRange::from_input(&s))
        }),
        map(atom_val("or"), |s| {
            BinaryOpNode::new(BinaryOp::BoolOr, StrRange::from_input(&s))
        }),
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::super::input::Position;
    use super::*;
    use std::convert::TryInto;

    fn test_op(s: &str, op: BinaryOp, ch1: u32, ch2: u32) {
        let test_input = Input::new_with_str(s);
        let input_len: u32 = test_input.len().try_into().unwrap();
        assert_eq!(
            binary_op(test_input),
            Ok((
                Input::new("", Position::new(0, input_len), Position::max()),
                BinaryOpNode::new(
                    op,
                    StrRange::new(Position::new(0, ch1), Position::new(0, ch2))
                )
            ))
        );
    }

    #[test]
    fn binary_op_test() {
        test_op(" +", BinaryOp::Plus, 1, 2);
        test_op(" -", BinaryOp::Minus, 1, 2);
        test_op(" *", BinaryOp::Mul, 1, 2);
        test_op(" /", BinaryOp::Div, 1, 2);
        test_op(" %", BinaryOp::Mod, 1, 2);
        test_op(" >", BinaryOp::Gt, 1, 2);
        test_op(" >=", BinaryOp::Geq, 1, 3);
        test_op(" <", BinaryOp::Lt, 1, 2);
        test_op(" <=", BinaryOp::Leq, 1, 3);
        test_op(" ==", BinaryOp::Eq, 1, 3);
        test_op(" !=", BinaryOp::Neq, 1, 3);
        test_op(" and", BinaryOp::BoolAnd, 1, 4);
        test_op(" or", BinaryOp::BoolOr, 1, 3);
    }

    fn test_unary_op(s: &str, op: UnaryOp, ch1: u32, ch2: u32) {
        let test_input = Input::new_with_str(s);
        let input_len: u32 = test_input.len().try_into().unwrap();
        assert_eq!(
            unary_op(test_input),
            Ok((
                Input::new("", Position::new(0, input_len), Position::max()),
                UnaryOpNode::new(
                    op,
                    StrRange::new(Position::new(0, ch1), Position::new(0, ch2))
                )
            ))
        );
    }

    #[test]
    fn unary_op_test() {
        test_unary_op(" +", UnaryOp::Plus, 1, 2);
        test_unary_op(" -", UnaryOp::Minus, 1, 2);
        test_unary_op(" not", UnaryOp::BoolNot, 1, 4);
    }
}

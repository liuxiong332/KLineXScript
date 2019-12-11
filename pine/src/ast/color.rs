use super::error::{PineError, PineErrorKind, PineResult};
use super::input::{Input, Position, StrRange};
use super::utils::skip_ws;
use nom::{
    bytes::complete::{tag, take_while},
    combinator::recognize,
    sequence::tuple,
    Err,
};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ColorNode<'a> {
    pub value: &'a str,
    pub input: Input<'a>,
}

impl<'a> ColorNode<'a> {
    #[inline]
    pub fn new(value: &'a str, input: Input<'a>) -> ColorNode<'a> {
        ColorNode { value, input }
    }

    pub fn from_str(value: &'a str) -> ColorNode<'a> {
        ColorNode {
            value,
            input: Input::new_with_start(value, Position::new(0, 0)),
        }
    }
}

fn is_hex_digit(c: char) -> bool {
    c.is_digit(16)
}

pub fn color_lit(input: Input) -> PineResult<ColorNode> {
    let (input, _) = skip_ws(input)?;
    let (next_input, out) = recognize(tuple((tag("#"), take_while(is_hex_digit))))(input)?;

    match out.len() {
        7 | 9 => Ok((next_input, ColorNode::new(out.src, out))),
        _ => Err(Err::Error(PineError::from_pine_kind(
            input,
            PineErrorKind::InvalidColorLiteral,
        ))),
    }
}

#[cfg(test)]
mod tests {
    use super::super::input::Position;
    use super::*;

    #[test]
    fn color_lit_test() {
        assert_eq!(
            color_lit(Input::new_with_str(" #123456 d")),
            Ok((
                Input::new(" d", Position::new(0, 8), Position::max()),
                ColorNode::new(
                    "#123456",
                    Input::new_with_start("#123456", Position::new(0, 1))
                )
            ))
        );
    }
}

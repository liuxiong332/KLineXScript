use super::error::{PineError, PineErrorKind, PineResult};
use super::utils::skip_ws;
use nom::{
    bytes::complete::{tag, take_while},
    combinator::recognize,
    sequence::tuple,
    Err,
};

fn is_hex_digit(c: char) -> bool {
    c.is_digit(16)
}

pub fn color_lit(input: &str) -> PineResult {
    let (input, _) = skip_ws(input)?;
    let (next_input, out) = recognize(tuple((tag("#"), take_while(is_hex_digit))))(input)?;
    match out.len() {
        7 | 9 => Ok((next_input, out)),
        _ => Err(Err::Error(PineError::from_pine_kind(
            input,
            PineErrorKind::InvalidColorLiteral,
        ))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn color_lit_test() {
        assert_eq!(color_lit(" #123456 d"), Ok((" d", "#123456")));
    }
}

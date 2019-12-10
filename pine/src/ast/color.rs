use super::error::{PineError, PineErrorKind, PineResult};
use super::input::Input;
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

pub fn color_lit(input: Input) -> PineResult<&str> {
    let (input, _) = skip_ws(input)?;
    println!("Now input {:?}", input);
    let (next_input, out) = recognize(tuple((tag("#"), take_while(is_hex_digit))))(input)?;
    println!("next input {:?}", next_input);

    match out.len() {
        7 | 9 => Ok((next_input, out.src)),
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
                "#123456"
            ))
        );
    }
}

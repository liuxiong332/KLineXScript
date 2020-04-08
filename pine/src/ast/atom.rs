use super::error::{PineError, PineErrorKind, PineResult};
use super::input::{Input, Position, StrRange};
use super::name::{alpha_or_underscore, is_alphanum_or_underscore};
use super::state::{AstState, PineInputError};
use super::utils::skip_ws;
use nom::{bytes::complete::take_while, combinator::recognize, sequence::pair};

pub fn atom_lit<'a>(input: Input<'a>, _state: &AstState) -> PineResult<'a> {
    let (input, _) = skip_ws(input)?;
    recognize(pair(
        alpha_or_underscore,
        take_while(is_alphanum_or_underscore),
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::super::input::Position;
    use super::*;

    #[test]
    fn atom_lit_test() {
        assert_eq!(
            atom_lit(Input::new_with_str(" a23456 d"), &AstState::new()),
            Ok((
                Input::new(" d", Position::new(0, 7), Position::max()),
                Input::new("a23456", Position::new(0, 1), Position::new(0, 7))
            ))
        );
    }
}

use super::comment::comment;
use super::error::{PineError, PineErrorKind, PineResult};
use super::input::Input;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    combinator::recognize,
    multi::{count, many0},
    sequence::{delimited, preceded, tuple},
    Err,
};

pub fn is_space(c: char) -> bool {
    match c {
        ' ' | '\t' => true,
        _ => false,
    }
}

pub fn is_ws(c: char) -> bool {
    match c {
        ' ' | '\t' | '\n' | '\r' => true,
        _ => false,
    }
}

pub fn skip_ws(input: Input) -> PineResult {
    let ws_or_comment = alt((take_while1(is_ws), comment));
    recognize(many0(ws_or_comment))(input)
}

pub fn skip_space(input: Input) -> PineResult {
    let space = take_while(is_space);
    recognize(space)(input)
}

pub fn eat_sep<'a, O, F>(fun: F) -> impl Fn(Input<'a>) -> PineResult<'a, O>
where
    F: Fn(Input<'a>) -> PineResult<O>,
{
    move |input: Input<'a>| preceded(skip_ws, &fun)(input)
}

pub fn eat_space<'a, O, F>(fun: F) -> impl Fn(Input<'a>) -> PineResult<'a, O>
where
    F: Fn(Input<'a>) -> PineResult<O>,
{
    move |input: Input<'a>| preceded(take_while(is_space), &fun)(input)
}

pub fn statement_indent<'a>(indent_count: usize) -> impl Fn(Input<'a>) -> PineResult {
    move |input: Input<'a>| recognize(count(alt((tag("    "), tag("\t"))), indent_count))(input)
}

pub fn input_end<'a>(input: Input<'a>) -> PineResult {
    if input.len() == 0 {
        Ok((input, Input::new_with_start("", input.start)))
    } else {
        Err(Err::Error(PineError::from_pine_kind(
            input,
            PineErrorKind::NotEndOfInput,
        )))
    }
}

pub fn statement_end<'a>(input: Input<'a>) -> PineResult {
    recognize(tuple((
        many0(alt((tag(" "), tag("\t")))),
        alt((tag("\n"), tag("\r\n"), comment, input_end)),
    )))(input)
}

pub fn eat_statement<'a, O1, F1, O, F>(start: F1, fun: F) -> impl Fn(Input<'a>) -> PineResult<O>
where
    F1: Fn(Input<'a>) -> PineResult<O1>,
    F: Fn(Input<'a>) -> PineResult<O>,
{
    move |input: Input<'a>| delimited(&start, &fun, statement_end)(input)
}

#[cfg(test)]
mod tests {
    use super::super::input::Position;
    use super::*;
    use nom::bytes::complete::tag;
    use nom::InputTake;

    #[test]
    fn separate_test() {
        let test_input = Input::new_with_str("   hello");
        assert_eq!(
            skip_ws(test_input.clone()),
            Ok(test_input.take_split("   ".chars().count()))
        );

        let test_in = Input::new_with_str("   \n\t  \r //ssd\nhello");
        assert_eq!(
            skip_ws(test_in.clone()),
            Ok(test_in.take_split(test_in.len() - "hello".chars().count()))
        );

        let test_in = Input::new_with_str("  hello");
        assert_eq!(
            eat_sep(tag("hello"))(test_in),
            Ok((
                Input::new("", Position::new(0, 7), Position::max()),
                Input::new("hello", Position::new(0, 2), Position::new(0, 7))
            ))
        );
        assert_eq!(
            eat_space(tag("hello"))(test_in),
            Ok((
                Input::new("", Position::new(0, 7), Position::max()),
                Input::new("hello", Position::new(0, 2), Position::new(0, 7))
            ))
        );
    }

    #[test]
    fn eat_statement_test() {
        let test_input = Input::new_with_str("    hello");
        assert_eq!(
            statement_indent(1)(test_input),
            Ok(test_input.take_split("    ".chars().count()))
        );

        let test_input = Input::new_with_str("  // hello \n");
        assert_eq!(
            statement_end(test_input),
            Ok((
                Input::new("", Position::new(1, 0), Position::max()),
                Input::new(test_input.src, Position::new(0, 0), Position::new(1, 0))
            ))
        );

        let test_input = Input::new_with_str("     \n");
        assert_eq!(
            statement_end(test_input),
            Ok((
                Input::new("", Position::new(1, 0), Position::max()),
                Input::new_u32(test_input.src, 0, 0, 1, 0)
            ))
        );

        let test_input = Input::new_with_str("hello world  \nhd");

        assert_eq!(
            eat_statement(tag("hello "), tag("world"))(test_input),
            Ok((
                Input::new("hd", Position::new(1, 0), Position::max()),
                Input::new_u32("world", 0, 6, 0, 11)
            ))
        );

        let test_input = Input::new_with_str("    world  \nhd");

        assert_eq!(
            eat_statement(statement_indent(1), tag("world"))(test_input),
            Ok((
                Input::new("hd", Position::new(1, 0), Position::max()),
                Input::new_u32("world", 0, 4, 0, 9)
            ))
        );

        let test_input = Input::new_with_str("    world ");
        assert_eq!(
            eat_statement(statement_indent(1), tag("world"))(test_input),
            Ok((
                Input::new("", Position::new(0, 10), Position::max()),
                Input::new_u32("world", 0, 4, 0, 9)
            ))
        );
    }

    // #[test]
    // fn eat_statement_exception_test() {
    //     let test_input = Input::new_with_str("    world ddd ");
    //     assert_eq!(
    //         eat_statement(statement_indent(1), tag("world"))(test_input),
    //         Ok((
    //             Input::new("", Position::new(0, 10), Position::max()),
    //             Input::new_u32("world", 0, 4, 0, 9)
    //         ))
    //     );
    // }
}

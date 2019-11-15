use crate::comment::comment;
use crate::error::PineResult;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    combinator::recognize,
    multi::many0,
    sequence::preceded,
};

pub fn is_ws(c: char) -> bool {
    match c {
        ' ' | '\t' | '\n' | '\r' => true,
        _ => false,
    }
}

pub fn skip_ws(input: &str) -> PineResult {
    let ws_or_comment = alt((take_while1(is_ws), comment));
    recognize(many0(ws_or_comment))(input)
}

pub fn eat_sep<'a, O, F>(fun: F) -> impl Fn(&'a str) -> PineResult<O>
where
    F: Fn(&'a str) -> PineResult<O>,
{
    move |input: &str| preceded(skip_ws, &fun)(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn separate_test() {
        assert_eq!(skip_ws("   hello"), Ok(("hello", "   ")));
        assert_eq!(
            skip_ws("   \n\t  \r //ssd\nhello"),
            Ok(("hello", "   \n\t  \r //ssd\n"))
        );

        assert_eq!(eat_sep(tag("hello"))("  hello"), Ok(("", "hello")));
    }
}

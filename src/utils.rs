use crate::comment::comment;
use crate::error::PineResult;
use nom::{branch::alt, bytes::complete::take_while1, combinator::recognize, multi::many0};

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
    }
}

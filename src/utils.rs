use crate::comment::comment;
use crate::error::PineResult;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    combinator::recognize,
    multi::{count, many0},
    sequence::{delimited, preceded, tuple},
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

pub fn statement_indent<'a>(indent_count: usize) -> impl Fn(&'a str) -> PineResult {
    move |input: &'a str| recognize(count(alt((tag("    "), tag("\t"))), indent_count))(input)
}

pub fn statement_end<'a>(input: &'a str) -> PineResult {
    recognize(tuple((
        many0(alt((tag(" "), tag("\t")))),
        alt((tag("\n"), tag("\r\n"), comment)),
    )))(input)
}

pub fn eat_statement<'a, O1, F1, O, F>(start: F1, fun: F) -> impl Fn(&'a str) -> PineResult<O>
where
    F1: Fn(&'a str) -> PineResult<O1>,
    F: Fn(&'a str) -> PineResult<O>,
{
    move |input: &str| delimited(&start, &fun, statement_end)(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::bytes::complete::tag;
    #[test]
    fn separate_test() {
        assert_eq!(skip_ws("   hello"), Ok(("hello", "   ")));
        assert_eq!(
            skip_ws("   \n\t  \r //ssd\nhello"),
            Ok(("hello", "   \n\t  \r //ssd\n"))
        );

        assert_eq!(eat_sep(tag("hello"))("  hello"), Ok(("", "hello")));
    }

    #[test]
    fn eat_statement_test() {
        assert_eq!(statement_indent(1)("    hello"), Ok(("hello", "    ")));

        assert_eq!(statement_end("  // hello \n"), Ok(("", "  // hello \n")));
        assert_eq!(statement_end("     \n"), Ok(("", "     \n")));
        assert_eq!(
            eat_statement(tag("hello "), tag("world"))("hello world  \nhd"),
            Ok(("hd", "world"))
        );

        assert_eq!(
            eat_statement(statement_indent(1), tag("world"))("    world  \nhd"),
            Ok(("hd", "world"))
        );
    }
}

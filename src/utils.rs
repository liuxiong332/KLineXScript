use crate::comment::comment;
use crate::error::{PineError, PineResult};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    combinator::{peek, recognize},
    multi::many0,
    sequence::preceded,
    IResult,
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

pub trait Len {
    fn len(&self) -> usize;
}

impl<T> Len for Vec<T> {
    fn len(&self) -> usize {
        Vec::len(self)
    }
}

// match optional `exp1` and optional `exp2` separated by `exp`
pub fn multi_opt_separated_pair<I, F1, S, F2, O1, O2>(
    exp1: F1,
    sep: S,
    exp2: F2,
) -> impl Fn(I) -> IResult<I, (O1, O2), PineError<I>>
where
    I: Clone + Copy,
    F1: Fn(I) -> IResult<I, O1, PineError<I>>,
    F2: Fn(I) -> IResult<I, O2, PineError<I>>,
    S: Fn(I) -> IResult<I, I, PineError<I>>,
    O1: Len + Default,
    O2: Len + Default,
{
    move |input: I| {
        let (input, res1) = exp1(input)?;
        if res1.len() > 0 {
            if peek(&sep)(input).is_ok() {
                let (input, res2) = preceded(&sep, &exp2)(input)?;
                Ok((input, (res1, res2)))
            } else {
                Ok((input, (res1, O2::default())))
            }
        } else {
            let (input, o2) = exp2(input)?;
            Ok((input, (O1::default(), o2)))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::{bytes::complete::tag, character::complete::alpha1, multi::separated_list};
    #[test]
    fn separate_test() {
        assert_eq!(skip_ws("   hello"), Ok(("hello", "   ")));
        assert_eq!(
            skip_ws("   \n\t  \r //ssd\nhello"),
            Ok(("hello", "   \n\t  \r //ssd\n"))
        );

        assert_eq!(eat_sep(tag("hello"))("  hello"), Ok(("", "hello")));
    }

    fn multi_opt_separated_pair_test() {
        let parser = multi_opt_separated_pair(
            separated_list(tag(","), alpha1),
            tag("|"),
            separated_list(tag("."), alpha1),
        );
        assert_eq!(
            parser("wo,dd|a.b.c"),
            Ok(("", (vec!["wo", "dd"], vec!["a", "b", "c"])))
        );
    }
}

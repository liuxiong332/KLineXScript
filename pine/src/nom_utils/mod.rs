use nom::error::ParseError;
use nom::internal::IResult;

/// Matches an object from the first parser and discards it,
/// then gets an object from the second parser.
///
/// # Arguments
/// * `first` The opening parser.
/// * `second` The second parser to get object.
/// ```rust
/// # use nom::{Err, error::ErrorKind, Needed};
/// # use nom::Needed::Size;
/// use nom::sequence::preceded;
/// use nom::bytes::complete::tag;
///
/// let parser = preceded(tag("abc"), tag("efg"));
///
/// assert_eq!(parser("abcefg"), Ok(("", "efg")));
/// assert_eq!(parser("abcefghij"), Ok(("hij", "efg")));
/// assert_eq!(parser(""), Err(Err::Error(("", ErrorKind::Tag))));
/// assert_eq!(parser("123"), Err(Err::Error(("123", ErrorKind::Tag))));
/// ```
pub fn preceded<I, S, O1, O2, E: ParseError<I>, F, G>(
    first: F,
    second: G,
) -> impl Fn(I, S) -> IResult<I, O2, E>
where
    F: Fn(I, S) -> IResult<I, O1, E>,
    G: Fn(I, S) -> IResult<I, O2, E>,
{
    move |input: I, state: S| {
        let (input, _) = first(input, state)?;
        second(input, state)
    }
}

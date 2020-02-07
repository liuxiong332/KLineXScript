use nom::{
    error::{ErrorKind, ParseError},
    Compare, CompareResult, Err, ExtendInto, FindSubstring, FindToken, IResult, InputIter,
    InputLength, InputTake, InputTakeAtPosition, Needed, Offset, Slice,
};
use std::convert::TryInto;
use std::ops;
use std::str::{CharIndices, Chars};
use std::u32;

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy, Serialize)]
pub struct Position {
    line: u32,
    character: u32,
}

impl Position {
    pub fn new(line: u32, character: u32) -> Position {
        Position { line, character }
    }

    pub fn translate(&self, line_delta: u32, character_delta: u32) -> Position {
        Position {
            line: self.line + line_delta,
            character: self.character + character_delta,
        }
    }

    pub fn max() -> Position {
        Position {
            line: u32::MAX,
            character: u32::MAX,
        }
    }

    pub fn get_line(&self) -> u32 {
        self.line
    }

    pub fn get_character(&self) -> u32 {
        self.character
    }
}

#[derive(Debug, PartialEq, Clone, Copy, Serialize)]
pub struct StrRange {
    pub start: Position,
    pub end: Position,
}

impl StrRange {
    pub fn new(start: Position, end: Position) -> StrRange {
        StrRange { start, end }
    }

    pub fn new_empty() -> StrRange {
        StrRange::new(Position::new(0, 0), Position::max())
    }

    pub fn from_input(input: &Input) -> StrRange {
        StrRange {
            start: input.start,
            end: input.end,
        }
    }

    pub fn from_start<'a>(src: &'a str, start: Position) -> StrRange {
        let (line_count, end_col) = Input::get_line_col(src);
        let end = if line_count == 1 {
            Position::new(start.line, end_col + start.character)
        } else {
            Position::new(start.line + line_count - 1, end_col)
        };
        StrRange::new(start, end)
    }

    pub fn contain(&self, pos: Position) -> bool {
        self.start <= pos && pos <= self.end
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Input<'a> {
    pub src: &'a str,
    pub start: Position,
    pub end: Position,
}

impl<'a> Input<'a> {
    pub fn new(src: &'a str, start: Position, end: Position) -> Input<'a> {
        Input { src, start, end }
    }

    pub fn new_u32(
        src: &'a str,
        start_line: u32,
        start_ch: u32,
        end_line: u32,
        end_ch: u32,
    ) -> Input<'a> {
        Input {
            src,
            start: Position::new(start_line, start_ch),
            end: Position::new(end_line, end_ch),
        }
    }

    pub fn new_with_str(src: &'a str) -> Input<'a> {
        Input {
            src,
            start: Position::new(0, 0),
            end: Position::new(u32::MAX, u32::MAX),
        }
    }

    pub fn new_with_start(src: &'a str, start: Position) -> Input<'a> {
        let (line_count, end_col) = Self::get_line_col(src);
        let end = if line_count == 1 {
            Position::new(start.line, end_col + start.character)
        } else {
            Position::new(start.line + line_count - 1, end_col)
        };
        Input::new(src, start, end)
    }

    pub fn new_empty() -> Input<'a> {
        Input::new("", Position::new(0, 0), Position::max())
    }

    pub fn len(&self) -> usize {
        self.src.len()
    }

    pub fn forward(&self, delta: usize) -> Input<'a> {
        let delta_str = &self.src[..delta];
        let start_input = Self::new_with_start(delta_str, self.start);
        Input::new(&self.src[delta..], start_input.end, self.end)
    }
}

impl<'a> Input<'a> {
    fn get_line_col(dest_str: &'a str) -> (u32, u32) {
        let mut line_count = 0u32;
        let mut last_line = "";
        for line in dest_str.split('\n') {
            line_count += 1;
            last_line = line;
        }
        let end_col: u32 = last_line.chars().count().try_into().unwrap();
        (line_count, end_col)
    }

    pub fn slice_start(&self, count: usize) -> Result<Self, Err<ErrorKind>> {
        let dest_str = self.src.get(..count);
        if dest_str.is_none() {
            return Err(Err::Incomplete(Needed::Unknown));
        }
        let dest_str = dest_str.unwrap();
        let start_input = Self::new_with_start(dest_str, self.start);
        Ok(start_input)
    }

    pub fn slice_split(&self, count: usize) -> Result<(Self, Self), Err<ErrorKind>> {
        let input = self.slice_start(count)?;
        let second_str = self.src.get(count..);
        if let Some(second_str) = second_str {
            Ok((
                Input::new(second_str, input.end, self.end),
                Input::new(input.src, self.start, input.end),
            ))
        } else {
            Err(Err::Incomplete(Needed::Unknown))
        }
    }
}

const WHITESPACES: &str =
    "                                                                       $\n";

impl<'a> InputTake for Input<'a> {
    fn take(&self, count: usize) -> Self {
        let dest_str = self.src.get(..count).or(WHITESPACES.get(..count)).unwrap();
        Self::new_with_start(dest_str, self.start)
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let input = self.take(count);
        let second_str = self.src.get(count..).or(Some("")).unwrap();
        (
            Input::new(second_str, input.end, self.end),
            Input::new(input.src, self.start, input.end),
        )
    }
}

impl<'a> InputLength for Input<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        self.src.len()
    }
}

impl<'a> Offset for Input<'a> {
    fn offset(&self, second: &Self) -> usize {
        let fst = self.src.as_ptr();
        let snd = second.src.as_ptr();

        snd as usize - fst as usize
    }
}

// impl<'a> AsBytes for Input<'a> {
//     #[inline(always)]
//     fn as_bytes(&self) -> &[u8] {
//         <str as AsBytes>::as_bytes(self.src)
//     }
// }

impl<'a> InputIter for Input<'a> {
    type Item = char;
    type Iter = CharIndices<'a>;
    type IterElem = Chars<'a>;
    #[inline]
    fn iter_indices(&self) -> Self::Iter {
        self.src.char_indices()
    }
    #[inline]
    fn iter_elements(&self) -> Self::IterElem {
        self.src.chars()
    }
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.src.position(predicate)
    }
    #[inline]
    fn slice_index(&self, count: usize) -> Option<usize> {
        self.src.slice_index(count)
    }
}

impl<'a> InputTakeAtPosition for Input<'a> {
    type Item = char;
    fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.src.find(predicate) {
            Some(i) => match self.slice_split(i) {
                Ok(s) => Ok(s),
                Err(_) => Err(Err::Incomplete(Needed::Unknown)),
            },
            None => Err(Err::Incomplete(Needed::Size(1))),
        }
    }

    fn split_at_position1<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.src.find(predicate) {
            Some(0) => Err(Err::Error(E::from_error_kind(self.clone(), e))),
            Some(i) => match self.slice_split(i) {
                Ok(s) => Ok(s),
                Err(_) => Err(Err::Incomplete(Needed::Unknown)),
            },
            None => Err(Err::Incomplete(Needed::Size(1))),
        }
    }

    fn split_at_position_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.src.find(predicate) {
            Some(i) => match self.slice_split(i) {
                Ok(s) => Ok(s),
                Err(_) => Err(Err::Incomplete(Needed::Unknown)),
            },
            None => Ok(self.take_split(self.input_len())),
        }
    }

    fn split_at_position1_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.src.find(predicate) {
            Some(0) => Err(Err::Error(E::from_error_kind(self.clone(), e))),
            Some(i) => match self.slice_split(i) {
                Ok(s) => Ok(s),
                Err(_) => Err(Err::Incomplete(Needed::Unknown)),
            },
            None => {
                if self.src.len() == 0 {
                    Err(Err::Error(E::from_error_kind(self.clone(), e)))
                } else {
                    Ok(self.take_split(self.input_len()))
                }
            }
        }
    }
}

impl<'a, 'b> Compare<Input<'b>> for Input<'a> {
    fn compare(&self, t: Input<'b>) -> CompareResult {
        self.src.compare(t.src)
    }

    fn compare_no_case(&self, t: Input<'b>) -> CompareResult {
        self.src.compare_no_case(t.src)
    }
}

impl<'a, 'b> Compare<&'b str> for Input<'a> {
    fn compare(&self, t: &'b str) -> CompareResult {
        self.src.compare(t)
    }

    fn compare_no_case(&self, t: &'b str) -> CompareResult {
        self.src.compare_no_case(t)
    }
}

impl<'a> FindToken<char> for Input<'a> {
    fn find_token(&self, token: char) -> bool {
        self.src.find_token(token)
    }
}

impl<'a, 'b> FindSubstring<Input<'b>> for Input<'a> {
    //returns byte index
    fn find_substring(&self, substr: Input<'b>) -> Option<usize> {
        self.src.find(substr.src)
    }
}

impl<'a, 'b> FindSubstring<&'b str> for Input<'a> {
    //returns byte index
    fn find_substring(&self, substr: &'b str) -> Option<usize> {
        self.src.find(substr)
    }
}

impl<'a> ExtendInto for Input<'a> {
    type Item = char;
    type Extender = String;

    #[inline]
    fn new_builder(&self) -> String {
        String::new()
    }

    #[inline]
    fn extend_into(&self, acc: &mut String) {
        acc.push_str(self.src);
    }
}

impl<'a> Slice<ops::Range<usize>> for Input<'a> {
    fn slice(&self, range: ops::Range<usize>) -> Self {
        let pos = self.take(range.start);
        let dest_str = self
            .src
            .get(range.clone())
            .or(WHITESPACES.get(range))
            .unwrap();
        let (line, col) = Self::get_line_col(dest_str);
        let end = Position::new(pos.end.line + line - 1, col);
        Input::new(dest_str, pos.end, end)
    }
}

impl<'a> Slice<ops::RangeTo<usize>> for Input<'a> {
    fn slice(&self, range: ops::RangeTo<usize>) -> Self {
        self.take(range.end)
    }
}

impl<'a> Slice<ops::RangeFrom<usize>> for Input<'a> {
    fn slice(&self, range: ops::RangeFrom<usize>) -> Self {
        let pos = self.take(range.start);
        let dest_str = self
            .src
            .get(range.clone())
            .or(WHITESPACES.get(range))
            .unwrap();
        Input::new(dest_str, pos.end, self.end)
    }
}

impl<'a> Slice<ops::RangeFull> for Input<'a> {
    fn slice(&self, _range: ops::RangeFull) -> Self {
        self.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::error::VerboseError;
    use std::u32;

    #[test]
    fn test_input_take() {
        let s = "abcd123\n123456\n8900";
        let s1 = Input::new(s, Position::new(0, 0), Position::new(u32::MAX, u32::MAX));
        assert_eq!(
            s1.take(7),
            Input::new("abcd123", Position::new(0, 0), Position::new(0, 7))
        );
        assert_eq!(
            s1.take(9),
            Input::new("abcd123\n1", Position::new(0, 0), Position::new(1, 1))
        );
        assert_eq!(
            s1.take(15),
            Input::new(
                "abcd123\n123456\n",
                Position::new(0, 0),
                Position::new(2, 0)
            )
        );
        assert_eq!(
            s1.take_split(15),
            (
                Input::new(
                    "8900",
                    Position::new(2, 0),
                    Position::new(u32::MAX, u32::MAX)
                ),
                Input::new(
                    "abcd123\n123456\n",
                    Position::new(0, 0),
                    Position::new(2, 0)
                ),
            )
        );

        assert_eq!(
            s1.take_split(15),
            (
                Input::new(
                    "8900",
                    Position::new(2, 0),
                    Position::new(u32::MAX, u32::MAX)
                ),
                Input::new(
                    "abcd123\n123456\n",
                    Position::new(0, 0),
                    Position::new(2, 0)
                ),
            )
        );

        assert_eq!(
            s1.take_split(20),
            (
                Input::new("", Position::new(0, 20), Position::new(u32::MAX, u32::MAX)),
                Input::new(
                    " ".repeat(20).as_str(),
                    Position::new(0, 0),
                    Position::new(0, 20)
                ),
            )
        );
    }

    #[test]
    fn test_split_at_position() {
        let s = Input::new(
            "abcd123\n123456\n8900",
            Position::new(0, 0),
            Position::new(u32::MAX, u32::MAX),
        );

        assert_eq!(
            s.split_at_position(|s| s == 'd') as IResult<Input, Input, VerboseError<Input>>,
            Ok((
                Input::new(
                    "d123\n123456\n8900",
                    Position::new(0, 3),
                    Position::new(u32::MAX, u32::MAX)
                ),
                Input::new("abc", Position::new(0, 0), Position::new(0, 3))
            ))
        );
    }

    #[test]
    fn get_line_col_test() {
        assert_eq!(Input::get_line_col("hello\ndd\n"), (3, 0));
        assert_eq!(
            Input::new("hello\ndd\nm", Position::new(0, 2), Position::max()).slice_start(5),
            Ok(Input::new(
                "hello",
                Position::new(0, 2),
                Position::new(0, 7)
            ))
        );
        assert_eq!(
            Input::new("hello\ndd\nm", Position::new(0, 2), Position::max()).slice_start(7),
            Ok(Input::new(
                "hello\nd",
                Position::new(0, 2),
                Position::new(1, 1)
            ))
        );
        assert_eq!(
            Input::new("hello\ndd\nm", Position::new(0, 2), Position::max()).forward(5),
            Input::new("\ndd\nm", Position::new(0, 7), Position::max())
        );
        assert_eq!(
            Input::new("hello\ndd\nm", Position::new(0, 2), Position::max()).slice_split(5),
            Ok((
                Input::new("\ndd\nm", Position::new(0, 7), Position::max()),
                Input::new("hello", Position::new(0, 2), Position::new(0, 7))
            ))
        );
    }
}

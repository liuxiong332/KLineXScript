use nom::InputTake;
use std::convert::TryInto;

#[derive(Debug, PartialEq, Clone, Copy)]
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
}

#[derive(Debug, PartialEq, Clone)]
pub struct Range {
    start: Position,
    end: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Input<'a> {
    src: &'a str,
    start: Position,
    end: Position,
}

impl<'a> Input<'a> {
    fn new(src: &'a str, start: Position, end: Position) -> Input<'a> {
        Input { src, start, end }
    }
}

impl<'a> InputTake for Input<'a> {
    #[inline]
    fn take(&self, count: usize) -> Self {
        let dest_str = &self.src[..count];
        let mut line_count = 0u32;
        let mut last_line = "";
        for line in dest_str.split('\n') {
            line_count += 1;
            last_line = line;
        }
        let end_col: u32 = last_line.chars().count().try_into().unwrap();
        let end = Position::new(line_count + self.start.line - 1, end_col);
        Input::new(dest_str, self.start, end)
    }

    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        let input = self.take(count);
        (
            Input::new(&self.src[count..], input.end, self.end),
            Input::new(&self.src[..count], self.start, input.end),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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
    }
}

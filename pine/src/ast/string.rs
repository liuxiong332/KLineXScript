use super::error::{PineError, PineErrorKind, PineResult};
use super::input::{Input, StrRange};
use super::utils::skip_ws;
use nom::{
    branch::alt,
    bytes::complete::{escaped, is_not, tag, take_until},
    character::complete::one_of,
    sequence::delimited,
    sequence::preceded,
    Err,
};

const ESCAPE_CODE: &'static str = "\'\"\\\n0123456789abfnrtv";

#[derive(Clone, Debug, PartialEq)]
pub struct StringNode {
    pub value: String,
    pub range: StrRange,
}

impl StringNode {
    pub fn new(value: String, range: StrRange) -> StringNode {
        StringNode { value, range }
    }
}

enum ControlChar {
    Byte(u8),
}

// Given b, returns the corresponding escaped char for \<b>
fn control_char_lookup(b: u8) -> Option<ControlChar> {
    match b {
        b'\\' => Some(ControlChar::Byte(b'\\')),
        b'\'' => Some(ControlChar::Byte(b'\'')),
        b'\"' => Some(ControlChar::Byte(b'\"')),
        b'\n' => Some(ControlChar::Byte(b'\n')),
        b'0' | b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7' | b'8' | b'9' => {
            Some(ControlChar::Byte(b - '0' as u8))
        }
        b'a' => Some(ControlChar::Byte(0x07)),  // bell
        b'b' => Some(ControlChar::Byte(0x08)),  // backspace
        b'f' => Some(ControlChar::Byte(0x0c)),  // form feed
        b'n' => Some(ControlChar::Byte(b'\n')), // newline
        b'r' => Some(ControlChar::Byte(0x0d)),  // carriage return
        b't' => Some(ControlChar::Byte(0x09)),  // horizontal tab
        b'v' => Some(ControlChar::Byte(0x0b)),  // vertical tab
        _ => None,
    }
}

pub fn unescape(buf: &str) -> Result<String, PineErrorKind> {
    if buf.len() == 0 {
        return Ok(String::from(""));
    }

    match take_until("\\")(buf) {
        Ok((buf, matched)) => {
            let ctrl_char = buf.as_bytes()[1];
            match control_char_lookup(ctrl_char) {
                Some(ControlChar::Byte(c)) => Ok([
                    matched,
                    String::from_utf8_lossy(&[c]).as_ref(),
                    unescape(&buf[2..])?.as_ref(),
                ]
                .concat()),
                None => Err(PineErrorKind::InvalidCtrlInStrLiteral),
            }
        }
        Err(Err::Error(PineError { errors: _ })) => Ok(String::from(buf)),
        _ => Err(PineErrorKind::InvalidStrLiteral),
    }
}

fn gen_quote_str(quote_char: &'static str) -> impl Fn(Input) -> PineResult<StringNode> {
    move |input: Input| {
        let ignore_chars = ["\n\\", quote_char].join("");
        let (next_input, out) = delimited(
            tag(quote_char),
            escaped(is_not(&ignore_chars[..]), '\\', one_of(ESCAPE_CODE)),
            tag(quote_char),
        )(input)?;
        match unescape(out.src) {
            Ok(res_str) => Ok((
                next_input,
                StringNode::new(res_str, StrRange::from_input(&out)),
            )),
            Err(err_str) => Err(Err::Error(PineError::from_pine_kind(input, err_str))),
        }
    }
}

pub fn string_lit(input: Input) -> PineResult<StringNode> {
    alt((gen_quote_str("\""), gen_quote_str("'")))(input)
}

pub fn string_lit_ws(input: Input) -> PineResult<StringNode> {
    preceded(skip_ws, alt((gen_quote_str("\""), gen_quote_str("'"))))(input)
}

#[cfg(test)]
mod tests {
    use super::super::input::Position;
    use super::*;
    use std::convert::TryInto;

    #[test]
    fn unescape_test() {
        assert_eq!(unescape(r"\\hello"), Ok(String::from("\\hello")));
        assert_eq!(unescape(r"\\hello\t\'"), Ok(String::from("\\hello\t\'")));
        assert_eq!(
            unescape("\\\\hello\\t\\\""),
            Ok(String::from("\\hello\t\""))
        );
    }

    #[test]
    fn string_lit_test() {
        let test_input = Input::new_with_str(r"'hello \' world'ding");
        let input_len: u32 = test_input.len().try_into().unwrap();
        assert_eq!(
            string_lit(test_input),
            Ok((
                Input::new("ding", Position::new(0, input_len - 4), Position::max()),
                StringNode::new(
                    String::from("hello ' world"),
                    StrRange::new(Position::new(0, 1), Position::new(0, 15))
                )
            ))
        );

        let test_input = Input::new_with_str("\"hello \' world\"ding");
        let input_len: u32 = test_input.len().try_into().unwrap();
        assert_eq!(
            string_lit(test_input),
            Ok((
                Input::new("ding", Position::new(0, input_len - 4), Position::max()),
                StringNode::new(
                    String::from("hello ' world"),
                    StrRange::new(Position::new(0, 1), Position::new(0, 14))
                )
            ))
        );
    }
}

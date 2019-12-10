use super::error::{PineError, PineErrorKind, PineResult};
use super::input::Input;
use nom::{
    branch::alt,
    bytes::complete::{escaped, is_not, tag, take_until},
    character::complete::one_of,
    sequence::delimited,
    Err,
};

const ESCAPE_CODE: &'static str = "\'\"\\\n0123456789abfnrtuvxz";

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

pub fn unescape(buf: &str) -> Result<String, &'static str> {
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
                None => Err("Invalid  control character was accepted!"),
            }
        }
        Err(Err::Error(PineError { errors: _ })) => Ok(String::from(buf)),
        _ => Err("unescape unexpected Incomplete character"),
    }
}

fn gen_quote_str(quote_char: &'static str) -> impl Fn(Input) -> PineResult<String> {
    move |input: Input| {
        let ignore_chars = ["\n\\", quote_char].join("");
        let (next_input, out) = delimited(
            tag(quote_char),
            escaped(is_not(&ignore_chars[..]), '\\', one_of(ESCAPE_CODE)),
            tag(quote_char),
        )(input)?;
        match unescape(out.src) {
            Ok(res_str) => Ok((next_input, res_str)),
            Err(err_str) => Err(Err::Error(PineError::from_pine_kind(
                input,
                PineErrorKind::InvalidStrLiteral(err_str),
            ))),
        }
    }
}

pub fn string_lit(input: Input) -> PineResult<String> {
    alt((gen_quote_str("\""), gen_quote_str("'")))(input)
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
                String::from("hello ' world")
            ))
        );

        let test_input = Input::new_with_str("\"hello \' world\"ding");
        let input_len: u32 = test_input.len().try_into().unwrap();
        assert_eq!(
            string_lit(test_input),
            Ok((
                Input::new("ding", Position::new(0, input_len - 4), Position::max()),
                String::from("hello ' world")
            ))
        );
    }
}

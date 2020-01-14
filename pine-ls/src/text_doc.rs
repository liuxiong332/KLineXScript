use lsp_types::Url;
use lsp_types::*;
use pine::ast::input::{Position as StrPos, StrRange};
use pine::runtime::error_format::PineFormatError;
use pine::PineScript;

pub struct TextDoc {
    text: String,
    uri: Url,
    line_lens: Vec<usize>,
}

fn get_line_lens(text: &str) -> Vec<usize> {
    let mut line_lens = vec![];
    let mut start = 0;
    let mut iter_index = 0;
    for ch in text.chars() {
        if ch == '\n' {
            line_lens.push(iter_index - start + 1);
            start = iter_index + 1;
        }
        iter_index += ch.len_utf8();
    }
    if iter_index > start || (iter_index == start && line_lens.len() > 0) {
        line_lens.push(iter_index - start);
    }
    line_lens
}

impl TextDoc {
    pub fn new(text: String, uri: Url) -> TextDoc {
        let line_lens = get_line_lens(&text);
        TextDoc {
            text,
            uri,
            line_lens,
        }
    }

    pub fn reset(&mut self, text: String) {
        self.line_lens = get_line_lens(&text);
        self.text = text;
    }

    pub fn change(&mut self, start: Position, end: Position, length: usize, text: String) {
        let mut count: usize = 0;
        for i in 0..start.line {
            count += self.line_lens[i as usize];
        }
        count += start.character as usize;
        self.text.replace_range(count..(count + length), &text);

        // split the new text to lines
        let mut new_lines: Vec<_> = get_line_lens(&text);
        new_lines[0] += start.character as usize;
        if end.line < self.line_lens.len() as u64 {
            let last_len = self.line_lens[end.line as usize] - end.character as usize;
            *new_lines.last_mut().unwrap() += last_len;
            self.line_lens
                .splice(start.line as usize..=end.line as usize, new_lines);
        } else {
            self.line_lens.splice(start.line as usize.., new_lines);
        }
    }

    pub fn get_uri(&self) -> &Url {
        &self.uri
    }

    // pub fn transfer_range(&self, range: StrRange) -> StrRange {
    //     if range.end == StrPos::max() {
    //         StrRange::new(
    //             range.start,
    //             StrPos::new(self.line_lens.len() as u32 - 1, range.end.get_character()),
    //         )
    //     } else {
    //         range
    //     }
    // }

    pub fn parse_src(&self) -> Result<(), Vec<PineFormatError>> {
        let mut pine_script = PineScript::new(None);
        pine_script.parse_src(&self.text)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_line_lens_test() {
        assert_eq!(
            get_line_lens("hello\nworld\r\nddd你好"),
            vec!["hello\n".len(), "world\r\n".len(), "ddd你好".len()]
        );
        assert_eq!(
            get_line_lens("hello\nworld\r\n"),
            vec!["hello\n".len(), "world\r\n".len(), 0]
        );
        assert_eq!(get_line_lens(""), Vec::<usize>::new());
        assert_eq!(get_line_lens("he"), vec![2]);
    }

    #[test]
    fn text_doc_test() {
        let mut text_doc = TextDoc::new(
            String::from("hello\nworld\nwode\n"),
            Url::parse("https://example.net").unwrap(),
        );
        text_doc.change(
            Position::new(1, 1),
            Position::new(2, 1),
            6,
            String::from("woyao"),
        );
        assert_eq!(text_doc.text, String::from("hello\nwwoyaoode\n"));
        assert_eq!(
            text_doc.line_lens,
            vec!["hello\n".len(), "wwoyaoode\n".len(), 0]
        );

        text_doc.change(
            Position::new(0, 0),
            Position::new(0, 0),
            0,
            String::from("good-"),
        );
        assert_eq!(text_doc.text, String::from("good-hello\nwwoyaoode\n"));
        assert_eq!(
            text_doc.line_lens,
            vec!["good-hello\n".len(), "wwoyaoode\n".len(), 0]
        );

        text_doc.change(
            Position::new(0, 0),
            Position::new(1, 0),
            11,
            String::from("hello\r\nwoyao\n我的"),
        );
        assert_eq!(
            text_doc.text,
            String::from("hello\r\nwoyao\n我的wwoyaoode\n")
        );
        assert_eq!(
            text_doc.line_lens,
            vec![
                "hello\r\n".len(),
                "woyao\n".len(),
                "我的wwoyaoode\n".len(), // 11 + 13
                0,
            ]
        );

        text_doc.change(
            Position::new(3, 0),
            Position::new(3, 0),
            0,
            String::from("hello"),
        );
        assert_eq!(
            text_doc.text,
            String::from("hello\r\nwoyao\n我的wwoyaoode\nhello")
        );
        assert_eq!(
            text_doc.line_lens,
            vec![
                "hello\r\n".len(),
                "woyao\n".len(),
                "我的wwoyaoode\n".len(),
                "hello".len()
            ]
        );

        text_doc.change(
            Position::new(0, 5),
            Position::new(0, 5),
            0,
            String::from("\r\n"),
        );
        assert_eq!(
            text_doc.line_lens,
            vec![
                "hello\r\n".len(),
                "\r\n".len(),
                "woyao\n".len(),
                "我的wwoyaoode\n".len(),
                "hello".len()
            ]
        );
    }
}

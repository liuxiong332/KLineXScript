#[derive(Debug, PartialEq)]
pub enum VarType {
    Variable,
    Function,
}

#[derive(Debug, PartialEq)]
pub struct DocBase {
    pub var_type: VarType,
    pub name: &'static str,
    pub signatures: Vec<String>,
    pub description: &'static str,
    pub example: &'static str,
    pub returns: &'static str,
    pub arguments: &'static str,
    pub remarks: &'static str,
    pub links: &'static str,
}

#[cfg(test)]
mod tests {
    use super::*;
    use comrak::{markdown_to_html, ComrakOptions};

    #[test]
    fn doc_base_test() {
        let options = ComrakOptions {
            hardbreaks: true,
            smart: true,
            github_pre_lang: true,
            width: std::usize::MAX,
            default_info_string: Some("pine".into()),
            unsafe_: true,
            ext_strikethrough: true,
            ext_tagfilter: true,
            ext_table: true,
            ext_autolink: true,
            ext_tasklist: false,
            ext_superscript: false,
            ext_header_ids: None,
            ext_footnotes: false,
            ext_description_lists: false,
        };
        assert_eq!(
            markdown_to_html("Hello, **世界**!", &options),
            "<p>Hello, <strong>世界</strong>!</p>\n"
        );
    }
}

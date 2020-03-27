use super::DocBase;
use super::VarType;
use comrak::{markdown_to_html, ComrakOptions};

lazy_static! {
    static ref MD_OPTIONS: ComrakOptions = ComrakOptions {
        hardbreaks: false,
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
}

pub fn gen_var_doc(name: String, doc_base: DocBase, sigs: Vec<String>) -> String {
    let name = Some(format!("## {}", name));
    let desc = match doc_base.description {
        "" => None,
        _ => Some(String::from(doc_base.description)),
    };
    let sigs = Some(
        sigs.into_iter()
            .map(|s| format!("```pine\n{}\n```", s))
            .collect::<Vec<_>>()
            .join("\n"),
    );
    let example = match doc_base.example {
        "" => None,
        _ => Some(format!("#### EXAMPLE\n{}", doc_base.example)),
    };
    let arguments = match doc_base.arguments {
        "" => None,
        _ => Some(format!("#### ARGUMENTS\n{}", doc_base.arguments)),
    };
    let returns = match doc_base.returns {
        "" => None,
        _ => Some(format!("#### RETURNS\n{}", doc_base.returns)),
    };
    let remarks = match doc_base.remarks {
        "" => None,
        _ => Some(format!("#### REMARKS\n{}", doc_base.remarks)),
    };
    let links = match doc_base.links {
        "" => None,
        _ => Some(format!("#### SEE ALSO\n{}", doc_base.links)),
    };

    let eles = vec![
        name, desc, sigs, example, arguments, returns, remarks, links,
    ];
    let doc_str = eles
        .into_iter()
        .filter_map(|s| s)
        .collect::<Vec<_>>()
        .join("\n");
    markdown_to_html(&doc_str, &MD_OPTIONS)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn vardoc_gen_test() {
        let fn_doc = DocBase {
            var_type: VarType::Function,
            name: "plot",
            signatures: vec![],
            description: "Plots a series of data on the chart.",
            example: "```pine\nhello\n```",
            returns: "A plot object, that can be used in [fill](#fun_fill)",
            arguments: "arg",
            remarks: "",
            links: "[plotshape](#fun_plotshape)",
        };
        assert_eq!(
            gen_var_doc(
                String::from("hello"),
                fn_doc,
                vec![String::from("int"), String::from("float")],
            ),
            String::from("")
        );
    }
}

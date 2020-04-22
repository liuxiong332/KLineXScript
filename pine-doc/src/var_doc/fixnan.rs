use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
For a given series replaces NaN values with previous nearest non-NaN value.
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "fixnan",
        signatures: vec![],
        description: DESCRIPTION,
        example: "",
        returns: "Series without na gaps.",
        arguments: "",
        remarks: "",
        links: "[na](#fun-na) [nz](#fun-nz)",
    };
    vec![fn_doc]
}

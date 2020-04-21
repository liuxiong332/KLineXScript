use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Cumulative (total) sum of x. In other words it's a sum of all elements of x.
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "cum",
        signatures: vec![],
        description: DESCRIPTION,
        example: "",
        returns: "Total sum series.",
        arguments: "",
        remarks: "",
        links: "[sum](#fun-sum)",
    };
    vec![fn_doc]
}

use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Base 10 logarithm of any `x > 0` is the unique `y` such that `10^y = x`
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "log",
        signatures: vec![],
        description: DESCRIPTION,
        example: "",
        returns: "The base 10 logarithm of x.",
        arguments: "",
        remarks: "",
        links: "[log](#fun-log)",
    };
    vec![fn_doc]
}

use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Natural logarithm of any `x > 0` is the unique `y` such that `e^y = x`
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "log",
        signatures: vec![],
        description: DESCRIPTION,
        example: "",
        returns: "The natural logarithm of x.",
        arguments: "",
        remarks: "",
        links: "[log10](#fun-log10)",
    };
    vec![fn_doc]
}

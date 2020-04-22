use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Sign (signum) of x is zero if the x is zero, 1.0 if the x is greater than zero, -1.0 if the x is less than zero.
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "sign",
        signatures: vec![],
        description: DESCRIPTION,
        example: "",
        returns: "The sign of the argument.",
        arguments: "",
        remarks: "",
        links: "",
    };
    vec![fn_doc]
}

use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
The ceil function returns the smallest (closest to negative infinity) integer that is greater than or equal to the argument.
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "ceil",
        signatures: vec![],
        description: DESCRIPTION,
        example: "",
        returns: "The smallest integer greater than or equal to the given number.",
        arguments: "",
        remarks: "",
        links: "",
    };
    vec![fn_doc]
}

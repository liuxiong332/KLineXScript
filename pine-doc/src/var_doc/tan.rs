use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
The tan function returns the trigonometric tangent of an angle.
"#;

const ARGUMENT: &'static str = r#"
**x** Angle, in radians.
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "tan",
        signatures: vec![],
        description: DESCRIPTION,
        example: "",
        returns: "The trigonometric tangent of an angle.",
        arguments: ARGUMENT,
        remarks: "",
        links: "",
    };
    vec![fn_doc]
}

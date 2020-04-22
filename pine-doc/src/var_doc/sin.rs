use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
The sin function returns the trigonometric sine of an angle.
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "sin",
        signatures: vec![],
        description: DESCRIPTION,
        example: "",
        returns: "The trigonometric sine of an angle.",
        arguments: "**x** Angle, in radians.",
        remarks: "",
        links: "",
    };
    vec![fn_doc]
}

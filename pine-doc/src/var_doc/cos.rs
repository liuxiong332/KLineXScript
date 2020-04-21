use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
The cos function returns the trigonometric cosine of an angle.
"#;

const PINE_FN_ARGUMENTS: &'static str = "
**x** Angle, in radians.
";

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "cos",
        signatures: vec![],
        description: DESCRIPTION,
        example: "",
        returns: "The trigonometric cosine of an angle.",
        arguments: PINE_FN_ARGUMENTS,
        remarks: "",
        links: "",
    };
    vec![fn_doc]
}

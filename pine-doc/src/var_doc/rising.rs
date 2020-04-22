use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Test if the x series is now rising for y bars long.
"#;

const ARGUMENTS: &'static str = r#"
source (series(float)) Series of values to process.
length (int) Number of bars (length).
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "rising",
        signatures: vec![],
        description: DESCRIPTION,
        example: "",
        returns:
            "true if current x is greater than any previous x for y bars back, false otherwise.",
        arguments: ARGUMENTS,
        remarks: "",
        links: "[falling](#fun-falling)",
    };
    vec![fn_doc]
}

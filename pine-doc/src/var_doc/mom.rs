use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Momentum of x price and x price y bars ago. This is simply a difference `x - x[y]`.
"#;

const ARGUMENTS: &'static str = r#"
source (series(float)) Series of values to process.
length (int) Offset from the current bar to the previous bar.
"#;

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "mom",
        signatures: vec![],
        description: DESCRIPTION,
        example: "",
        returns: "Momentum of x price and x price y bars ago.",
        arguments: ARGUMENTS,
        remarks: "",
        links: "",
    };
    vec![fn_doc]
}

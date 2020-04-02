use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Calculates average of all given series (elementwise).
"#;

const PINE_FN_ARGUMENTS: &'static str = "
**length (integer)** Length (number of bars back).
";

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "avg",
        signatures: vec![],
        description: DESCRIPTION,
        example: "",
        returns: "Average.",
        arguments: PINE_FN_ARGUMENTS,
        remarks: "",
        links: "[sum](#fun-sum) [cum](#fun-cum) [sma](#fun-sma)",
    };
    vec![fn_doc]
}

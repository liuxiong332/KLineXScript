use crate::{DocBase, VarType};

const DESCRIPTION: &'static str = r#"
Function atr (average true range) returns the RMA of true range. True range is max(high - low, abs(high - close[1]), abs(low - close[1]))
"#;

const PINE_FN_ARGUMENTS: &'static str = "
**length (int)** Length (number of bars back).
";

pub fn gen_doc() -> Vec<DocBase> {
    let fn_doc = DocBase {
        var_type: VarType::Function,
        name: "atr",
        signatures: vec![],
        description: DESCRIPTION,
        example: "",
        returns: "Average true range.",
        arguments: PINE_FN_ARGUMENTS,
        remarks: "",
        links: "[tr](#fun-tr) [rma](#fun-rma)",
    };
    vec![fn_doc]
}
